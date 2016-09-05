from flask import Flask, render_template, request, abort, redirect, url_for, make_response, Response
import hmac, pyblake2
import base64
from urlparse import urljoin
import feedparser
from feedgen.feed import FeedGenerator
from time import mktime
from datetime import datetime
import requests
import tempfile
import os, sys
import subprocess
import urllib
import hmac, pyblake2, base64
import shutil
import urlparse
import logging
import pickle

OPUS_TYPE = 'audio/ogg; codecs=opus'

log = logging.getLogger(__name__)
app = Flask(__name__)

@app.before_first_request
def setup_logging():
    if not app.debug:
        # In production mode, add log handler to sys.stderr.
        app.logger.addHandler(logging.StreamHandler())
        app.logger.setLevel(logging.DEBUG)

HMAC_KEY = os.environ['MAC_KEY']
STORE_DIR = '/tmp/pod-opus-store/'

@app.before_first_request
def setup_store():
  if not os.path.isdir(STORE_DIR):
    os.makedirs(STORE_DIR)

@app.route('/')
def index():
    try:
      uri = request.args['uri'].encode('utf8')
    except KeyError:
      encoded = None
    else:
      mac = hmac.new(HMAC_KEY, uri.encode('utf8'), digestmod=pyblake2.blake2s).digest()
      encoded = urljoin(request.url, url_for('feed', uri=base64.urlsafe_b64encode(uri),
	  verif=base64.urlsafe_b64encode(mac)))

    print encoded
    return render_template("root.html",
	encode_rss_action=url_for('index'),
	encoded=encoded
	)

@app.route('/feed/<uri>/<verif>')
def feed(uri, verif):
  uri = base64.urlsafe_b64decode(uri.encode('utf8'))
  verif = base64.urlsafe_b64decode(verif.encode('utf8'))
  mac = hmac.new(HMAC_KEY, uri.encode('utf8'), digestmod=pyblake2.blake2s).digest()
  if not hmac.compare_digest(verif, mac):
    abort(403)

  cachefile = os.path.join(STORE_DIR, urllib.quote_plus(uri)) + ".pickle"
  modified = etag = None
  cached = None
  if os.path.isfile(cachefile):
    try:
      with file(cachefile) as f:
	cached = pickle.load(f)
	app.logger.debug("Loaded cache from cachefile:%r", cachefile)
	etag = cached.etag if 'etag' in cached else None
	modified = cached.modified if 'modified' in cached else None
    except Exception, e:
      app.logger.warn("Could not load cache:%r", e)

  app.logger.debug("Parse feed: %r; etag:%r; modified:%r", uri, etag, modified)
  parsed = feedparser.parse(uri, etag=etag, modified=modified)
  app.logger.debug("Parsed feed: %r; %r", uri, parsed.status)
  if not parsed.entries:
    parsed = cached

  if 'etag' in parsed or 'modified' in parsed:
    with tempfile.NamedTemporaryFile(delete=False) as f:
      pickle.dump(parsed, f)
      f.flush()
      os.rename(f.name, cachefile)
      app.logger.debug("Saved cache to cachefile:%r", cachefile)

  feed = FeedGenerator()
  feed.id(uri)
  feed.title(parsed.feed.title)
  feed.link(href=parsed.feed.link)
  feed.description(parsed.feed.description or '?')
  if 'image' in parsed.feed and 'href' in parsed.feed.image:
    feed.image(parsed.feed.image.href)

  for e in parsed.entries:
      entry = feed.add_entry()
      entry.title(e.title)
      entry.id(e.id)
      entry.updated(e.updated)
      entry.published(e.published)
      entry.description(e.description)

      for l in (e.links if 'link' in e else []):
	  if l.rel == 'enclosure' and 'href' in l:
	      storename = transcoded_href(l.href)
	      entry.enclosure(urlparse.urljoin(request.url, storename), l.get('size', None),
		  l.get('type', OPUS_TYPE))
	  elif l.rel == 'alternate' and 'href' in l:
	      entry.link(**l)

      for c in (e.content if 'content' in e else []):
	  if 'type' in c and c.type.startswith('text/html'):
	      entry.content(content=c.value, type='html')
	  else:
	      entry.content(content=c.value, type='text')

  resp = make_response(feed.rss_str(pretty=True))
  resp.headers['content-type'] = 'application/xml'
  return resp

def file_reader(fname):
  with file(fname) as f:
    for chunk in stream(f):
      yield chunk

def stream(f):
  while True:
    data = f.read(4096)
    if not data:
      break
    yield data



@app.route('/audio/<uri>/<verif>.opus')
def audio(uri, verif):
  uri = base64.urlsafe_b64decode(uri.encode('utf8'))
  verif = base64.urlsafe_b64decode(verif.encode('utf8'))
  mac = hmac.new(HMAC_KEY, uri.encode('utf8'), digestmod=pyblake2.blake2s).digest()
  if not hmac.compare_digest(verif, mac):
    abort(403)

  gen = transcode_do(uri)
  return Response(gen, mimetype=OPUS_TYPE)


def transcoded_href(uri):
    verif = hmac.new(HMAC_KEY, uri.encode('utf8'), digestmod=pyblake2.blake2s).digest()
    return url_for('audio', uri=base64.urlsafe_b64encode(uri), verif=base64.urlsafe_b64encode(verif))

def transcode_do(uri):
    storebase = urllib.quote_plus(uri)
    storename = os.path.join(STORE_DIR, "%s.opus" % (storebase,))
    orig = os.path.join(STORE_DIR, storebase)

    if not os.path.isfile(orig):
        log.debug("Fetch: " + uri)
        blob = requests.get(uri, stream=True)
        with tempfile.NamedTemporaryFile(delete=False) as outf:
	  shutil.copyfileobj(blob.raw, outf)
	  os.rename(outf.name, orig)
    if not os.path.isfile(storename):
            with tempfile.NamedTemporaryFile(delete=False, suffix=".opus") as outf:
                cmd = ["ffmpeg",  "-i", orig,
		      "-stats",
                     "-acodec", "libopus", "-b:a", str(32*1024), "-compression_level", "10", "-f", "opus",
                     "-y", "/dev/stdout"]
                app.logger.debug("Running:%r", cmd)
                proc = subprocess.Popen(cmd, stdout=subprocess.PIPE)
		while True:
		    data = proc.stdout.read(1024)
		    if not data:
		      break
		    outf.write(data)
		    yield data
                assert proc.wait() == 0
                os.rename(outf.name, storename)
    else:
      for chunk in file_reader(storename):
	yield chunk
