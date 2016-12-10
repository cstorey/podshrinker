from flask import Flask, render_template, request, abort, redirect, url_for, make_response, Response
import hmac, pyblake2
import base64
from urlparse import urljoin
import feedparser
from feedgen.feed import FeedGenerator
from time import mktime
from datetime import datetime
import pytz
import requests
import tempfile
import os, sys
import subprocess
import urllib
import hmac, pyblake2, base64
import shutil
import urlparse
import logging
from concurrent import futures
import jsonpickle

OPUS_TYPE = 'audio/ogg; codecs=opus'

log = logging.getLogger(__name__)
app = Flask(__name__)


pool = futures.ThreadPoolExecutor(max_workers=4)

@app.before_first_request
def setup_logging():
    if not app.debug:
        # In production mode, add log handler to sys.stderr.
        app.logger.addHandler(logging.StreamHandler())
        app.logger.setLevel(logging.DEBUG)

HMAC_KEY = os.environ['MAC_KEY']
FEED_DIR = os.environ.get('FEED_DIR', '/tmp/pod-feed-store/')
MEDIA_DIR = os.environ.get('MEDIA_DIR', '/tmp/pod-opus-store/')


@app.before_first_request
def setup_store():
  for d in (FEED_DIR, MEDIA_DIR):
    if not os.path.isdir(d):
      os.makedirs(d)

def verify_uri(uri):
  p = urlparse.urlparse(uri)
  if p.scheme not in ('http', 'https'):
    app.logger.warn("Bad scheme (%r) passed in uri: %s", p.scheme, uri)
    abort(404)

@app.route('/')
def index():
    try:
      uri = request.args['uri'].encode('utf8')
    except KeyError:
      encoded = None
    else:
      verify_uri(uri)
      mac = hmac.new(HMAC_KEY, uri.encode('utf8'), digestmod=pyblake2.blake2s).digest()
      encoded = urljoin(request.url, url_for('feed', uri=base64.urlsafe_b64encode(uri),
	  verif=base64.urlsafe_b64encode(mac)))

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

  verify_uri(uri)

  cachefile = pathfor(uri, '.picklejson', FEED_DIR)
  modified = etag = None
  cached = None
  if os.path.isfile(cachefile):
    try:
      with file(cachefile) as f:
	cached = jsonpickle.decode(f.read())
	app.logger.debug("Loaded cache from cachefile:%r", cachefile)
	etag = cached.etag if 'etag' in cached else None
	modified = cached.modified if 'modified' in cached else None
    except Exception, e:
      app.logger.warn("Could not load cache:%r", e)

  app.logger.debug("Parse feed: %r; etag:%r; modified:%r", uri, etag, modified)
  parsed = feedparser.parse(uri, etag=etag, modified=modified)
  app.logger.debug("Parsed feed: %r; %r", uri, 'status' in parsed and parsed.status)

  if cached and not parsed.entries:
    parsed = cached

  def save_to_cache():
    with tempfile.NamedTemporaryFile(delete=False, dir=FEED_DIR) as f:
      f.write(jsonpickle.encode(parsed))
      f.flush()
      os.rename(f.name, cachefile)
      os.chmod(cachefile, 0644)
      app.logger.debug("Saved cache to cachefile:%r", cachefile)

  pool.submit(save_to_cache)

  feed = FeedGenerator()
  feed.id(uri)
  feed.title(parsed.feed.get('title', None) or '???')
  feed.link(href=parsed.feed.get('link', None) or 'about:blank')
  feed.description(parsed.feed.get('description', None) or '???')
  if 'image' in parsed.feed and 'href' in parsed.feed.image:
    feed.image(parsed.feed.image.href)

  for e in parsed.entries:
    try:
      entry = feed.add_entry()
      id = e.id if 'id' in e else None

      for l in (e.links if 'links' in e else []):
	  if l.rel == 'enclosure' and 'href' in l:
	      if not id:
		id = l.href
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

      entry.id(id)
      entry.title(e.get('title', None) or '???')
      entry.description(e.get('description', None) or '???')
      if 'updated_parsed' in e and e.updated_parsed:
	  entry.updated(datetime.fromtimestamp(mktime(e.updated_parsed), pytz.UTC))
      if 'published_parsed' in e and e.published_parsed:
	  entry.published(datetime.fromtimestamp(mktime(e.published_parsed), pytz.UTC))
    finally:
      pass

  try:
    resp = make_response(feed.rss_str(pretty=True))
    resp.headers['content-type'] = 'application/xml'
    return resp
  except BaseException, e:
    raise e

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

  verify_uri(uri)

  gen = transcode_do(uri)
  return Response(gen, mimetype=OPUS_TYPE)


def transcoded_href(uri):
    verif = hmac.new(HMAC_KEY, uri.encode('utf8'), digestmod=pyblake2.blake2s).digest()
    return url_for('audio', uri=base64.urlsafe_b64encode(uri), verif=base64.urlsafe_b64encode(verif))

def pathfor(uri, suff, dir):
    maxlen = min(os.pathconf(dir, 'PC_PATH_MAX') - len(dir.encode('utf8')),
	os.pathconf(dir, 'PC_NAME_MAX'))

    storebase = "%s;%s" % (urllib.quote_plus(uri),
	base64.urlsafe_b64encode(pyblake2.blake2s(uri.encode('utf8')).digest()))

    storebase = storebase[:maxlen-len(suff.encode('utf8'))]
    return os.path.join(dir, "%s%s" % (storebase, suff))

def transcode_do(uri):
    storename = pathfor(uri, '.opus', MEDIA_DIR)
    orig = pathfor(uri, '.orig', MEDIA_DIR)

    if not os.path.isfile(orig):
      log.debug("Fetch: " + uri)
      blob = requests.get(uri, stream=True)

      with tempfile.NamedTemporaryFile(delete=False, dir=MEDIA_DIR) as outf:
	shutil.copyfileobj(blob.raw, outf)
	os.rename(outf.name, orig)
	os.chmod(orig, 0644)
	app.logger.debug("Saved original to %r", orig)

    if not os.path.isfile(storename):
      with tempfile.NamedTemporaryFile(delete=False, suffix=".opus", dir=MEDIA_DIR) as outf:
	cmd = transcode_command(orig)
	app.logger.debug("Running:%r", cmd)
	proc = subprocess.Popen(cmd, stdout=subprocess.PIPE)
	try:
	    while True:
		data = proc.stdout.read(1024)
		if not data:
		  break
		outf.write(data)
		yield data
	    assert proc.wait() == 0
	    os.rename(outf.name, storename)
	    os.chmod(storename, 0644)
	    app.logger.debug("Saved transcoded to %r", orig)
	finally:
	  app.logger.debug("Finishing... %r", proc.poll())
	  proc.stdout.close()
	  if proc.poll() is None:
	    app.logger.debug("TERM %r", proc.pid)
	    proc.terminate()
	  if proc.poll() is None:
	    app.logger.debug("KILL %r", proc.pid)
	    proc.kill()
	    proc.wait()
	  if proc.poll() is None:
	    app.logger.debug("Leaking child %r", proc.pid)

	  if os.path.isfile(outf.name):
	    os.unlink(outf.name)
    else:
      for chunk in file_reader(storename):
	yield chunk

def transcode_command(orig, bitrate=32):
  return ["ffmpeg",  "-i", orig,
    "-stats",
    "-acodec", "libopus", "-b:a", str(bitrate*1024), "-compression_level", "10", "-f", "opus",
    "-y", "/dev/stdout"]

if __name__ == '__main__':
  from waitress import serve
  import os
  port = int(os.environ.get('PORT', 5000))
  serve(app, port=port)
