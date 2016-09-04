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
import os
import subprocess
import urllib
import hmac, pyblake2, base64
import shutil
import urlparse

app = Flask(__name__)

HMAC_KEY = os.environ['MAC_KEY']
STORE_DIR = '/tmp/pod-opus-store/'


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

  parsed = feedparser.parse(uri)

  feed = FeedGenerator()
  feed.id(uri)
  feed.title(parsed.feed.title)
  feed.link(href=parsed.feed.link)
  feed.description(parsed.feed.description or '?')

  for e in parsed.entries:
      links = e.links
      entry = feed.add_entry()
      entry.title(e.title)
      entry.id(e.id)
      entry.updated(e.updated)
      entry.description(e.description)

      for l in links:
	  if l.rel == 'enclosure' and 'href' in l:
	      storename = transcoded_href(l.href)
	      entry.enclosure(urlparse.urljoin(request.url, storename), l.get('size', None), l.get('type', None))
	  elif l.rel == 'alternate':
	      entry.link(**l)
	  else:
	      print l
      
      for c in (e.content if 'content' in e else []):
	  if c.type.startswith('text/html'):
	      entry.content(content=c.value, type='html')
	  elif c.type.startswith('text/plain'):
	      entry.content(content=c.value, type='text')

  resp = make_response(feed.rss_str(pretty=True))
  resp.headers['content-type'] = 'application/xml'
  return resp 

def file_reader(fname):
  with file(fname) as f:
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

  fname = transcode_do(uri)
  return Response(file_reader(fname), mimetype='audio/ogg; codecs=opus')


def transcoded_href(uri):
    verif = hmac.new(HMAC_KEY, uri.encode('utf8'), digestmod=pyblake2.blake2s).digest()
    return url_for('audio', uri=base64.urlsafe_b64encode(uri), verif=base64.urlsafe_b64encode(verif))

def transcode_do(uri):
    storebase = urllib.quote_plus(uri)
    storename = os.path.join(STORE_DIR, "%s.opus" % (storebase,))
    if not os.path.isdir(STORE_DIR):
      os.makedirs(STORE_DIR) 
    if not os.path.isfile(storename):
        print "Fetch: " + uri
        blob = requests.get(uri, stream=True)
        with tempfile.NamedTemporaryFile(delete=False) as inf:
            with tempfile.NamedTemporaryFile(delete=False, suffix=".opus") as outf:
                shutil.copyfileobj(blob.raw, inf)
                cmd = ["ffmpeg",  "-i", inf.name,
                     "-acodec", "libopus", "-b:a", str(32*1024), "-compression_level", "10",
                    "-y", outf.name]
                print cmd
                ret = subprocess.call(cmd)
                assert ret == 0
                os.rename(outf.name, storename)
    return storename
