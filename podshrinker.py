from flask import Flask, render_template, request, abort, redirect, url_for, make_response, Response
from werkzeug.middleware.proxy_fix import ProxyFix
import hmac, pyblake2
import base64
from urllib.parse import urljoin, urlparse, quote_plus
import feedparser
from feedgen.feed import FeedGenerator
from time import mktime
import time
from datetime import datetime
import pytz
import requests
import tempfile
import os, sys
import subprocess
import hmac, pyblake2, base64
import shutil
import logging
from logging.config import dictConfig
from concurrent import futures
import jsonpickle
import json
import re
import threading

OPUS_TYPE = 'audio/ogg; codecs=opus'

BLKSZ = 1 << 16

dictConfig({
    'version': 1,
    'formatters': {'default': {
        'format': '[%(asctime)s] %(levelname)s in %(module)s: %(message)s',
    }},
    'handlers': {},
    'root': {
        'level': 'DEBUG',
        'handlers': []
    }
})

app = Flask(__name__)

pool = futures.ThreadPoolExecutor(max_workers=4)

HMAC_KEY = os.environ['MAC_KEY'].encode('utf8')
FEED_DIR = os.environ.get('FEED_DIR', '/tmp/pod-feed-store/')
MEDIA_DIR = os.environ.get('MEDIA_DIR', '/tmp/pod-opus-store/')
PROXY_DEPTH = int(os.environ.get('PROXY_DEPTH', 0))

app.wsgi_app = ProxyFix(
    app.wsgi_app,
    x_for=PROXY_DEPTH,
    x_proto=PROXY_DEPTH,
    x_host=PROXY_DEPTH,
    x_port=PROXY_DEPTH,
)

@app.before_first_request
def setup_store():
  for d in (FEED_DIR, MEDIA_DIR):
    if not os.path.isdir(d):
      os.makedirs(d)

def verify_uri(uri):
  p = urlparse(uri)
  if p.scheme not in ('http', 'https'):
    app.logger.warn("Bad scheme (%r) passed in uri: %s", p.scheme, uri)
    abort(404)

@app.route('/')
def index():
    encoded = None
    if 'uri' in request.args:
      uri = request.args['uri']
      verify_uri(uri)

      uri = unitunes(uri)

      mac = hmac.new(HMAC_KEY, uri.encode('utf8'), digestmod=pyblake2.blake2s).digest()
      print(repr((uri, uri.encode('utf8'), mac)))
      rel_url = url_for('feed', uri=base64.urlsafe_b64encode(uri.encode('utf8')), verif=base64.urlsafe_b64encode(mac))
      encoded = urljoin(request.url, rel_url)
      return redirect(encoded, code=303)
    else:
      return render_template("root.html",
          encode_rss_action=url_for('index'),
        encoded=encoded
        )

@app.route('/feed/<uri>/<verif>')
def feed(uri, verif):
  uri = base64.urlsafe_b64decode(uri.encode('utf8'))
  verif = base64.urlsafe_b64decode(verif.encode('utf8'))
  mac = hmac.new(HMAC_KEY, uri, digestmod=pyblake2.blake2s).digest()
  if not hmac.compare_digest(verif, mac):
    abort(403)

  uri = uri.decode('utf8')
  verify_uri(uri)

  cachefile = pathfor(uri, '.picklejson', FEED_DIR)
  modified = etag = None
  cached = None
  if os.path.isfile(cachefile):
    try:
      with open(cachefile, 'rb') as f:
        cached = jsonpickle.decode(f.read())
        app.logger.debug("Loaded cache from cachefile:%r", cachefile)
        etag = cached.etag if 'etag' in cached else None
        modified = cached.modified if 'modified' in cached else None
    except Exception as e:
      app.logger.warn("Could not load cache:%r", e)

  app.logger.debug("Parse feed: %r; etag:%r; modified:%r", uri, etag, modified)
  parsed = feedparser.parse(uri, etag=etag, modified=modified)

  app.logger.debug("Parsed feed: %r; %r", uri, 'status' in parsed and parsed.status)
  if parsed.status < 200 or parsed.status >= 400:
    app.logger.warn("Non okay status code, 404?")
    abort(404)


  if cached and not parsed.entries:
    parsed = cached

  def save_to_cache():
    with tempfile.NamedTemporaryFile(delete=False, dir=FEED_DIR) as f:
      encoded = jsonpickle.encode(parsed)
      indented = json.dumps(json.loads(encoded), indent=4, sort_keys=True)
      f.write(indented)
      f.flush()
      os.rename(f.name, cachefile)
      os.chmod(cachefile, 0o644)
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
      entry = feed.add_entry(order='append')
      id = e.id if 'id' in e else None

      for l in (e.links if 'links' in e else []):
          if l.rel == 'enclosure' and 'href' in l:
              if not id:
                id = l.href
              storename = transcoded_href(l.href)
              entry.enclosure(urljoin(request.url, storename), l.get('size', None),
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
  except BaseException as e:
    raise e

def file_reader(fname):
  with open(fname, 'rb') as f:
    for chunk in stream(f):
      yield chunk

def stream(f):
  while True:
    data = f.read(BLKSZ)
    if not data:
      break
    yield data


@app.route('/audio-2/<verif>/<uri>/<fname>.opus')
def audio_2(verif, uri, fname):
  return audio(uri, verif)

@app.route('/audio/<uri>/<verif>.opus')
def audio(uri, verif):
  uri = base64.urlsafe_b64decode(uri.encode('utf8'))
  verif = base64.urlsafe_b64decode(verif.encode('utf8'))
  mac = hmac.new(HMAC_KEY, uri, digestmod=pyblake2.blake2s).digest()
  if not hmac.compare_digest(verif, mac):
    abort(403)

  uri = uri.decode('utf8')
  verify_uri(uri)

  gen = transcode_do(uri, ua=request.headers.get('user-agent', None))
  return Response(gen, mimetype=OPUS_TYPE)


def unitunes(uri):
  p = urlparse(uri)
  # ParseResult(scheme='https', netloc='itunes.apple.com', path='/us/podcast/the-mad-scientist-podcast/id1114969265', params='', query='mt=2', fragment='')
  app.logger.debug("Parse url: %r", p)
  if p.netloc not in ['itunes.apple.com', 'podcasts.apple.com']:
    app.logger.debug("Not an itunes URL: %r", uri)
    return uri

  m = re.search(r'/podcast/(?:[^/]*/)?id([0-9]*)$', p.path)

  if not m:
    app.logger.debug("Can't tell this is a podcast?")
    # This still makes no sense, but ...
    return uri

  pid = m.group(1)
  app.logger.debug("Podcast id: %r", pid)
  resp = requests.get('https://itunes.apple.com/lookup?id={}&entity=podcast'.format(pid))

  app.logger.debug("Response: %r", resp)
  json = resp.json()
  for result in json.get('results', []):
    app.logger.debug("Result: %r", result)
    feedUrl = result['feedUrl']
    app.logger.debug("Found feed URL for %r: %r", result['collectionName'], feedUrl)
    return feedUrl

  app.logger.debug("No result found? %r", json)
  return uri

def transcoded_href(uri):
    verif = hmac.new(HMAC_KEY, uri.encode('utf8'), digestmod=pyblake2.blake2s).digest()
    fname = os.path.basename(uri)
    return url_for('audio_2', uri=base64.urlsafe_b64encode(uri.encode('utf8')), verif=base64.urlsafe_b64encode(verif), fname=fname)

def pathfor(uri, suff, dir):
    maxlen = min(os.pathconf(dir, 'PC_PATH_MAX') - len(dir.encode('utf8')),
        os.pathconf(dir, 'PC_NAME_MAX'))

    storebase = \
        quote_plus(uri) + \
        ';' + \
        base64.urlsafe_b64encode(pyblake2.blake2s(uri.encode('utf8')).digest()).decode('ascii')

    storebase = storebase[:maxlen-len(suff.encode('utf8'))]
    return os.path.join(dir, storebase + suff)

def transcode_do(uri, ua=None):
    storename = pathfor(uri, '.opus', MEDIA_DIR)
    orig = pathfor(uri, '.orig', MEDIA_DIR)

    if not os.path.isfile(orig):
      app.logger.debug("Fetch: " + uri)
      blob = requests.get(uri, stream=True, headers={'user-agent': ua})
      app.logger.debug("Headers:%r", blob.headers)

      prev_stamp = 0

      with tempfile.NamedTemporaryFile(delete=False, dir=MEDIA_DIR) as outf:
        clen = float(blob.headers['content-length']) if 'content-length' in blob.headers else None
        sofar = 0
        while True:
          data = blob.raw.read(BLKSZ)
          if not data:
            break
          outf.write(data)
          sofar += len(data)
          now = time.time()
          if sofar == clen or (now - prev_stamp) > 1.0:
            prev_stamp = now
            if clen:
              app.logger.debug("Progress: %r/%r (%f%%)", sofar, clen, 100.0*sofar/clen)
            else:
              app.logger.debug("Progress: %r/?", sofar)
        yield ''

        #shutil.copyfileobj(blob.raw, outf)
        os.rename(outf.name, orig)
        os.chmod(orig, 0o644)
        app.logger.debug("Saved original to %r", orig)

    if not os.path.isfile(storename):
      with tempfile.NamedTemporaryFile(delete=False, suffix=".opus", dir=MEDIA_DIR) as outf:
        cmd = transcode_command(orig)
        app.logger.debug("Running:%r", cmd)
        proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        _err_logger = threading.Thread(target=spool_stderr, args=(proc.stderr,))
        _err_logger.start()
        try:
            while True:
                data = proc.stdout.read(BLKSZ)
                if not data:
                  break
                outf.write(data)
                yield data
            assert proc.wait() == 0
            os.rename(outf.name, storename)
            os.chmod(storename, 0o644)
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

def transcode_command(orig, bitrate=16):
  return ["ffmpeg",  "-i", orig,
    "-stats",
    "-af", "acompressor=threshold=-24dB:ratio=16:attack=25:release=1000:makeup=20dB",
    "-acodec", "libopus", "-b:a", str(bitrate*1024), "-compression_level", "10", "-f", "opus",
    "-y", "/dev/stdout"]

def spool_stderr(stderr):
  buf = b''
  app.logger.debug("Reading from stderr pipe…")
  while True:
    read = stderr.read1(BLKSZ)
    #app.logger.debug("Read %dbytes from stderr: %r", len(read), read)
    if not read:
      app.logger.debug("stderr EOF")
      break

    buf += read

    chunks = [chunk for line in buf.split(b'\n') for chunk in line.split(b'\r')]
    for line in chunks[:-1]:
      if line:
        app.logger.debug("stderr: %s", line.decode('utf8'))
    buf = chunks[-1]

if __name__ == '__main__':
  from waitress import serve
  import os
  port = int(os.environ.get('PORT', 5000))
  serve(app, port=port)
