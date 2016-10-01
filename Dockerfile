FROM ubuntu:16.04
RUN apt-get update && apt-get -y install eatmydata
RUN eatmydata apt-get update && eatmydata apt-get -y install python-virtualenv gcc python-dev libxml2-dev libxslt1-dev ffmpeg sudo
ADD requirements.txt podshrinker.py /srv/
ADD templates /srv/templates
RUN eatmydata virtualenv /srv/.venv
RUN eatmydata /srv/.venv/bin/pip install -r /srv/requirements.txt
WORKDIR /srv
VOLUME ["/var/lib/feed-store", "/var/lib/media-store"]
CMD chown -R nobody: /var/lib/feed-store /var/lib/media-store && sudo -E -u nobody env PORT=5000 ./.venv/bin/python podshrinker.py
