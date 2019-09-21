FROM ubuntu:19.04
RUN apt-get update && apt-get -y install eatmydata
RUN eatmydata apt-get update && eatmydata apt-get -y install python-virtualenv gcc python3.7-dev python3-pip libxml2-dev libxslt1-dev ffmpeg sudo
RUN pip3 install pipenv
WORKDIR /srv
ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8
ENV PIPENV_VENV_IN_PROJECT=y
ADD Pipfile Pipfile.lock /srv/
RUN eatmydata pipenv install --deploy
ADD podshrinker.py /srv/
ADD templates /srv/templates
VOLUME ["/var/lib/feed-store", "/var/lib/media-store"]
# CMD chown -R nobody: /var/lib/feed-store /var/lib/media-store && sudo -E -u nobody env PORT=5000 ./.venv/bin/python podshrinker.py
CMD chown -R nobody: /var/lib/feed-store /var/lib/media-store && sudo -E -u nobody env PORT=5000 ./.venv/bin/python podshrinker.py
