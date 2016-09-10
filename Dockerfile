FROM ubuntu:16.04
RUN apt-get update 
RUN apt-get -y install eatmydata
RUN eatmydata apt-get -y install python-virtualenv gcc python-dev libxml2-dev libxslt1-dev ffmpeg

ADD requirements.txt podshrinker.py /srv/
ADD templates /srv/templates
RUN eatmydata virtualenv /srv/.venv
RUN eatmydata /srv/.venv/bin/pip install -r /srv/requirements.txt
WORKDIR /srv
CMD env PORT=5000 ./.venv/bin/python podshrinker.py
