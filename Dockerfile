FROM ubuntu:16.04
RUN apt-get update && apt-get -y install python-virtualenv gcc python-dev libxml2-dev libxslt1-dev ffmpeg

ADD requirements.txt podshrinker.py /srv/
ADD templates /srv/templates
RUN virtualenv /srv/.venv
RUN /srv/.venv/bin/pip install -r /srv/requirements.txt
WORKDIR /srv
CMD env FLASK_APP=podshrinker.py .venv/bin/flask run --host=0.0.0.0
