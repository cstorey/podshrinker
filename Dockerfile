FROM docker.io/debian:bookworm-slim
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get -y install eatmydata
RUN eatmydata apt-get update && \
    eatmydata apt-get -y install gcc python3-dev python3-poetry libxml2-dev libxslt1-dev ffmpeg
WORKDIR /srv
ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8
ENV YOUR_ENV=${YOUR_ENV} \
    PYTHONFAULTHANDLER=1 \
    PYTHONUNBUFFERED=1 \
    PYTHONHASHSEED=random \
    PIP_NO_CACHE_DIR=off \
    # Poetry's configuration:
    POETRY_NO_INTERACTION=1 \
    POETRY_VIRTUALENVS_CREATE=false \
    POETRY_CACHE_DIR='/var/cache/pypoetry' \
    POETRY_HOME='/usr/local'

ADD pyproject.toml poetry.lock /srv/
RUN eatmydata poetry install
ADD podshrinker.py /srv/
ADD templates /srv/templates
RUN find /srv -type f -exec sha256sum {} \;
RUN install -o nobody -g nogroup -d /var/lib/feed-store /var/lib/media-store
VOLUME ["/var/lib/feed-store", "/var/lib/media-store"]
USER nobody
CMD PORT=5000 python3 podshrinker.py
