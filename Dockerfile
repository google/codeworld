FROM ubuntu:18.04
MAINTAINER Brandon Barker <brandon.barker@cornell.edu>

ARG GIT_COMMIT

RUN apt update -y

RUN apt install -y --no-install-recommends \
    ca-certificates git sudo && \
  mkdir /work && \
  cd /work && \
  git clone https://github.com/google/codeworld.git && \
  cd codeworld && \
  git checkout $GIT_COMMIT && \
  ./install.sh

RUN echo "#!/usr/bin/env bash\n\$@\n" > /opt/entrypoint && \
  chmod a+x /opt/entrypoint

ENTRYPOINT ["/opt/entrypoint"]

