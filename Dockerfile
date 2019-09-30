FROM debian:stretch
RUN useradd -ms /bin/bash codeworld \
  && apt-get update \
  && apt-get install -y sudo \
  && echo "codeworld ALL=(ALL:ALL) NOPASSWD:ALL" >> /etc/sudoers
WORKDIR /codeworld
RUN chown codeworld:codeworld .
USER codeworld
COPY --chown=codeworld . .
RUN ./install.sh
CMD ./run.sh
