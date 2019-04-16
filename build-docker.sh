#!/usr/bin/env bash
GIT_SHORT=$(git rev-parse --short HEAD)
GIT_COMMIT=${GIT_COMMIT:$GIT_SHORT}
docker build \
       --build-arg GIT_COMMIT="$GIT_COMMIT" \
       -t codeworld:latest -f Dockerfile .
