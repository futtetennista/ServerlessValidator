## -*- docker-image-name: "serverless-validator-builder" -*-
FROM haskell:8.0

# https://docs.docker.com/engine/userguide/eng-image/dockerfile_best-practices/#run
RUN apt-get update && apt-get install --yes \
    xz-utils \
    make

RUN stack --resolver lts-9.14 install \
    base \0;95;0c
    protolude \
    text \
    aeson \
    yaml \
    unordered-containers \
    case-insensitive \
    regex-compat
