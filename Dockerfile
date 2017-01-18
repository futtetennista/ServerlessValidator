## -*- docker-image-name: "serverless-validator" -*-
FROM haskell:latest

RUN stack --resolver lts-7.10 \
    --install-ghc \
    install base protolude text aeson yaml unordered-containers case-insensitive regex-compat

COPY src/ServerlessValidator.hs /usr/local/bin/serverless-validator.hs

WORKDIR "/tmp"

ENTRYPOINT ["/usr/local/bin/serverless-validator.hs"]
