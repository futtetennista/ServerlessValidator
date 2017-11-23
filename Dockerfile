## -*- docker-image-name: "serverless-validator" -*-
FROM haskell:8.2 as builder

WORKDIR "/home/"

RUN apt-get update
RUN apt-get install --yes xz-utils make

RUN stack --resolver lts-9.14 \
    --install-ghc \
    install base \
            protolude \
            text \
            aeson \
            yaml \
            unordered-containers \
            case-insensitive \
            regex-compat

COPY src/ServerlessValidator.hs ./serverless-validator.hs


FROM alpine:latest
RUN apk --no-cache add ca-certificates

WORKDIR /usr/local/bin

COPY --from=builder /home/serverless-validator.hs  .

ENTRYPOINT ["./serverless-validator.hs"]
