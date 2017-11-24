## -*- docker-image-name: "serverless-validator" -*-
FROM futtetennista/serverless-validator-builder as builder

WORKDIR "/home/serverless-validator/"

COPY . .

RUN stack --resolver lts-9.14 install


FROM fpco/haskell-scratch:integer-gmp

COPY --from=builder /root/.local/bin/serverless-validator /bin/

ENTRYPOINT ["/bin/serverless-validator"]
