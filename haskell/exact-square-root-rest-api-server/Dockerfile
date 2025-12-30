FROM ubuntu:24.04 AS builder_haskell
RUN apt update \
  && apt install -y curl
RUN curl -sSL https://get.haskellstack.org/ | sh
WORKDIR /app
COPY . .
RUN stack install --local-bin-path ./build/bin

FROM debian
WORKDIR /app
COPY --from=builder_haskell /app/build/bin/* .
ENTRYPOINT ["/app/exact-square-root-rest-api-server-exe"]