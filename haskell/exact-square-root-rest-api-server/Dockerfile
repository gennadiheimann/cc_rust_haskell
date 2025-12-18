FROM ubuntu:24.04
RUN apt update \
  && apt install -y curl
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack setup 
# WORKDIR /app
# COPY . .
# RUN stack build
