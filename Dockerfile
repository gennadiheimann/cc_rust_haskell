FROM ubuntu:24.04
RUN apt update \
&& apt install -y cmake build-essential
#RUN useradd -ms /bin/bash dev
#USER dev
