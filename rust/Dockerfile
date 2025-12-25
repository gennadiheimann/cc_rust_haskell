FROM ubuntu:24.04
RUN apt update \
&& apt install -y cmake build-essential curl git vim
# install rust
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain stable
ENV PATH="/root/.cargo/bin:${PATH}"
