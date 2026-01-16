FROM ubuntu:24.04 AS cc_builder
RUN apt update \
&& apt install -y cmake build-essential git curl

WORKDIR /app
COPY ./app ./app
COPY ./src ./src
COPY ./tests ./tests
COPY ./CMakeLists.txt .

RUN cmake -B build && cmake --build build

FROM debian
WORKDIR /app
COPY --from=cc_builder /app/build/app/exact_square_root_rest_api_server .
EXPOSE 8081
ENTRYPOINT ["/app/exact_square_root_rest_api_server"]

