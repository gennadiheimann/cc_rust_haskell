# cc_rust_haskell

## Architecture

[Link to dot Architecture](https://dreampuf.github.io/GraphvizOnline/?engine=dot#digraph%20Arcitecture%20%7B%0A%20%20%20%20node%20%5Bshape%3Dbox%5D%3B%0A%20%20%20%20%22WebContent%20html%2Fjs%22%20-%3E%20%20%22WebServer%5Cn%20Apache2%5Cn%20Docker%20container%22%20%5Blabel%3D%22REST%3A8080%22%2C%20fontsize%3D8%2C%20labeldistance%3D2%5D%3B%0A%20%20%20%20%22WebServer%5Cn%20Apache2%5Cn%20Docker%20container%22%20-%3E%20%22C%2B%2B%20Rest%20Server%5Cn%20Docker%20container%22%20%20%5Blabel%3D%22REST%3A8081%22%2C%20fontsize%3D8%2C%20labeldistance%3D2%5D%3B%0A%20%20%20%20%22C%2B%2B%20Rest%20Server%5Cn%20Docker%20container%22%20-%3E%20%22logic%20c%2B%2B%22%20%0A%20%20%20%20%22WebServer%5Cn%20Apache2%5Cn%20Docker%20container%22%20-%3E%20%22Haskell%20Rest%20Server%5Cn%20Docker%20container%22%20%20%5Blabel%3D%22REST%3A8082%22%2C%20fontsize%3D8%2C%20labeldistance%3D2%5D%0A%20%20%20%20%22Haskell%20Rest%20Server%5Cn%20Docker%20container%22%20-%3E%20%22logic%20haskell%22%0A%20%20%20%20%22WebServer%5Cn%20Apache2%5Cn%20Docker%20container%22%20-%3E%20%22Rust%20REST%20Server%5Cn%20Docker%20container%22%20%5Blabel%3D%22REST%3A8083%22%2C%20fontsize%3D8%2C%20labeldistance%3D2%5D%20%0A%20%20%20%20%22Rust%20REST%20Server%5Cn%20Docker%20container%22%20-%3E%20%22logic%20rust%22%0A%7D)

```dot
digraph Arcitecture {
    node [shape=box];
    "WebContent html/js" ->  "WebServer\n Apache2\n Docker container" [label="REST:8080", fontsize=8, labeldistance=2];
    "WebServer\n Apache2\n Docker container" -> "C++ Rest Server\n Docker container"  [label="REST:8081", fontsize=8, labeldistance=2];
    "C++ Rest Server\n Docker container" -> "logic c++" 
    "WebServer\n Apache2\n Docker container" -> "Haskell Rest Server\n Docker container"  [label="REST:8082", fontsize=8, labeldistance=2]
    "Haskell Rest Server\n Docker container" -> "logic haskell"
    "WebServer\n Apache2\n Docker container" -> "Rust REST Server\n Docker container" [label="REST:8083", fontsize=8, labeldistance=2] 
    "Rust REST Server\n Docker container" -> "logic rust"
}
```

## Run project

```console
cd cc_rust_haskell
docker compose up
``
## CC

### run cmake

```console
cd build
cmake ..
cmake --build .
```

or

```console
cmake -B build -S .
cmake --build build
# run tests
cd build
ctest

```

### rest api

``` console
curl http://localhost:8081/exactSquareRoot/1000
```

### start docker rest api server  

```console
docker build -t example_restapi_json_server .
docker run -it -v $(pwd):/app example_restapi_json_server bash
docker run -it -p 8080:8080 -v $(pwd):/app example_restapi_json_server /app/start_rest_api_json_server.sh
```

## RUST

``` console
cargo new hello_world
cargo run
```

### start docker rest api server rust (exact square root)

```console
cd rust/exact-square-root-rest-api-server
docker build -t rust_rest_api_server_sqrt .
docker run -it -p 8083:8083 rust_rest_api_server_sqrt:latest
```

## HASKELL

### create project

```console
stack setup
stack templates
stack new hello_world
```

### haskell rest api

```console
curl http://localhost:8082/exactSquareRoot/
```

### start docker rest api server haskell (exact square root)

```console
# go to project
cd haskell/exact-square-root-rest-api-server
# create image
docker build -t image_haskell_exact_square_root_rest_api_server .
# for test run container in bash
docker run -it -v $(pwd):/app image_haskell_exact_square_root_rest_api_server:latest bash
# run container with publish port 8082 and run rest api server
# first time
docker run -it --name haskell_exact_square_root_rest_api_server -p 8082:8082 -v $(pwd):/app image_haskell_exact_square_root_rest_api_server /app/start.sh
# start container next time
docker start -i haskell_exact_square_root_rest_api_server
# stop container 
docker stop haskell_exact_square_root_rest_api_server
```

## multistage image

```console
docker build -t haskell_rest_api_server_sqrt .
docker run -it -p 8082:8082 haskell_rest_api_server_sqrt:latest /app/exact-square-root-rest-api-server-exe
```

## WEB CLIENT

### apache container

```console
docker build -t my-apache2 .
docker run -it --name my-running-app -p 8080:80 my-apache2
```
