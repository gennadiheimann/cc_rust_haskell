cmake_minimum_required(VERSION 3.16)
project(example_rest_api_json_server LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(FetchContent)

# nlohmann/json
FetchContent_Declare(json
  GIT_REPOSITORY https://github.com/nlohmann/json.git
  GIT_TAG v3.12.0
)
FetchContent_MakeAvailable(json)

# cpp-httplib (Header-only)
FetchContent_Declare(httplib
  GIT_REPOSITORY https://github.com/yhirose/cpp-httplib.git
  GIT_TAG v0.28.0
)
FetchContent_MakeAvailable(httplib)

add_executable(example_rest_api_json_server src/main.cpp)

# Include-Pfade setzen (httplib ist header-only)
target_include_directories(example_rest_api_json_server PRIVATE
  ${httplib_SOURCE_DIR}
)

# Linke json-Bibliothek
target_link_libraries(example_rest_api_json_server PRIVATE nlohmann_json::nlohmann_json)

# Threads & optional OpenSSL
# find_package(Threads REQUIRED)
# target_link_libraries(rest_cpp PRIVATE Threads::Threads)

# find_package(OpenSSL)
# if (OpenSSL_FOUND)
#  target_compile_definitions(rest_cpp PRIVATE CPPHTTPLIB_OPENSSL_SUPPORT)
#  target_link_libraries(rest_cpp PRIVATE OpenSSL::SSL OpenSSL::Crypto)
# endif()

