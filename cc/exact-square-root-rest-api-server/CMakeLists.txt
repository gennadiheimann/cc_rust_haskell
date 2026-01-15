cmake_minimum_required(VERSION 3.16)
project(exact_square_root_rest_api_server LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(EXECUTABLE_NAME exact_square_root_rest_api_server)
set(LYBRARY_NAME exact_sqrt)


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

# Catch2
FetchContent_Declare(
    Catch2
    GIT_REPOSITORY https://github.com/catchorg/Catch2
    GIT_TAG v3.12.0
    GIT_SHALLOW TRUE
  )
FetchContent_MakeAvailable(Catch2)
list(APPEND CMAKE_MODULE_PATH ${catch2_SOURCE_DIR}/extras)


# add_executable(exact_square_root_rest_api_server 
#   src/main.cpp
#   src/exact_square_root.cpp
#   src/exact_square_root.h)




# Linke json-Bibliothek
# target_link_libraries(exact_square_root_rest_api_server PRIVATE nlohmann_json::nlohmann_json)


add_subdirectory(app)
add_subdirectory(src)
add_subdirectory(tests)

enable_testing()

# Threads & optional OpenSSL
# find_package(Threads REQUIRED)
# target_link_libraries(rest_cpp PRIVATE Threads::Threads)

# find_package(OpenSSL)
# if (OpenSSL_FOUND)
#  target_compile_definitions(rest_cpp PRIVATE CPPHTTPLIB_OPENSSL_SUPPORT)
#  target_link_libraries(rest_cpp PRIVATE OpenSSL::SSL OpenSSL::Crypto)
# endif()

