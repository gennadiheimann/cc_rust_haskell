add_executable(${EXECUTABLE_NAME} main.cpp)

# Include-Pfade setzen (httplib ist header-only)
target_include_directories(exact_square_root_rest_api_server PRIVATE
  ${httplib_SOURCE_DIR}
  # ${CMAKE_SOURCE_DIR}/src
)

target_link_libraries(
  ${EXECUTABLE_NAME} PUBLIC
  ${LYBRARY_NAME}
  nlohmann_json::nlohmann_json
  # fmt::fmt
  # spdlog::spdlog
  # cxxopts::cxxopts
)