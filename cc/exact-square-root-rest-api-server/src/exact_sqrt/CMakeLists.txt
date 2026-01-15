set(LIBRARY_SOURCES
  "exact_square_root.cpp")

set(LIBRARY_INCLUDES
  "./" 
  # ${CMAKE_BINARY_DIR}/configured_files/include
)

add_library(${LYBRARY_NAME} ${LIBRARY_SOURCES})

target_link_libraries(${LYBRARY_NAME} PUBLIC
  # nlohmann_json::nlohmann_json
  # fmt::fmt
  # spdlog::spdlog
  # cxxopts::cxxopts
)

target_include_directories(${LYBRARY_NAME} PUBLIC 
  ${LIBRARY_INCLUDES})