cmake_minimum_required(VERSION 3.10)
project(Sparkle 
  VERSION 0.1.0
  DESCRIPTION "The People's AI Infrastructure"
  LANGUAGES Fortran C)

# Set default build type
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

# Fortran compiler flags
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -Wextra")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -fcheck=all -fbacktrace")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -march=native")
endif()

# Find optional dependencies
find_package(OpenMP)
find_package(CUDA QUIET)
find_package(HIP QUIET)

# Core library sources
set(SPARKLE_SOURCES
  src/sporkle_types.f90
  src/sporkle_mesh_types.f90
  src/cpu_device.f90
  src/sporkle_discovery.f90
  src/sporkle_scheduler.f90
  src/sporkle_collectives.f90
)

# Optional GPU sources
if(CUDA_FOUND)
  list(APPEND SPARKLE_SOURCES src/cuda_discovery.f90)
  add_definitions(-DSPARKLE_CUDA_ENABLED)
endif()

if(HIP_FOUND)
  list(APPEND SPARKLE_SOURCES src/amd_discovery.f90)
  add_definitions(-DSPARKLE_HIP_ENABLED)
endif()

# Build the main library
add_library(sparkle ${SPARKLE_SOURCES})
target_include_directories(sparkle PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
  $<INSTALL_INTERFACE:include>
)

# Link optional libraries
if(OpenMP_Fortran_FOUND)
  target_link_libraries(sparkle PUBLIC OpenMP::OpenMP_Fortran)
endif()

target_link_libraries(sparkle PUBLIC dl)  # For dynamic loading

# Build examples
add_executable(test_basic test_basic.f90)
target_link_libraries(test_basic sparkle)

add_executable(test_mesh test_mesh.f90)
target_link_libraries(test_mesh sparkle)

add_executable(test_scheduler test_scheduler.f90)
target_link_libraries(test_scheduler sparkle)

add_executable(test_collectives test_collectives.f90)
target_link_libraries(test_collectives sparkle)

# Installation
install(TARGETS sparkle
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
)

install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/
  DESTINATION include
  FILES_MATCHING PATTERN "*.mod"
)

# Package configuration
include(CMakePackageConfigHelpers)
write_basic_package_version_file(
  "SparkleConfigVersion.cmake"
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY SameMajorVersion
)

install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/SparkleConfigVersion.cmake"
  DESTINATION lib/cmake/Sparkle
)