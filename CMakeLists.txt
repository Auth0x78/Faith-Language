cmake_minimum_required(VERSION 3.16)

project(faith)

# -------------------------------------------------------------
# Global build settings
# -------------------------------------------------------------

# Use C++23 for all targets
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Build static libraries by default for all subprojects
set(BUILD_SHARED_LIBS OFF CACHE BOOL "Build static libraries by default" FORCE)

# Use static MSVC runtime (MultiThreaded / MultiThreadedDebug)
# This ensures no dependency on external MSVC runtime DLLs.
if(MSVC)
    set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>" CACHE STRING "Use static MSVC runtime" FORCE)
endif()

# -------------------------------------------------------------
# Project source files
# -------------------------------------------------------------

# Automatically include all .cpp files under src/
file(GLOB_RECURSE MY_SOURCES CONFIGURE_DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")

# Create main executable
add_executable("${CMAKE_PROJECT_NAME}")
target_sources("${CMAKE_PROJECT_NAME}" PRIVATE ${MY_SOURCES} )

# Set include directories
target_include_directories("${CMAKE_PROJECT_NAME}" PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/include/")

# Define path for resources
target_compile_definitions("${CMAKE_PROJECT_NAME}" 
    PUBLIC RESOURCES_PATH="${CMAKE_CURRENT_SOURCE_DIR}/resources/"
)

# -------------------------------------------------------------
# Platform-specific configuration
# -------------------------------------------------------------
if(MSVC)
    target_compile_definitions("${CMAKE_PROJECT_NAME}" PUBLIC _CRT_SECURE_NO_WARNINGS)
    
    # Uncomment below to remove console window in Windows GUI apps
    # set_target_properties("${CMAKE_PROJECT_NAME}" PROPERTIES LINK_FLAGS "/SUBSYSTEM:WINDOWS /ENTRY:mainCRTStartup")

elseif(APPLE)
    # macOS specific settings can go here
    message(STATUS "Configuring for macOS")

elseif(UNIX)
    # Linux specific settings can go here
    message(STATUS "Configuring for Linux/Unix")
endif()

# -------------------------------------------------------------
# Summary
# -------------------------------------------------------------
message(STATUS "Project '${CMAKE_PROJECT_NAME}' configured for ${CMAKE_SYSTEM_NAME}")
message(STATUS "Source directory: ${CMAKE_CURRENT_SOURCE_DIR}")
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")