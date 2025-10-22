#ifndef FILE_READ_H
#define FILE_READ_H

#include <expected>
#include <filesystem>
#include <fstream>
#include <string>

// A clear error type for our function
enum class FileError {
  CannotOpenFile,
  CannotReadFile,
  FileTooLarge, // Good practice to have a size guard
};

// Reads a file into a std::string if it's within the size limit.
static std::expected<std::string, FileError>
read_file_to_string(const std::filesystem::path &path,
                    std::uintmax_t max_size = 128LL * 1024 * 1024) {
  // 1. Check file size first to avoid allocating huge amounts of memory.
  std::error_code ec;
  auto file_size = std::filesystem::file_size(path, ec);
  if (ec) {
    return std::unexpected(FileError::CannotOpenFile);
  }
  if (file_size > max_size) {
    return std::unexpected(FileError::FileTooLarge);
  }

  // Open the file.
  std::ifstream file(path, std::ios::binary);
  if (!file.is_open()) {
    return std::unexpected(FileError::CannotOpenFile);
  }

  // Allocate memory and read the file in one go.
  std::string content(file_size, '\0');
  if (!file.read(content.data(), file_size)) {
    return std::unexpected(FileError::CannotReadFile);
  }

  return content;
}

#endif // DEBUG