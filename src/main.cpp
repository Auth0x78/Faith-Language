#include <expected>
#include <filesystem> // For std::filesystem::file_size
#include <fstream>
#include <string>
#include <string_view>

#include "Lexer.h"
#include "Logger.h"

// A clear error type for our function
enum class FileError {
  CannotOpenFile,
  CannotReadFile,
  FileTooLarge, // Good practice to have a size guard
};

// File buffer string, lifetime: Until Program ends
static std::string file_buffer;

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

int main() {

  std::string filepath;
  Logger::fmtLog(LogLevel::Info, "Faith Compiler - Debug File Reader\n "
                                 "Enter path to file to compile(.ft):");
  std::cin >> filepath;
  const std::filesystem::path osfilepath(filepath);

  auto expected_content = read_file_to_string(osfilepath);

  if (!expected_content) {
    // Failure! Handle the specific error.
    switch (expected_content.error()) {
    case FileError::CannotOpenFile:
      Logger::fmtLog(LogLevel::Error, "Error: Could not open the file.");
      break;
    case FileError::CannotReadFile:
      Logger::fmtLog(LogLevel::Error,
                     "Error: An issue occurred while reading the file.");
      break;
    case FileError::FileTooLarge:
      Logger::fmtLog(LogLevel::Error, "Error: File exceeds the 128MB limit.");
      break;
    }
    return 1;
  }

  // The string 'file_buffer' now owns the data.
  file_buffer = std::move(*expected_content);

  // Create a non-owning view of the data for processing.
  std::string_view content_view(file_buffer);

  Logger::fmtLog(LogLevel::Info, "Successfully read %lld bytes",
                 content_view.length());
  return 0;
}