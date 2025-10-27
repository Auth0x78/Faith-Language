#include <string_view>
#include <vector>

#include "FaithParser.h"
#include "FileRead.h"
#include "Lexer.h"
#include "Logger.h"

// File buffer string, lifetime: Until Program ends
static std::string file_buffer;
static std::vector<Token> scannedTokens;

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

  // Provide the file's content to Lexer for scanning
  Lexer lexer(content_view);
  scannedTokens = std::move(lexer.scanTokens());

  // Generate Abstract Syntax Tree using the scanned tokens
  FaithParser parser(scannedTokens);
  std::unique_ptr<Faith::Program> astProgram = parser.parse();

  return 0;
}