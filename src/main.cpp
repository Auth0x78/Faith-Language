#include <string_view>
#include <vector>

#include "ASTPrinter.h"
#include "FaithParser.h"
#include "FileRead.h"
#include "Lexer.h"
#include "Logger.h"

// File buffer string, lifetime: Until Program ends
static std::string file_buffer;
static std::vector<Token> scannedTokens;

// Function forward define
int handleError(FileError);

int main() {

  std::string filepath;
  Logger::fmtLog(LogLevel::Info, "Faith Compiler - Debug File Reader\n "
                                 "Enter path to file to compile(.ft):");
  std::cin >> filepath;
  const std::filesystem::path osfilepath(filepath);

  auto expected_content = read_file_to_string(osfilepath);

  // Handle errors
  if (!expected_content)
    return handleError(expected_content.error());

  // The string 'file_buffer' now owns the data.
  file_buffer = std::move(*expected_content);

  // Create a non-owning view of the data for processing.
  std::string_view content_view(file_buffer);

  Logger::fmtLog(LogLevel::Info, "Successfully read {} bytes",
                 content_view.length());

  // Provide the file's content to Lexer for scanning
  Lexer lexer(content_view);
  scannedTokens = std::move(lexer.scanTokens());

  // Generate abstract syntax tree using the scanned tokens
  FaithParser parser(scannedTokens);
  std::unique_ptr<Faith::Program> astProgram = parser.parse();

  // Instantiate AST Printer and print the abstract syntax tree
  Faith::ASTPrinter printer;
  printer.print(astProgram.get());

  return 0;
}

// Function Definations
int handleError(FileError err) {
  // Failure, Handle the specific error & print error message.
  switch (err) {
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