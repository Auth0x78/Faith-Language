#include <Logger.h>
#include <format>
#include <print>

// Set default value of _level
LogLevel Logger::_level = LogLevel::Info;

// Constructor
Logger::Logger() {}

// ----------------------
// Simple Log (no formatting, no level)
// ----------------------
void Logger::Log(const std::string &message) {
  std::println("{}{}[] {}{}", GREEN_COLOR, "", RESET_COLOR, message);
}

// ----------------------
// Log with level (no formatting)
// ----------------------
void Logger::Log(LogLevel level, const std::string &message) {
  if (level < _level)
    return;

  switch (level) {
  case Info:
    std::println("{}[INFO]:{} {}", BLUE_COLOR, RESET_COLOR, message);
    break;
  case Warning:
    std::println("{}[WARN]:{} {}", YELLOW_COLOR, RESET_COLOR, message);
    break;
  case Error:
    std::println("{}[ERROR]:{} {}", RED_COLOR, RESET_COLOR, message);
    break;
  case None:
    std::println("{}", message);
    break;
  default:
    break;
  }
}

// ----------------------
// fmtLog (with formatting + level)
// ----------------------
void Logger::fmtLog(LogLevel level, const char *const fmt, ...) {
  if (level < _level)
    return;

  // Print prefix
  switch (level) {
  case Info:
    std::print("{}[INFO]:{} ", BLUE_COLOR, RESET_COLOR);
    break;
  case Warning:
    std::print("{}[WARN]:{} ", YELLOW_COLOR, RESET_COLOR);
    break;
  case Error:
    std::print("{}[ERROR]:{} ", RED_COLOR, RESET_COLOR);
    break;
  case None:
    break;
  }

  va_list args;
  va_start(args, fmt);

  // Use std::vprint_nonunicode to print formatted args
  std::vprint_nonunicode(std::cout, std::string_view(fmt),
                         std::make_format_args(args));

  va_end(args);

  std::print("\n");
}

// ----------------------
// fmtLog with default level = None
// ----------------------
void Logger::fmtLog(const char *fmt, ...) {
  if (!fmt) {
    std::println("(null)");
    return;
  }

  va_list args;
  va_start(args, fmt);

  std::vprint_nonunicode(std::cout, std::string_view(fmt),
                         std::make_format_args(args));

  va_end(args);

  std::print("\n");
}

// ----------------------
// Level control
// ----------------------
void Logger::SetLogLevel(LogLevel level) { _level = level; }
LogLevel Logger::GetLogLevel() { return _level; }

// ----------------------
// Private helpers (simple versions using <print>)
// ----------------------
void Logger::LogInfo(const std::string &message) {
  std::println("{}[INFO]:{} {}", BLUE_COLOR, RESET_COLOR, message);
}

void Logger::LogWarning(const std::string &message) {
  std::println("{}[WARN]:{} {}", YELLOW_COLOR, RESET_COLOR, message);
}

void Logger::LogError(const std::string &message) {
  std::println("{}[ERROR]:{} {}", RED_COLOR, RESET_COLOR, message);
}
