#ifndef Faith_lexer_h
#define Faith_lexer_h

#include <Logger.h>
#include <cctype>
#include <cstdint>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#define ERROR(line, msg)                                                       \
  {                                                                            \
    Logger::fmtLog(LogLevel::Error, msg, line);                                \
    std::exit(1);                                                              \
  }

enum class TokenType {
  // ===========================================================================
  // SINGLE-CHARACTER SYMBOLS
  // ===========================================================================
  LeftParen,      // (
  RightParen,     // )
  LeftCBracket,   // {
  RightCBracket,  // }
  LeftSqBracket,  // [
  RightSqBracket, // ]
  Comma,          // ,
  Dot,            // .
  Colon,          // :
  Semicolon,      // ;
  Question,       // ?

  Plus,    // +
  Minus,   // -
  Star,    // *
  Slash,   // /
  Percent, // %

  Ampersand, // &
  Pipe,      // |
  Caret,     // ^
  Bang,      // !
  Equal,     // =
  Less,      // <
  Greater,   // >

  // ===========================================================================
  // MULTI-CHARACTER SYMBOLS / OPERATORS
  // ===========================================================================
  PlusPlus,      // ++
  MinusMinus,    // --
  PlusEqual,     // +=
  MinusEqual,    // -=
  StarEqual,     // *=
  SlashEqual,    // /=
  PercentEqual,  // %=
  AndEqual,      // &=
  OrEqual,       // |=
  XorEqual,      // ^=
  QuestionEqual, // ?=

  EqualEqual,   // ==
  BangEqual,    // !=
  LessEqual,    // <=
  GreaterEqual, // >=

  LogicalAnd, // &&
  LogicalOr,  // ||

  ShiftLeft,  // <<
  ShiftRight, // >>

  Arrow,    // ->
  FatArrow, // =>
  //(used in function return type syntax)
  //(used in member access of struct)
  // ===========================================================================
  // LITERALS
  // ===========================================================================
  Identifier,    // [a-zA-Z_][a-zA-Z0-9_]*
  CharLiteral,   // ''
  StringLiteral, // "..."
  IntLiteral,    // 123
  FloatLiteral,  // 12.34
  HexLiteral,    // 0xFF
  BinaryLiteral, // 0b1010

  // ===========================================================================
  // KEYWORDS
  // ===========================================================================
  Kw_Func,      // func
  Kw_Let,       // let
  Kw_Const,     // const
  Kw_Return,    // return
  Kw_If,        // if
  Kw_Else,      // else
  Kw_For,       // for
  Kw_While,     // while
  Kw_Switch,    // switch
  Kw_Case,      // case
  Kw_Default,   // default
  Kw_Match,     // match
  Kw_Extern,    // extern
  Kw_Static,    // static
  Kw_Struct,    // struct
  Kw_typealias, // typealias
  Kw_Defer,     // defer
  Kw_Break,     // break
  Kw_Continue,  // continue

  // ===========================================================================
  // LITERALS / CONSTANTS
  // ===========================================================================
  Kw_True,  // true
  Kw_False, // false
  Kw_Null,  // null

  // ===========================================================================
  // BUILTIN TYPES
  // ===========================================================================
  Kw_U8,
  Kw_U16,
  Kw_U32,
  Kw_U64,
  Kw_I8,
  Kw_I16,
  Kw_I32,
  Kw_I64,
  Kw_F32,
  Kw_F64,
  Kw_Bool,
  Kw_Char,
  Kw_Void,

  // ===========================================================================
  // SPECIAL SYMBOLS
  // ===========================================================================

  // ===========================================================================
  // IGNORE TOKEN / ERROR / END
  // ===========================================================================
  Ignore,
  Error,
  EndOfFile
};

struct Token {
  TokenType type;
  std::string_view token;
  int64_t line;

  Token() = delete;
  Token(TokenType ttype, const char *msg, int64_t l)
      : type(ttype), token(msg), line(l) {}
  Token(TokenType ttype, std::string_view sv, int64_t l)
      : type(ttype), token(sv), line(l) {}
};

class Lexer {
public:
  Lexer(std::string_view src);
  std::vector<Token> &scanTokens();

private:
  Token scanToken();
  Token scanWord();
  Token scanNumber(char firstDigit);
  Token scanStringLiteral();
  Token scanCharLiteral();
  Token skipLineComment();
  Token skipMultilineComment();

  bool isAtEnd() const;
  char advance();
  char peek() const;

  char peekNext();
  bool match(char expected);

  Token makeToken(TokenType type);
  Token errorToken(const char *msg);

  std::vector<Token> m_tokens;
  std::string_view m_source;
  size_t m_start;
  size_t m_current;
  int64_t m_line;
};
#endif