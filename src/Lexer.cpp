#include "Lexer.h"
#define MIN_TOKENS_SIZE 128

static const Token ignoreToken(TokenType::Ignore, "#ignore", 0x0BAD);

using TT = TokenType;
static const std::unordered_map<std::string_view, TokenType> s_keywordTable = {
    {"func", TokenType::Kw_Func},
    {"let", TokenType::Kw_Let},
    {"const", TokenType::Kw_Const},
    {"return", TokenType::Kw_Return},
    {"if", TokenType::Kw_If},
    {"else", TokenType::Kw_Else},
    {"for", TokenType::Kw_For},
    {"while", TokenType::Kw_While},
    {"match", TokenType::Kw_Match},
    {"extern", TokenType::Kw_Extern},
    {"struct", TokenType::Kw_Struct},
    {"static", TokenType::Kw_Static},
    {"typealias", TokenType::Kw_Typealias},
    {"defer", TokenType::Kw_Defer},
    {"break", TokenType::Kw_Break},
    {"continue", TokenType::Kw_Continue},
    {"cast", TokenType::Kw_Cast},

    {"true", TokenType::Kw_True},
    {"false", TokenType::Kw_False},
    {"null", TokenType::Kw_Null},

    {"u8", TokenType::Kw_U8},
    {"i8", TokenType::Kw_I8},
    {"char", TokenType::Kw_Char},

    {"u16", TokenType::Kw_U16},
    {"i16", TokenType::Kw_I16},

    {"u32", TokenType::Kw_U32},
    {"i32", TokenType::Kw_I32},

    {"u64", TokenType::Kw_U64},
    {"i64", TokenType::Kw_I64},

    {"f32", TokenType::Kw_F32},
    {"f64", TokenType::Kw_F64},
    {"bool", TokenType::Kw_Bool},
    {"void", TokenType::Kw_Void},
};

static bool isHexDigit(char c) {
  if (std::isdigit(c))
    return true;

  c = std::tolower(c);
  switch (c) {
  case 'a':
  case 'b':
  case 'c':
  case 'd':
  case 'e':
  case 'f':
    return true;
  default:
    return false;
  }

  return false;
}

Lexer::Lexer(std::string_view src)
    : m_source(src), m_start(0), m_current(0), m_line(1) {
  m_tokens.reserve(MIN_TOKENS_SIZE);
}

std::vector<Token> &Lexer::scanTokens() {

  while (!isAtEnd()) {
    m_start = m_current;

    Token token = scanToken();
    if (token.type != TT::EndOfFile && token.type != TT::Ignore)
      m_tokens.push_back(token);
  }

  // Add EOF token add the end of parsing of file
  m_tokens.push_back(Token(TT::EndOfFile, "$", m_line));
  m_tokens.shrink_to_fit();
  return m_tokens;
}

Token Lexer::scanToken() {
  char c = advance();
  switch (c) {
  case '(':
    return makeToken(TT::LeftParen);
  case ')':
    return makeToken(TT::RightParen);
  case '{':
    return makeToken(TT::LeftBrace);
  case '}':
    return makeToken(TT::RightBrace);
  case '[':
    return makeToken(TT::LeftBracket);
  case ']':
    return makeToken(TT::RightBracket);
  case ',':
    return makeToken(TT::Comma);
  case '.':
    if (match('.')) {
      if (match('.'))
        return makeToken(TT::Ellipsis);

      return makeToken(TT::RangeDot);
    } else
      return makeToken(TT::Dot);
  case ':':
    return makeToken(TT::Colon);
  case ';':
    return makeToken(TT::Semicolon);
  case '?':
    return makeToken(TT::Question);
  case '+':
    if (match('+'))
      return makeToken(TT::PlusPlus);
    else if (match('='))
      return makeToken(TT::PlusEqual);

    return makeToken(TT::Plus);
  case '-':
    if (match('-'))
      return makeToken(TT::MinusMinus);
    else if (match('='))
      return makeToken(TT::MinusEqual);
    else if (match('>'))
      return makeToken(TT::Arrow);

    return makeToken(TT::Minus);
  case '*':
    if (match('='))
      return makeToken(TT::StarEqual);

    return makeToken(TT::Star);
  case '/':
    if (match('/'))
      return skipLineComment();
    else if (match('*'))
      return skipMultilineComment();
    else if (match('='))
      return makeToken(TT::SlashEqual);

    return makeToken(TT::Slash);
  case '%':
    if (match('='))
      return makeToken(TT::PercentEqual);

    return makeToken(TT::Percent);
  case '&':
    if (match('='))
      return makeToken(TT::AndEqual);
    else if (match('&'))
      return makeToken(TT::LogicalAnd);

    return makeToken(TT::Ampersand);
  case '|':
    if (match('='))
      return makeToken(TT::OrEqual);
    else if (match('|'))
      return makeToken(TT::LogicalOr);

    return makeToken(TT::Pipe);
  case '^':
    if (match('='))
      return makeToken(TT::XorEqual);

    return makeToken(TT::Caret);
  case '!':
    if (match('='))
      return makeToken(TT::BangEqual);

    return makeToken(TT::Bang);
  case '=':
    if (match('='))
      return makeToken(TT::EqualEqual);
    else if (match('>'))
      return makeToken(TT::FatArrow);

    return makeToken(TT::Equal);
  case '<':
    if (match('='))
      return makeToken(TT::LessEqual);
    else if (match('<'))
      return makeToken(TT::ShiftLeft);

    return makeToken(TT::Less);
  case '>':
    if (match('='))
      return makeToken(TT::GreaterEqual);
    else if (match('>'))
      return makeToken(TT::ShiftRight);

    return makeToken(TT::Greater);
  case '_':
    return makeToken(TT::Underscore);
  case '"':
    return scanStringLiteral();
  case '\'':
    return scanCharLiteral();
  case ' ':
  case '\r':
  case '\t':
    // ignore whitespace
    return ignoreToken;
  case '\n':
    m_line++;
    return ignoreToken;
    break;
  default:
    if (std::isalpha(c) || c == '_')
      return scanWord();
    else if (std::isdigit(c))
      return scanNumber(c);
    else
      return errorToken("Unexpected character found!");
  }

  return errorToken("Error[Lexer::scanToken] : Unreachable!");
}

Token Lexer::scanWord() {
  while (!isAtEnd() && (std::isalnum(peek()) || peek() == '_'))
    advance();
  std::string_view word(m_source.data() + m_start, m_current - m_start);

  // Check if it is a keyword or identifier
  auto it = s_keywordTable.find(word);
  if (it != s_keywordTable.end())
    return makeToken(it->second);
  return makeToken(TT::Identifier);
}

Token Lexer::scanNumber(char firstDigit) {
  bool isFloat = false;

  if (firstDigit == '0') {
    if (match('x') || match('X')) {
      // Hex literal
      while (!isAtEnd() && isHexDigit(peek()))
        advance();
      return makeToken(TT::HexLiteral);
    } else if (match('b') || match('B')) {
      while (!isAtEnd() && (peek() == '0' || peek() == '1'))
        advance();
      return makeToken(TT::BinaryLiteral);
    }
  }

  while (!isAtEnd() && std::isdigit(peek()))
    advance();
  if (!isAtEnd() && peek() == '.' && std::isdigit(peekNext())) {
    isFloat = true;
    advance();

    while (!isAtEnd() && std::isdigit(peek()))
      advance();
  }

  if (!isAtEnd() && (peek() == 'e' || peek() == 'E')) {
    isFloat = true;
    advance();

    if (!isAtEnd() && (peek() == '+' || peek() == '-'))
      advance();
    if (isAtEnd() || !isdigit(peek())) {
      return errorToken("Invalid float exponent in number literal!");
    }
    while (!isAtEnd() && isdigit(peek()))
      advance();
  }

  return makeToken(isFloat ? TT::FloatLiteral : TT::IntLiteral);
}

Token Lexer::scanStringLiteral() {
  auto handleEscapeSequence = [&]() {
    if (match('n') || match('r') || match('t') || match('\\') || match('\'') ||
        match('"')) {
      // Consumed a simple escape.
    } else if (match('x')) {
      // Hex escape: \xHH
      if (!std::isxdigit(peek()))
        return errorToken("Invalid hex escape in string.");
      advance(); // Consume first hex digit
      if (std::isxdigit(peek()))
        advance(); // Consume optional second
    } else if (std::isdigit(peek())) {
      // Decimal escape: \DDD
      advance(); // Consume first digit
      if (std::isdigit(peek()))
        advance(); // Consume optional second
      if (std::isdigit(peek()))
        advance(); // Consume optional third
    }

    return errorToken("Unknown escape sequence in string.");
  };

  if (match('"') && match('"')) {
    // --- Handle raw string literal: """...""" ---
    ERROR(__LINE__, "Multiline string needs to be tested");
    while (!isAtEnd()) {
      if (match('"')) {
        // Found a quote. Check if it's the """ terminator.
        if (match('"') && match('"')) {
          return makeToken(TT::StringLiteral);
        }
        // It was just one (") or two ("") quotes, not the end.
        // Keep consuming as part of the string.
      } else if (match('\\')) {
        handleEscapeSequence();
      } else if (peek() == '\n') {
        // Raw strings allow newlines.
        m_line++;
        advance();
      } else {
        // Any other character.
        advance();
      }
    }

    // Reached EOF without the """ terminator.
    return errorToken("Unterminated raw string literal.");
  }

  // --- Handle standard string literal: "..." ---
  while (!isAtEnd() && peek() != '"') {
    if (peek() == '\n') {
      return errorToken("Unterminated string literal (newline found).");
    }

    // Handle escape sequences
    if (match('\\')) {
      handleEscapeSequence();
    } else {
      // A regular character.
      advance();
    }
  }

  // Check for the closing quote.
  if (!match('"')) {
    return errorToken("Unterminated string literal.");
  }

  return makeToken(TT::StringLiteral);
}

Token Lexer::scanCharLiteral() {
  // 1. Check for <char_content>
  if (match('\\')) {
    // --- Handle <escape_sequence> ---

    // Simple escapes: \n, \r, \t, \\, \', \"
    if (match('n') || match('r') || match('t') || match('\\') || match('\'') ||
        match('"')) {
      // Valid escape, content is consumed.

    }
    // <hex_escape>: \xHH
    else if (match('x')) {
      if (!std::isxdigit(peek())) {
        return errorToken(
            "Invalid hex escape: '\\x' must be followed by hex digits.");
      }
      advance(); // Consume first hex digit
      if (std::isxdigit(peek())) {
        advance(); // Consume optional second hex digit
      }
    }
    // <decimal_escape>: \DDD
    else if (std::isdigit(peek())) {
      advance(); // Consume first digit
      if (std::isdigit(peek()))
        advance(); // Consume optional second
      if (std::isdigit(peek()))
        advance(); // Consume optional third
      // Note: A real implementation might also check if the value is > 255
    } else {
      return errorToken("Unknown escape sequence.");
    }
  } else if (peek() == '\'') {
    // This is an empty character literal, e.g., ''
    return errorToken("Empty character literal.");
  } else if (isAtEnd() || peek() == '\n') {
    // Reached end of file or a newline without a closing quote.
    return errorToken("Unterminated character literal.");
  } else if (isprint(peek())) {
    advance(); // Consume the character
  } else {
    // A non-printable char that wasn't part of an escape sequence.
    return errorToken("Invalid character in literal.");
  }

  // 2. Consume the closing "'"
  if (!match('\'')) {
    // We parsed valid content, but the closing quote is missing.
    // e.g., 'a\n or 'a' at EOF.
    return errorToken("Unterminated character literal.");
  }

  // 3. Success, token made includes in the SV: '*'
  return makeToken(TT::CharLiteral);
}

bool Lexer::isAtEnd() const {
  return m_current >= m_source.size() || m_source[m_current] == '\0';
}

char Lexer::advance() { return m_source[m_current++]; }

char Lexer::peek() const { return m_source[m_current]; }

char Lexer::peekNext() {
  if (m_current + 1 >= m_source.size() || m_source[m_current + 1] == '\0')
    return '\0';
  return m_source[m_current + 1];
}

bool Lexer::match(char expected) {
  if (isAtEnd() || m_source[m_current] != expected)
    return false;
  m_current++;
  return true;
}

Token Lexer::skipLineComment() {
  while (!isAtEnd() && peek() != '\n')
    advance();
  return ignoreToken;
}

Token Lexer::skipMultilineComment() {
  while (!isAtEnd()) {
    if (peek() == '*' && peekNext() == '/')
      break;
    if (peek() == '\n')
      m_line++;
    advance();
  }

  // Is at end of file without closing '*/'
  if (isAtEnd()) {
    Logger::fmtLog(LogLevel::Warning,
                   "Multiline comment unclosed at end of file");
    return ignoreToken;
  }

  // Consume the closing '*/'
  advance(); // consume '*'
  advance(); // consume '/'
  return ignoreToken;
}

Token Lexer::makeToken(TokenType type) {
  Token tk(type,
           std::string_view(m_source.data() + m_start, m_current - m_start),
           m_line);
  return tk;
}

Token Lexer::errorToken(const char *msg) {
  Token etk(TT::Error, msg, m_line);
  return etk;
}
