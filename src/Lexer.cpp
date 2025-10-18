#include "Lexer.h"
#define MIN_TOKENS_SIZE 128

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
    {"switch", TokenType::Kw_Switch},
    {"case", TokenType::Kw_Case},
    {"default", TokenType::Kw_Default},
    {"match", TokenType::Kw_Match},
    {"extern", TokenType::Kw_Extern},
    {"struct", TokenType::Kw_Struct},
    {"static", TokenType::Kw_Static},
    {"typealias", TokenType::Kw_typealias},
    {"defer", TokenType::Kw_Defer},
    {"break", TokenType::Kw_Break},
    {"continue", TokenType::Kw_Continue},

    {"true", TokenType::Kw_True},
    {"false", TokenType::Kw_False},
    {"null", TokenType::Kw_Null},

    {"i8", TokenType::Kw_I8},
    {"char", TokenType::Kw_Char},
    {"i16", TokenType::Kw_I16},
    {"i32", TokenType::Kw_I32},
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
    : source(src), start(0), current(0), line(1) {
  tokens.reserve(MIN_TOKENS_SIZE);
}

std::vector<Token> &Lexer::scanTokens() {

  while (!isAtEnd()) {
    start = current;

    Token token = scanToken();
    if (token.type != TT::EndOfFile)
      tokens.push_back(token);
  }

  // Add EOF token add the end of parsing of file
  tokens.push_back(Token(TT::EndOfFile, "$", line));
  tokens.shrink_to_fit();
  return tokens;
}

Token Lexer::scanToken() {
  char c = consume();
  switch (c) {
  case '(':
    return makeToken(TT::LeftParen);
  case ')':
    return makeToken(TT::RightParen);
  case '{':
    return makeToken(TT::LeftCBracket);
  case '}':
    return makeToken(TT::RightCBracket);
  case '[':
    return makeToken(TT::LeftSqBracket);
  case ']':
    return makeToken(TT::RightSqBracket);
  case ',':
    return makeToken(TT::Comma);
  case '.':
    return makeToken(TT::Dot);
  case ':':
    return makeToken(TT::Colon);
  case ';':
    return makeToken(TT::Semicolon);
  case '?':
    if (match('='))
      return makeToken(TT::QuestionEqual);
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
      skipLineComment();
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
  case ' ':
  case '\r':
  case '\t':
    // ignore whitespace
    break;
  case '\n':
    line++;
    break;
  default:
    if (std::isalpha(c) || c == '_') {
      return scanWord();
    } else if (std::isdigit(c)) {
      return scanNumber(c);
    } else {
      return errorToken("Unexpected character found!");
    }
  }

  return errorToken("Error[Lexer::scanToken] : Unreachable!");
}

Token Lexer::scanWord() {
  while (!isAtEnd() && (std::isalnum(peek()) || peek() == '_'))
    consume();
  std::string_view word(source.data() + start, current - start);

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
        consume();
      return makeToken(TT::HexLiteral);
    } else if (match('b') || match('B')) {
      while (!isAtEnd() && (peek() == '0' || peek() == '1'))
        consume();
      return makeToken(TT::BinaryLiteral);
    }
  }

  while (!isAtEnd() && std::isdigit(peek()))
    consume();
  if (!isAtEnd() && peek() == '.' && std::isdigit(peekNext())) {
    isFloat = true;
    consume();

    while (!isAtEnd() && std::isdigit(peek()))
      consume();
  }

  if (!isAtEnd() && (peek() == 'e' || peek() == 'E')) {
    isFloat = true;
    consume();

    if (!isAtEnd() && (peek() == '+' || peek() == '-'))
      consume();
    if (isAtEnd() || !isdigit(peek())) {
      return errorToken("Invalid float exponent in number literal!");
    }
    while (!isAtEnd() && isdigit(peek()))
      consume();
  }

  return makeToken(isFloat ? TT::FloatLiteral : TT::IntLiteral);
}

bool Lexer::isAtEnd() const { return source[current] == '\0'; }

char Lexer::consume() { return source[current++]; }

char Lexer::peek() const { return source[current]; }

char Lexer::peekNext() {
  if (isAtEnd())
    return '\0';
  return source[current];
}

bool Lexer::match(char expected) {
  if (isAtEnd() || source[current] != expected)
    return false;
  current++;
  return true;
}

void Lexer::skipLineComment() {
  while (!isAtEnd() && peek() != '\n')
    consume();
}

Token Lexer::makeToken(TokenType type) {
  Token tk(type, std::string_view(source.data() + start, current - start),
           line);
  return tk;
}

Token Lexer::errorToken(const char *msg) {
  Token etk(TT::Error, msg, line);
  return etk;
}
