#ifndef FAITH_PARSER_H
#define FAITH_PARSER_H

#include <memory>
#include <optional>
#include <stack>
#include <string_view>
#include <vector>

#include "FaithAST.h"
#include "Lexer.h"
#include "Logger.h"

using paramsTypeVec = std::vector<std::unique_ptr<Faith::TypeSpec>>;

// Error struct
struct parseError {
  // Location of the token that triggered the error
  Faith::TokenView errorLoc;

  // The specific error message (must be owning)
  std::string message;

  // Default construct
  parseError() : errorLoc(nullptr) {}

  // Construct with only message
  parseError(std::string &&msg) : errorLoc(nullptr), message(msg) {}

  // Constructor to set both members
  parseError(Faith::TokenView location, std::string &&msg)
      : errorLoc(location), message(std::move(msg)) {}
};

class FaithParser {
public:
  FaithParser(const std::vector<Token> &tokens);
  std::unique_ptr<Faith::Program> parse();

private:
  // Parser functions
  [[nodiscard]] std::unique_ptr<Faith::Decl> parseDecl();

  [[nodiscard]] std::unique_ptr<Faith::FuncDecl>
  parseFuncDecl(Faith::LinkageSpecifier linkage);

  [[nodiscard]] std::unique_ptr<Faith::VarDecl>
  parseVarDecl(bool isConst = false, bool isGlobal = false);

  [[nodiscard]] std::unique_ptr<Faith::StructDecl> parseStructDecl();

  [[nodiscard]] std::unique_ptr<Faith::TypealiasDecl> parseTypeAliasDecl();

  [[nodiscard]] std::unique_ptr<Faith::TypeSpec> parseTypeSpec();

  [[nodiscard]] std::unique_ptr<Faith::BaseType> parseBaseType();

  [[nodiscard]] std::unique_ptr<Faith::PrimitiveType>
  parsePrimitiveType(Faith::PrimitiveTypeKind kind);

  [[nodiscard]] std::unique_ptr<Faith::StructType> parseStructType();

  [[nodiscard]] std::unique_ptr<Faith::FuncPtrType> parseFuncPtrType();

  [[nodiscard]] std::unique_ptr<paramsTypeVec> parseParamTypeList();

  [[nodiscard]] std::unique_ptr<Faith::PtrType> parsePtrType();

  [[nodiscard]] std::unique_ptr<Faith::RefType> parseRefType();

  // Creates and pushes a error onto stack
  // Returns the index of error message
  uint32_t createError(Faith::TokenView loc, std::string &&errMessage);

  uint32_t createError(std::string &&errMessage);

  // Prints error to logger
  void printErrors();

  // Only provide view of Tokens, dont consume them
  [[nodiscard]] Faith::TokenView peek();
  [[nodiscard]] Faith::TokenView peekNext();
  [[nodiscard]] Faith::TokenView peekPrev();

  // Consumers of Token
  Faith::TokenView advance();
  Faith::TokenView match(TokenType type);

  // Is at end helper function
  bool isAtEnd() const;

  const std::vector<Token> &m_tokens;
  std::unique_ptr<Faith::Program> m_program;
  size_t m_current;
  size_t m_tokenArrSize;

  std::stack<parseError> m_errorStack;
  uint32_t m_errNum;
};

#endif