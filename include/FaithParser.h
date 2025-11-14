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

  [[nodiscard]] std::unique_ptr<Faith::ExternDecl> parseExternFuncDecl();

  [[nodiscard]] std::unique_ptr<Faith::StaticDef> parseStaticFuncDecl();

  [[nodiscard]] std::unique_ptr<Faith::FuncDecl>
  parseFuncDecl(bool expectOnlyDecl);

  [[nodiscard]] std::unique_ptr<Faith::paramsList> parseParamsList();

  [[nodiscard]] std::unique_ptr<Faith::Param> parseParam();

  [[nodiscard]] std::unique_ptr<Faith::VarDecl>
  parseVarDecl(bool isConst = false, bool isGlobal = false);

  [[nodiscard]] std::unique_ptr<Faith::StructDecl> parseStructDecl();

  [[nodiscard]] std::unique_ptr<Faith::StructField> parseStructField();

  [[nodiscard]] std::unique_ptr<Faith::TypealiasDecl> parseTypeAliasDecl();

  [[nodiscard]] std::unique_ptr<Faith::CompoundStmt> parseCompoundStmt();

  [[nodiscard]] std::unique_ptr<Faith::StmtList> parseStmtList();

  [[nodiscard]] std::unique_ptr<Faith::Stmt> parseStmt();

  [[nodiscard]] std::unique_ptr<Faith::WhileStmt> parseWhileStmt();

  [[nodiscard]] std::unique_ptr<Faith::ForStmt> parseForStmt();

  [[nodiscard]] std::unique_ptr<Faith::DeferStmt> parseDeferStmt();

  [[nodiscard]] std::unique_ptr<Faith::IfStmt> parseIfStmt();

  [[nodiscard]] std::unique_ptr<Faith::Stmt> parseElseBlock();

  [[nodiscard]] std::unique_ptr<Faith::ReturnStmt> parseReturnStmt();

  [[nodiscard]] std::unique_ptr<Faith::ExprStmt> parseExprStmt();

  [[nodiscard]] std::unique_ptr<Faith::TypeSpec> parseTypeSpec();

  [[nodiscard]] std::unique_ptr<Faith::BaseType> parseBaseType();

  [[nodiscard]] std::unique_ptr<Faith::PrimitiveType>
  parsePrimitiveType(Faith::PrimitiveTypeKind kind);

  [[nodiscard]] std::unique_ptr<Faith::StructType> parseStructType();

  [[nodiscard]] std::unique_ptr<Faith::FuncPtrType> parseFuncPtrType();

  [[nodiscard]] std::unique_ptr<Faith::paramsTypeVec> parseParamTypeList();

  [[nodiscard]] std::unique_ptr<Faith::PtrType> parsePtrType();

  [[nodiscard]] std::unique_ptr<Faith::RefType> parseRefType();

  [[nodiscard]] std::unique_ptr<Faith::Expr> parseExpr();

  [[nodiscard]] std::unique_ptr<Faith::Expr> parseAssignExpr();

  [[nodiscard]] std::unique_ptr<Faith::Expr> parseConditionalExpr();

  [[nodiscard]] std::unique_ptr<Faith::Expr> parseLogicalOrExpr();

  [[nodiscard]] std::unique_ptr<Faith::Expr> parseLogicalAndExpr();

  [[nodiscard]] std::unique_ptr<Faith::Expr> parseBitOrExpr();

  [[nodiscard]] std::unique_ptr<Faith::Expr> parseBitXorExpr();

  [[nodiscard]] std::unique_ptr<Faith::Expr> parseBitAndExpr();

  [[nodiscard]] std::unique_ptr<Faith::Expr> parseEqualityExpr();

  [[nodiscard]] std::unique_ptr<Faith::Expr> parseRelationalExpr();

  [[nodiscard]] std::unique_ptr<Faith::Expr> parseShiftExpr();

  [[nodiscard]] std::unique_ptr<Faith::Expr> parseAddExpr();

  [[nodiscard]] std::unique_ptr<Faith::Expr> parseMultExpr();

  [[nodiscard]] std::unique_ptr<Faith::Expr> parseCastExpr();

  [[nodiscard]] std::unique_ptr<Faith::Expr> parseUnaryExpr();

  [[nodiscard]] std::unique_ptr<Faith::Expr> parsePostfixExpr();

  [[nodiscard]] std::unique_ptr<Faith::Expr> parsePrimaryExpr();

  [[nodiscard]] std::unique_ptr<Faith::argList> parseArgList();

  [[nodiscard]] std::unique_ptr<Faith::StructInit> parseStructInit();

  [[nodiscard]] std::unique_ptr<Faith::structInitFieldList>
  parseStructInitFieldList();

  [[nodiscard]] std::unique_ptr<Faith::StructInitField> parseStructInitField();

  // Helper class for parsing right hand side of expr
  using ParseFuncPtr = std::unique_ptr<Faith::Expr> (FaithParser::*)();

  std::unique_ptr<Faith::Expr>
  parseBinaryRHS(std::unique_ptr<Faith::Expr> leftExpr, ParseFuncPtr parseRHS,
                 Faith::BinaryOp type);

  // Creates and pushes a error onto stack
  // Returns the index of error message
  uint32_t createError(std::string &&errMessage);
  uint32_t createGlobalError(std::string &&errMessage);

  // Prints error to logger
  void printErrors();

  // Only provide view of Tokens, dont consume them
  [[nodiscard]] Faith::TokenView peek();
  [[nodiscard]] Faith::TokenView peekNext(uint64_t lookahead = 1);
  [[nodiscard]] Faith::TokenView peekPrev();

  // Consumers of Token
  Faith::TokenView advance();
  Faith::TokenView match(TokenType type);
  std::optional<Faith::AssignmentOp> matchAssignmentOp();

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