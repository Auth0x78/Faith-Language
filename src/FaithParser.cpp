#include "FaithParser.h"

#define NYI(msg) throw std::runtime_error(msg)

using token_view = Faith::TokenView;
using primTy = Faith::PrimitiveTypeKind;
using binOp = Faith::BinaryOp;

#pragma region Helper & Constructor Functions

FaithParser::FaithParser(const std::vector<Token> &tokens)
    : m_current(0), m_tokens(tokens), m_tokenArrSize(m_tokens.size()),
      m_program(std::make_unique<Faith::Program>()), m_errorStack(),
      m_errNum(0) {}

std::unique_ptr<Faith::Expr>
FaithParser::parseBinaryRHS(std::unique_ptr<Faith::Expr> leftExpr,
                            ParseFuncPtr parseRHS, Faith::BinaryOp type) {
  // Consume the operator token
  token_view token = advance();

  // Check if any tokens available to consume, if not ret with error
  if (isAtEnd()) {
    createError("Expected an expression on the right of the binary operator!");
    return nullptr;
  }

  // Call member function of this class
  auto rightExpr = (this->*parseRHS)();

  if (rightExpr == nullptr)
    return nullptr; // Error was already created by parseRHS

  // Create the new expression, making the old leftExpr the left child
  return std::make_unique<Faith::BinaryExpr>(std::move(leftExpr), token, type,
                                             std::move(rightExpr));
}

uint32_t FaithParser::createError(std::string &&errMessage) {

  parseError &err = m_errorStack.emplace();
  err.message = errMessage;
  err.errorLoc = peekPrev();

  return m_errNum++;
}

uint32_t FaithParser::createGlobalError(std::string &&errMessage) {
  parseError &err = m_errorStack.emplace();
  err.message = errMessage;

  return m_errNum++;
}

void FaithParser::printErrors() {
  if (m_errorStack.empty()) {
    Logger::Log(LogLevel::Info, "No parser errors found.");
    return;
  }

  Logger::fmtLog(LogLevel::Error,
                 "=== Parser Errors (%zu) ===", m_errorStack.size());

  // Copy stack to preserve it
  std::stack<parseError> tempStack = m_errorStack;
  uint32_t count = 0;

  while (!tempStack.empty()) {
    const parseError &err = tempStack.top();
    ++count;

    // If Faith::TokenView gives location info, print it
    if (err.errorLoc) {
      Logger::fmtLog(LogLevel::Error, "#%u | Token at line %u, column %u -> %s",
                     count, err.errorLoc->line, err.message.c_str());
    } else {
      Logger::fmtLog(LogLevel::Error, "#%u | <unknown location> -> %s", count,
                     err.message.c_str());
    }

    tempStack.pop();
  }

  Logger::Log(LogLevel::Error, "=============================");
};

Faith::TokenView FaithParser::match(TokenType type) {
  if (isAtEnd() || m_tokens[m_current].type != type)
    return nullptr;

  return advance();
}

std::optional<Faith::AssignmentOp> FaithParser::matchAssignmentOp() {
  using assignOp = Faith::AssignmentOp;
  token_view curr = peek();
  if (!curr.has_value())
    return {};

  Faith::AssignmentOp opType;

  switch (curr->type) {
  case TokenType::Equal:
    opType = assignOp::Assign;
    break;
  case TokenType::PlusEqual:
    opType = assignOp::AddAssign;
    break;
  case TokenType::MinusEqual:
    opType = assignOp::SubAssign;
    break;
  case TokenType::StarEqual:
    opType = assignOp::MulAssign;
    break;
  case TokenType::SlashEqual:
    opType = assignOp::DivAssign;
    break;
  case TokenType::PercentEqual:
    opType = assignOp::ModAssign;
    break;
  case TokenType::AndEqual:
    opType = assignOp::BitAndAssign;
    break;
  case TokenType::OrEqual:
    opType = assignOp::BitOrAssign;
    break;
  case TokenType::XorEqual:
    opType = assignOp::BitXorAssign;
    break;
  default:
    return {};
  }

  advance();
  return {opType};
}

Faith::TokenView FaithParser::advance() {
  const Token *addr = &m_tokens[m_current];
  m_current++;

  return Faith::TokenView(addr);
}

Faith::TokenView FaithParser::peekNext(uint64_t lookahead) {
  if (m_current + lookahead >= m_tokenArrSize)
    return nullptr;

  return Faith::TokenView(&m_tokens[m_current + lookahead]);
}

Faith::TokenView FaithParser::peekPrev() {
  return Faith::TokenView(&m_tokens[m_current - 1]);
}

Faith::TokenView FaithParser::peek() {

  return m_current >= m_tokenArrSize ? nullptr
                                     : Faith::TokenView(&m_tokens[m_current]);
}

bool FaithParser::isAtEnd() const {
  if (m_current >= m_tokenArrSize)
    return true;

  return m_tokens[m_current].type == TokenType::EndOfFile;
}
#pragma endregion

std::unique_ptr<Faith::Program> FaithParser::parse() {
  while (!isAtEnd()) {
    auto decl = parseDecl();
    if (decl != nullptr)
      m_program->declarations.push_back(std::move(decl));
    else {
      printErrors();
      NYI("NYI: Error Recovery!");
    }
  }

  if (m_errNum != 0) {
    createGlobalError("Failed to compile program!");
  }

  return std::move(m_program);
}

std::unique_ptr<Faith::Decl> FaithParser::parseDecl() {
  token_view current = peek();
  std::unique_ptr<Faith::Decl> decl;

  switch (current->type) {
  // Guareented to be just declaration
  case TokenType::Kw_Extern:
    return parseExternFuncDecl();
  // Parse function decl or def
  case TokenType::Kw_Static:
    return parseStaticFuncDecl();
  case TokenType::Kw_Func:
    return parseFuncDecl(false);
    break;
  case TokenType::Kw_Let:
    return parseVarDecl(false, true);
  case TokenType::Kw_Const:
    return parseVarDecl(true, true);
    // Parse global variable decl
  case TokenType::Kw_Struct:
    // Parse both struct decl as well as forward decl
    return parseStructDecl();
  case TokenType::Kw_Typealias:
    // Type alias declaration
    return parseTypeAliasDecl();
  default:
    break;
  }

  // Default error exit before that consume token
  advance();
  createGlobalError("Invalid declaration starting with token: " +
                    std::string(current->token));
  return nullptr;
}

std::unique_ptr<Faith::ExternDecl> FaithParser::parseExternFuncDecl() {
  // consume the extern token
  auto externTok = advance();

  auto optStr = match(TokenType::StringLiteral);

  // True => Tell parse func decl to only parse func declarations and not
  // defination
  auto funcDecl = parseFuncDecl(true);
  if (funcDecl == nullptr)
    return nullptr;

  return std::make_unique<Faith::ExternDecl>(
      externTok, optStr.has_value() ? optStr : nullptr, std::move(funcDecl));
}

std::unique_ptr<Faith::StaticDef> FaithParser::parseStaticFuncDecl() {
  // consume the static token
  token_view staticTok = advance();

  // False => Tell parse func decl to parse func declarations and try to parse
  // defination (decl followed by compound body)
  auto funcDecl = parseFuncDecl(false);
  if (funcDecl == nullptr)
    return nullptr;

  return std::make_unique<Faith::StaticDef>(staticTok, std::move(funcDecl));
}

std::unique_ptr<Faith::FuncDecl>
FaithParser::parseFuncDecl(bool expectOnlyDecl) {
  token_view funcTok = advance();

  token_view ident = match(TokenType::Identifier);
  if (!ident) {
    createError("Expected a identifer function name after 'func'!");
    return nullptr;
  }

  if (!match(TokenType::LeftParen)) {
    createError("Expected a '(' after identfier!");
    return nullptr;
  }

  std::unique_ptr<Faith::paramsList> paramsVec = nullptr;
  if (!isAtEnd() && peek()->type != TokenType::RightParen) {
    paramsVec = parseParamsList();

    // TODO: Error Recovery
    if (paramsVec == nullptr)
      return nullptr;
  }

  if (!match(TokenType::RightParen)) {
    if (paramsVec != nullptr)
      createError("Expected a ')' after end of parameter list!");
    else
      createError("Expected a ')' after left parenthesis '('!");
    return nullptr;
  }

  std::unique_ptr<Faith::TypeSpec> retType = std::make_unique<Faith::TypeSpec>(
      nullptr, false,
      std::make_unique<Faith::PrimitiveType>(primTy::VOID, nullptr), 0);

  if (match(TokenType::Arrow)) {
    if (isAtEnd()) {
      createError("Expected return type after '->'!");
      return nullptr;
    }

    retType = parseTypeSpec();
    if (retType == nullptr)
      return nullptr;
  }

  if (match(TokenType::Semicolon)) {
    return std::make_unique<Faith::FuncDecl>(
        funcTok, ident, std::move(paramsVec), std::move(retType));
  }

  // Parse Function body and return func defination
  if (expectOnlyDecl) {
    createError("Expected a ';' after the function declaration!");
    return nullptr;
  }

  if (isAtEnd()) {
    createError("Expected a function body but got end of file instead!");
    return nullptr;
  }

  auto fnBody = parseCompoundStmt();
  if (fnBody == nullptr)
    return nullptr;

  return std::make_unique<Faith::FuncDef>(funcTok, ident, std::move(paramsVec),
                                          std::move(retType),
                                          std::move(fnBody));
}

std::unique_ptr<Faith::paramsList> FaithParser::parseParamsList() {
  auto paramlist = std::make_unique<Faith::paramsList>();

  auto param0 = parseParam();
  if (param0 == nullptr)
    return nullptr;
  paramlist->push_back(std::move(param0));

  // Parse the all the "," <type> till we either reach end
  // Or we till we dont have any more "," to match
  while (match(TokenType::Comma)) {
    if (isAtEnd()) {
      createError("Expected a type after ',' but got end of file!");
      return nullptr;
    }

    auto paramN = parseParam();

    // TODO: Error recovery till next sync token
    if (paramN == nullptr)
      return nullptr;

    paramlist->push_back(std::move(paramN));
  }

  return paramlist;
}

std::unique_ptr<Faith::Param> FaithParser::parseParam() {
  auto ident = match(TokenType::Identifier);
  if (!ident) {
    createError("Expected a name for the param");
    return nullptr;
  }

  if (!match(TokenType::Colon)) {
    createError("Expected a ':' after param name!");
    return nullptr;
  }

  // Now a <type> should be present
  if (isAtEnd()) {
    createError("Expected a type for the parameter!");
    return nullptr;
  }

  auto type = parseTypeSpec();

  return std::make_unique<Faith::Param>(ident, std::move(type));
}

std::unique_ptr<Faith::VarDecl> FaithParser::parseVarDecl(bool isConst,
                                                          bool isGlobal) {
  // Consume the let or const tok
  auto varDeclTok = advance();

  auto ident = match(TokenType::Identifier);
  if (!ident) {
    createError("Expected an identifier name for variable declaration!");
    return nullptr;
  }

  if (!match(TokenType::Colon)) {
    createError("Expected a ':' after name identifier!");
    return nullptr;
  }

  if (isAtEnd()) {
    createError("Expected a variable type but got end of file!");
    return nullptr;
  }

  auto type = parseTypeSpec();
  if (type == nullptr)
    return nullptr;

  std::unique_ptr<Faith::Expr> initExpr = nullptr;
  if (match(TokenType::Equal)) {
    if (isAtEnd()) {
      createError("Expected a initializer expression but got end of file!");
      return nullptr;
    }

    // Now next must a initializer expr
    initExpr = parseExpr();
    if (initExpr == nullptr)
      return nullptr;
  }

  if (!match(TokenType::Semicolon)) {
    createError("Expected a ';' at the end of variable declaration!");
    return nullptr;
  }

  return std::make_unique<Faith::VarDecl>(isConst, isGlobal, ident,
                                          std::move(type), std::move(initExpr));
}

std::unique_ptr<Faith::StructDecl> FaithParser::parseStructDecl() {
  // TODO: Implement parsing of struct decl
  auto structToken = advance();

  auto ident = match(TokenType::Identifier);
  if (!ident) {
    createError("Expected a name for the struct!");
    return nullptr;
  }

  if (!match(TokenType::LeftBrace)) {
    createError("Expected '{' after struct name!");
    return nullptr;
  }

  auto sbody = std::make_unique<Faith::StructBody>();

  while (peek().has_value() && peek()->type != TokenType::RightBrace) {
    auto structField = parseStructField();
    if (structField == nullptr)
      return nullptr;

    sbody->push_back(std::move(structField));
  }

  if (!match(TokenType::RightBrace)) {
    createError("Expected a '}' after struct fields!");
    return nullptr;
  }

  if (!match(TokenType::Semicolon)) {
    createError("Expected a ';' after end of the declaration!");
    return nullptr;
  }

  return std::make_unique<Faith::StructDecl>(ident, std::move(sbody));
}

std::unique_ptr<Faith::StructField> FaithParser::parseStructField() {
  auto memberName = match(TokenType::Identifier);

  if (!memberName) {
    createError("Expected a struct member field name!");
    return nullptr;
  }

  if (!match(TokenType::Colon)) {
    createError("Expected a ':' after memberName followed by type!");
    return nullptr;
  }

  if (isAtEnd()) {
    createError("Expected a type for struct member!");
    return nullptr;
  }

  auto type = parseTypeSpec();
  if (type == nullptr)
    return nullptr;

  std::unique_ptr<Faith::Expr> defVal = nullptr;
  if (match(TokenType::Equal)) {
    if (isAtEnd()) {
      createError("Expected a compile time constant expression after '='!");
      return nullptr;
    }

    defVal = parseLogicalOrExpr();
    if (defVal == nullptr)
      return nullptr;
  }

  if (!match(TokenType::Semicolon)) {
    createError("Expected ';' at the end of struct field parameter!");
    return nullptr;
  }

  return std::make_unique<Faith::StructField>(memberName, std::move(type),
                                              std::move(defVal));
}

std::unique_ptr<Faith::TypealiasDecl> FaithParser::parseTypeAliasDecl() {
  // consume the initial typealias token
  auto typeAliasTok = advance();

  auto aliasType = match(TokenType::Identifier);
  if (!aliasType) {
    createError("Expected identifier for type alias name after 'typealias'.");
    return nullptr;
  }

  auto equalTok = match(TokenType::Equal);
  if (!equalTok) {
    createError("Expected '=' after type alias name.");
    return nullptr;
  }

  // Parse the type specification that the alias refers to
  if (isAtEnd()) {
    createError("Expected a type for target type after '='");
    return nullptr;
  }
  auto targetType = parseTypeSpec();
  if (!targetType)
    return nullptr;

  // Expect the terminating semicolon ';'
  if (!match(TokenType::Semicolon)) {
    createError("Expected ';' after type alias definition.");
    return nullptr;
  }

  // Return the constructed AST node
  return std::make_unique<Faith::TypealiasDecl>(aliasType,
                                                std::move(targetType));
}

std::unique_ptr<Faith::CompoundStmt> FaithParser::parseCompoundStmt() {
  token_view leftBrace = match(TokenType::LeftBrace);
  if (!leftBrace) {
    createError("Expected '{' after function definition!");
    return nullptr;
  }

  std::unique_ptr<Faith::StmtList> stmtList = nullptr;
  if (peek().has_value() && peek()->type != TokenType::RightBrace) {
    stmtList = parseStmtList();
    if (stmtList == nullptr)
      return nullptr;
  }

  if (!match(TokenType::RightBrace)) {
    createError("Expected '}' at the end of compound statement!");
    return nullptr;
  }

  return std::make_unique<Faith::CompoundStmt>(leftBrace, stmtList);
}

std::unique_ptr<Faith::StmtList> FaithParser::parseStmtList() {
  // Create a stmtlist and parse and push 0th stmt onto list
  auto stmtList = std::make_unique<Faith::StmtList>();
  {
    auto stmt0 = parseStmt();
    if (stmt0 == nullptr)
      return nullptr;

    stmtList->push_back(std::move(stmt0));
  }

  // If we find more statements then parse and push onto list
  while (!isAtEnd() && peek()->type != TokenType::RightBrace) {
    auto stmtN = parseStmt();
    if (stmtN == nullptr)
      return nullptr;

    stmtList->push_back(std::move(stmtN));
  }

  return stmtList;
}

std::unique_ptr<Faith::Stmt> FaithParser::parseStmt() {
  switch (peek()->type) {
  case TokenType::Kw_Let:
    return parseVarDecl(false, false);
  case TokenType::Kw_Const:
    return parseVarDecl(true, false);
  case TokenType::Kw_If:
    return parseIfStmt();
  case TokenType::Kw_While:
    return parseWhileStmt();
  case TokenType::Kw_For:
    NYI("NYI: Parse for statments!");
  case TokenType::Kw_Return:
    return parseReturnStmt();
  case TokenType::Kw_Defer:
    return parseDeferStmt();
  case TokenType::Kw_Match:
    NYI("NYI: Parse match statments!");
  case TokenType::Kw_Break:
    return std::make_unique<Faith::BreakStmt>(advance());
  case TokenType::Kw_Continue:
    return std::make_unique<Faith::ContinueStmt>(advance());
  case TokenType::LeftBrace:
    return parseCompoundStmt();
  case TokenType::Semicolon:
    return std::make_unique<Faith::EmptyStmt>(advance());
  default:
    return parseExprStmt();
  }

  NYI("Unreachable: Reached unreachable section in parseStmt()");
  return std::unique_ptr<Faith::Stmt>();
}

std::unique_ptr<Faith::WhileStmt> FaithParser::parseWhileStmt() {
  // Consume the while token
  token_view whileTok = advance();

  if (!match(TokenType::LeftParen)) {
    createError("Expected '(' after 'while' token!");
    return nullptr;
  }

  if (isAtEnd()) {
    createError("Expected an expression after '(' in while statement!");
    return nullptr;
  }

  auto conditionExpr = parseExpr();
  if (conditionExpr == nullptr)
    return nullptr;

  if (!match(TokenType::RightParen)) {
    createError("Expected ')' after condition in while statement!");
    return nullptr;
  }

  if (isAtEnd()) {
    createError("Expected a statement after ')' in 'while' statement!");
    return nullptr;
  }

  auto stmtBody = parseStmt();
  if (stmtBody == nullptr)
    return nullptr;

  return std::make_unique<Faith::WhileStmt>(whileTok, std::move(conditionExpr),
                                            std::move(stmtBody));
}

std::unique_ptr<Faith::ForStmt> FaithParser::parseForStmt() {
  return std::unique_ptr<Faith::ForStmt>();
}

std::unique_ptr<Faith::DeferStmt> FaithParser::parseDeferStmt() {
  token_view deferTok = advance();

  if (isAtEnd()) {
    createError("Expected an expression to defer after the 'defer' keyword!");
    return nullptr;
  }

  auto exitExpr = parseExpr();
  if (exitExpr == nullptr)
    return nullptr;

  if (!match(TokenType::Semicolon)) {
    createError("Expected ';' after exit expression in 'defer' stmt!");
    return nullptr;
  }

  return std::make_unique<Faith::DeferStmt>(deferTok, exitExpr);
}

std::unique_ptr<Faith::IfStmt> FaithParser::parseIfStmt() {
  token_view iftok = advance();

  if (!match(TokenType::LeftParen)) {
    createError("Expected '(' after 'if' token!");
    return nullptr;
  }

  if (isAtEnd()) {
    createError("Expected an expression after '(' in if statement!");
    return nullptr;
  }

  auto conditionExpr = parseExpr();
  if (conditionExpr == nullptr)
    return nullptr;

  if (!match(TokenType::RightParen)) {
    createError("Expected ')' after expression!");
    return nullptr;
  }

  if (isAtEnd()) {
    createError("Expected a statement after ')' in 'if' statement!");
    return nullptr;
  }

  auto thenBody = parseStmt();
  if (thenBody == nullptr)
    return nullptr;

  if (peek().has_value() && peek()->type == TokenType::Kw_Else) {
    auto elseBody = parseElseBlock();
    if (elseBody == nullptr)
      return nullptr;

    return std::make_unique<Faith::IfStmt>(iftok, std::move(conditionExpr),
                                           std::move(thenBody),
                                           std::move(elseBody));
  }

  return std::make_unique<Faith::IfStmt>(iftok, std::move(conditionExpr),
                                         std::move(thenBody));
}

std::unique_ptr<Faith::Stmt> FaithParser::parseElseBlock() {
  // Consume the "else" token and then parse stmt after "else" token
  advance();
  return parseStmt();
}

std::unique_ptr<Faith::ReturnStmt> FaithParser::parseReturnStmt() {
  token_view retTok = advance();

  if (isAtEnd()) {
    createError("Expected an expression or ';' after return!");
    return nullptr;
  }

  std::unique_ptr<Faith::Expr> expr = nullptr;
  if (peek().has_value() && peek()->type != TokenType::Semicolon) {
    expr = parseExpr();
    if (expr == nullptr)
      return nullptr;
  }

  if (!match(TokenType::Semicolon)) {
    createError("Expected a ';' at end of return statement!");
    return nullptr;
  }

  return std::make_unique<Faith::ReturnStmt>(retTok, std::move(expr));
}

std::unique_ptr<Faith::ExprStmt> FaithParser::parseExprStmt() {
  // Try parse expression statement
  auto exprStmt = parseExpr();
  if (exprStmt == nullptr) {
    createError("Expected a statement!");
    return nullptr;
  }

  if (!match(TokenType::Semicolon)) {
    createError("Expected a ';' at the end of expression statement!");
    return nullptr;
  }

  return std::make_unique<Faith::ExprStmt>(std::move(exprStmt));
}

std::unique_ptr<Faith::TypeSpec> FaithParser::parseTypeSpec() {

  auto locToken = peek();
  auto bang = match(TokenType::Bang);

  if (isAtEnd()) {
    createError("Expected a type after '!'");
    return nullptr;
  }
  // Parse base type, if tokens are avail.
  auto base = parseBaseType();
  if (base == nullptr)
    return nullptr;

  int arrDim = 0;

  while (match(TokenType::LeftBracket)) {
    if (!match(TokenType::RightBracket)) {
      // If ']' is missing, this is a syntax error.
      createError("Expected ']' to close array type suffix.");
      return nullptr;
    }
    arrDim++;
  }

  return std::make_unique<Faith::TypeSpec>(locToken, bang.has_value(),
                                           std::move(base), arrDim);
}

std::unique_ptr<Faith::BaseType> FaithParser::parseBaseType() {
  // Base type token
  token_view base = peek();

  switch (base->type) {
  case TokenType::Kw_U8:
    return parsePrimitiveType(primTy::U8);
  case TokenType::Kw_U16:
    return parsePrimitiveType(primTy::U16);
  case TokenType::Kw_U32:
    return parsePrimitiveType(primTy::U32);
  case TokenType::Kw_U64:
    return parsePrimitiveType(primTy::U64);
  case TokenType::Kw_I8:
    return parsePrimitiveType(primTy::I8);
  case TokenType::Kw_I16:
    return parsePrimitiveType(primTy::I16);
  case TokenType::Kw_I32:
    return parsePrimitiveType(primTy::I32);
  case TokenType::Kw_I64:
    return parsePrimitiveType(primTy::I64);
  case TokenType::Kw_F32:
    return parsePrimitiveType(primTy::F32);
  case TokenType::Kw_F64:
    return parsePrimitiveType(primTy::F64);
  case TokenType::Kw_Char:
    return parsePrimitiveType(primTy::CHAR);
  case TokenType::Kw_Bool:
    return parsePrimitiveType(primTy::BOOL);
  case TokenType::Kw_Void:
    return parsePrimitiveType(primTy::VOID);
  // Parse struct type
  case TokenType::Identifier:
    return parseStructType();
  // Identify if its ptr to baseType or func pointer type
  // Parse the ptr to baseType recursively
  case TokenType::Star: {
    // Distinguish between ptr to base type and func ptr
    auto lookupFunc = peekNext();
    if (lookupFunc.has_value() && lookupFunc->type == TokenType::Kw_Func)
      return parseFuncPtrType();
  }
    return parsePtrType();
  case TokenType::Ampersand:
    return parseRefType();
  default:
    break;
  }

  createError("Unrecognized base type: " + std::string(base->token));
  return nullptr;
}

std::unique_ptr<Faith::PrimitiveType>
FaithParser::parsePrimitiveType(Faith::PrimitiveTypeKind kind) {
  if (!isAtEnd())
    return std::make_unique<Faith::PrimitiveType>(kind, advance());

  // The parser expected a token but hit the end of the file.
  createError("Expected a primitive type keyword but reached end of file.");
  return nullptr;
}

std::unique_ptr<Faith::StructType> FaithParser::parseStructType() {
  if (!isAtEnd())
    return std::make_unique<Faith::StructType>(advance());

  // The parser expected a identfier but hit the end of the file.
  createError("Expected name of struct type but reached end of file.");
  return nullptr;
}

std::unique_ptr<Faith::FuncPtrType> FaithParser::parseFuncPtrType() {
  // Store and consume the '*' & 'func' token for location tracking
  token_view starTok = advance();

  // Expect and consume the 'func' keyword
  token_view funcTok = match(TokenType::Kw_Func);
  if (!funcTok) {
    createError("Expected 'func' keyword after pointer operator '*'.");
    return nullptr;
  }

  // Expect and consume the opening parenthesis '('
  token_view leftParen = match(TokenType::LeftParen);
  if (!leftParen) {
    createError("Expected a '(' after 'func' but reached end of file.");
    return nullptr;
  }

  // Check if we can proceed ahead
  if (isAtEnd()) {
    createError("Expected a ')' or params type list after '(' but "
                "reached end of file.");
    return nullptr;
  }

  // Check if the parameter list is empty (i.e., we see ')')

  std::unique_ptr<Faith::paramsTypeVec> paramsTy = nullptr;
  if (peek()->type != TokenType::RightParen) {
    // If not ')' then it must be start of list
    paramsTy = parseParamTypeList();

    // An error occured while parsing types param
    // TODO: Perform error recovery by consume till ')' or ','
    if (paramsTy == nullptr)
      return nullptr;
  }

  token_view rightParen = match(TokenType::RightParen);
  if (!rightParen) {
    createError("Expected a ')' at the end!");
    return nullptr;
  }

  token_view arrow = match(TokenType::Arrow);
  if (!arrow) {
    createError("Expected a '->' after ')'!");
    return nullptr;
  }

  if (isAtEnd()) {
    createError("Expected a return type after '->' but got end of file!");
    return nullptr;
  }

  auto retType = parseTypeSpec();
  if (retType == nullptr)
    return nullptr;

  return std::make_unique<Faith::FuncPtrType>(starTok, std::move(paramsTy),
                                              std::move(retType));
}

std::unique_ptr<Faith::paramsTypeVec> FaithParser::parseParamTypeList() {
  auto paramsTyList = std::make_unique<Faith::paramsTypeVec>();

  // Parse the first <type> param rule
  auto type0 = parseTypeSpec();
  if (type0 == nullptr)
    return nullptr;
  paramsTyList->push_back(std::move(type0));

  // Parse the all the "," <type> till we either reach end
  // Or we till we dont have any more "," to match
  while (match(TokenType::Comma)) {
    if (isAtEnd()) {
      createError("Expected another type but got end of file!");
      return nullptr;
    }

    auto typeN = parseTypeSpec();

    if (typeN == nullptr) // TODO: Error recovery till next sync token
      return nullptr;

    paramsTyList->push_back(std::move(typeN));
  }

  return paramsTyList;
}

std::unique_ptr<Faith::PtrType> FaithParser::parsePtrType() {
  // Store and consume the '*' token for location tracking.
  token_view starTok = advance();

  if (isAtEnd()) {
    createError("Expected a base type (e.g., 'i32', 'f32') after pointer "
                "operator '*' but reached end of file.");
    return nullptr;
  }

  // Parse the base type that the pointer points to (e.g., 'int' in '*int').
  //    This is the recursive step.
  std::unique_ptr<Faith::BaseType> baseType = parseBaseType();

  // Propagate error upwards
  if (baseType == nullptr)
    return nullptr;

  return std::make_unique<Faith::PtrType>(starTok, std::move(baseType));
}

std::unique_ptr<Faith::RefType> FaithParser::parseRefType() {
  // Store and consume the '&' token for location tracking.
  token_view ampTok = advance();

  if (isAtEnd()) {
    createError("Expected a base type (e.g., 'i32', 'f32') after reference "
                "operator '&' but reached end of file.");
    return nullptr;
  }

  // Parse the base type that it references.
  //    This is the recursive step.
  std::unique_ptr<Faith::BaseType> baseType = parseBaseType();

  // Propagate error upwards
  if (baseType == nullptr)
    return nullptr;

  return std::make_unique<Faith::RefType>(ampTok, std::move(baseType));
}

std::unique_ptr<Faith::Expr> FaithParser::parseExpr() {
  return parseAssignExpr();
}

std::unique_ptr<Faith::Expr> FaithParser::parseAssignExpr() {
  auto leftExpr = parseConditionalExpr();
  if (leftExpr == nullptr)
    return nullptr;

  // Store temp token before matching, as
  // match will consume it
  token_view opTok = peek();
  auto opType = matchAssignmentOp();
  if (!opType.has_value())
    return leftExpr;

  if (isAtEnd()) {
    createError("Expected an expr on the right of assignment operation!");
    return nullptr;
  }
  auto rightExpr = parseAssignExpr();
  if (rightExpr == nullptr)
    return nullptr;

  return std::make_unique<Faith::AssignmentExpr>(
      std::move(leftExpr), opTok, opType.value(), std::move(rightExpr));
}

std::unique_ptr<Faith::Expr> FaithParser::parseConditionalExpr() {
  auto condition = parseLogicalOrExpr();
  if (!condition)
    return nullptr;

  // Store temp token before matching, as
  // match will consume it
  auto question = match(TokenType::Question);
  if (!question)
    return condition;

  if (isAtEnd()) {
    createError("Expected an expr on the right of '?' character!");
    return nullptr;
  }
  auto ifThenExpr = parseExpr();
  if (ifThenExpr == nullptr)
    return nullptr;

  if (!match(TokenType::Colon)) {
    createError("Expected a ':' after if then expression!");
  }

  if (isAtEnd()) {
    createError("Expected an expr on the right of ':' character!");
    return nullptr;
  }

  auto elseExpr = parseConditionalExpr();
  if (elseExpr == nullptr)
    return nullptr;

  return std::make_unique<Faith::ConditionalExpr>(
      std::move(condition), question, std::move(ifThenExpr),
      std::move(elseExpr));
}

std::unique_ptr<Faith::Expr> FaithParser::parseLogicalOrExpr() {
  // Parse left expr
  std::unique_ptr<Faith::Expr> leftExpr = parseLogicalAndExpr();
  if (leftExpr == nullptr)
    return nullptr;

  // Match '||' token if match
  auto orToken = match(TokenType::LogicalOr);
  while (orToken) {
    // Check if any tokens available to consume, if not ret with error
    if (isAtEnd()) {
      createError("Expected an expr on the right of assignment operation!");
      return nullptr;
    }

    // Try parse the right expr
    auto rightExpr = parseLogicalAndExpr();
    if (rightExpr == nullptr)
      return nullptr;

    leftExpr = std::make_unique<Faith::BinaryExpr>(
        std::move(leftExpr), orToken, binOp::LogicalOr, std::move(rightExpr));

    // consume more '||' tokens
    orToken = match(TokenType::LogicalOr);
  }

  return leftExpr;
}

std::unique_ptr<Faith::Expr> FaithParser::parseLogicalAndExpr() {
  // Parse left expr
  std::unique_ptr<Faith::Expr> leftExpr = parseBitOrExpr();
  if (leftExpr == nullptr)
    return nullptr;

  // Match '&&' token if match
  auto andToken = match(TokenType::LogicalAnd);
  while (andToken) {
    // Check if any tokens available to consume, if not ret with error
    if (isAtEnd()) {
      createError("Expected an expr on the right of assignment operation!");
      return nullptr;
    }

    // Try parse the right expr
    auto rightExpr = parseBitOrExpr();
    if (rightExpr == nullptr)
      return nullptr;

    leftExpr = std::make_unique<Faith::BinaryExpr>(
        std::move(leftExpr), andToken, binOp::LogicalAnd, std::move(rightExpr));

    // consume more '&&' tokens
    andToken = match(TokenType::LogicalAnd);
  }

  return leftExpr;
}

std::unique_ptr<Faith::Expr> FaithParser::parseBitOrExpr() {
  // Parse left expr
  std::unique_ptr<Faith::Expr> leftExpr = parseBitXorExpr();
  if (leftExpr == nullptr)
    return nullptr;

  // Match '|' token if match
  auto token = match(TokenType::Pipe);
  while (token) {
    // Check if any tokens available to consume, if not ret with error
    if (isAtEnd()) {
      createError("Expected an expr on the right of assignment operation!");
      return nullptr;
    }

    // Try parse the right expr
    auto rightExpr = parseBitXorExpr();
    if (rightExpr == nullptr)
      return nullptr;

    leftExpr = std::make_unique<Faith::BinaryExpr>(
        std::move(leftExpr), token, binOp::BitwiseOr, std::move(rightExpr));

    // consume more '|' tokens
    token = match(TokenType::Pipe);
  }

  return leftExpr;
}

std::unique_ptr<Faith::Expr> FaithParser::parseBitXorExpr() {
  // Parse left expr
  std::unique_ptr<Faith::Expr> leftExpr = parseBitAndExpr();
  if (leftExpr == nullptr)
    return nullptr;

  // Match '^' token if match
  auto token = match(TokenType::Caret);
  while (token) {
    // Check if any tokens available to consume, if not ret with error
    if (isAtEnd()) {
      createError("Expected an expr on the right of assignment operation!");
      return nullptr;
    }

    // Try parse the right expr
    auto rightExpr = parseBitAndExpr();
    if (rightExpr == nullptr)
      return nullptr;

    leftExpr = std::make_unique<Faith::BinaryExpr>(
        std::move(leftExpr), token, binOp::BitwiseXor, std::move(rightExpr));

    // consume more '^' tokens
    token = match(TokenType::Caret);
  }

  return leftExpr;
}

std::unique_ptr<Faith::Expr> FaithParser::parseBitAndExpr() {
  // Parse left expr
  std::unique_ptr<Faith::Expr> leftExpr = parseEqualityExpr();
  if (leftExpr == nullptr)
    return nullptr;

  // Match '&' token if match
  auto token = match(TokenType::Ampersand);
  while (token) {
    // Check if any tokens available to consume, if not ret with error
    if (isAtEnd()) {
      createError("Expected an expr on the right of assignment operation!");
      return nullptr;
    }

    // Try parse the right expr
    auto rightExpr = parseEqualityExpr();
    if (rightExpr == nullptr)
      return nullptr;

    leftExpr = std::make_unique<Faith::BinaryExpr>(
        std::move(leftExpr), token, binOp::BitwiseAnd, std::move(rightExpr));

    // consume more '^' tokens
    token = match(TokenType::Ampersand);
  }

  return leftExpr;
}

std::unique_ptr<Faith::Expr> FaithParser::parseEqualityExpr() {
  // Parse left expr
  std::unique_ptr<Faith::Expr> leftExpr = parseRelationalExpr();

  while (peek().has_value() && leftExpr != nullptr) {
    switch (peek()->type) {
    case TokenType::EqualEqual:
      leftExpr =
          parseBinaryRHS(std::move(leftExpr), &FaithParser::parseRelationalExpr,
                         binOp::Equality);
      break;
    case TokenType::BangEqual:
      leftExpr =
          parseBinaryRHS(std::move(leftExpr), &FaithParser::parseRelationalExpr,
                         binOp::NotEqual);
      break;
    default:
      goto exit_while;
    }
  }

exit_while:
  return leftExpr;
}

std::unique_ptr<Faith::Expr> FaithParser::parseRelationalExpr() {
  // Parse left expr
  std::unique_ptr<Faith::Expr> leftExpr = parseShiftExpr();

  while (peek().has_value() && leftExpr != nullptr) {
    switch (peek()->type) {
    case TokenType::Less:
      leftExpr = parseBinaryRHS(std::move(leftExpr),
                                &FaithParser::parseShiftExpr, binOp::Less);
      break;
    case TokenType::LessEqual:
      leftExpr = parseBinaryRHS(std::move(leftExpr),
                                &FaithParser::parseShiftExpr, binOp::LessEqual);
      break;
    case TokenType::Greater:
      leftExpr = parseBinaryRHS(std::move(leftExpr),
                                &FaithParser::parseShiftExpr, binOp::Greater);
      break;
    case TokenType::GreaterEqual:
      leftExpr =
          parseBinaryRHS(std::move(leftExpr), &FaithParser::parseShiftExpr,
                         binOp::GreaterEqual);
      break;
    default:
      goto exit_while;
    }
  }

exit_while:
  return leftExpr;
}

std::unique_ptr<Faith::Expr> FaithParser::parseShiftExpr() {
  // Parse left expr
  std::unique_ptr<Faith::Expr> leftExpr = parseAddExpr();

  while (peek().has_value() && leftExpr != nullptr) {
    switch (peek()->type) {
    case TokenType::ShiftLeft:
      leftExpr = parseBinaryRHS(std::move(leftExpr), &FaithParser::parseAddExpr,
                                binOp::ShiftLeft);
      break;
    case TokenType::ShiftRight:
      leftExpr = parseBinaryRHS(std::move(leftExpr), &FaithParser::parseAddExpr,
                                binOp::ShiftRight);
      break;
    default:
      goto exit_while;
    }
  }

exit_while:
  return leftExpr;
}

std::unique_ptr<Faith::Expr> FaithParser::parseAddExpr() {
  // Parse left expr
  std::unique_ptr<Faith::Expr> leftExpr = parseMultExpr();

  while (peek().has_value() && leftExpr != nullptr) {
    switch (peek()->type) {
    case TokenType::Plus:
      leftExpr = parseBinaryRHS(std::move(leftExpr),
                                &FaithParser::parseMultExpr, binOp::Add);
      break;
    case TokenType::Minus:
      leftExpr = parseBinaryRHS(std::move(leftExpr),
                                &FaithParser::parseMultExpr, binOp::Subtract);
      break;
    default:
      goto exit_while;
    }
  }

exit_while:
  return leftExpr;
}

std::unique_ptr<Faith::Expr> FaithParser::parseMultExpr() {
  // Parse left expr
  std::unique_ptr<Faith::Expr> leftExpr = parseCastExpr();

  while (peek().has_value() && leftExpr != nullptr) {
    switch (peek()->type) {
    case TokenType::Star:
      leftExpr = parseBinaryRHS(std::move(leftExpr),
                                &FaithParser::parseCastExpr, binOp::Multiply);
      break;
    case TokenType::Slash:
      leftExpr = parseBinaryRHS(std::move(leftExpr),
                                &FaithParser::parseCastExpr, binOp::Divide);
      break;
    case TokenType::Percent:
      leftExpr = parseBinaryRHS(std::move(leftExpr),
                                &FaithParser::parseCastExpr, binOp::Modulo);
      break;
    default:
      goto exit_while;
    }
  }

exit_while:
  return leftExpr;
}

std::unique_ptr<Faith::Expr> FaithParser::parseCastExpr() {
  token_view castKw = match(TokenType::Kw_Cast);

  // If cast keyword not present then
  // the expression is a unary expr
  if (!castKw)
    return parseUnaryExpr();

  // Valid cast expression, now parse it
  if (!match(TokenType::Less)) {
    createError("Expected '<' after cast keyword!");
    return nullptr;
  }

  if (isAtEnd()) {
    createError("Expected a type for the cast expression!");
    return nullptr;
  }

  // Parse the toType of the cast_expr
  auto toType = parseTypeSpec();
  if (toType == nullptr)
    return nullptr;

  if (!match(TokenType::Greater)) {
    createError("Expected '>' after type!");
    return nullptr;
  }

  if (!match(TokenType::LeftParen)) {
    createError("Expected '(' after <type>!");
    return nullptr;
  }

  if (isAtEnd()) {
    createError("Expected an expression after '(' but got end of file!");
    return nullptr;
  }

  // Parse expression that needs to be casted
  auto expr = parseExpr();
  if (expr == nullptr)
    return nullptr;

  // Match for ')' if not return null
  if (!match(TokenType::RightParen)) {
    createError("Expected a ')' after expression!");
    return nullptr;
  }

  return std::make_unique<Faith::CastExpr>(castKw, std::move(toType),
                                           std::move(expr));
}

std::unique_ptr<Faith::Expr> FaithParser::parseUnaryExpr() {
  token_view curr = peek();
  Faith::UnaryOp uOp;

  switch (curr->type) {
  case TokenType::Plus:
    advance();
    uOp = Faith::UnaryOp::Plus;
    break;
  case TokenType::Minus:
    advance();
    uOp = Faith::UnaryOp::Minus;
    break;
  case TokenType::Bang:
    advance();
    uOp = Faith::UnaryOp::LogicalNot;
    break;
  case TokenType::Star:
    advance();
    uOp = Faith::UnaryOp::Dereference;
    break;
  case TokenType::Ampersand:
    advance();
    uOp = Faith::UnaryOp::AddressOf;
    break;
  case TokenType::PlusPlus:
    advance();
    uOp = Faith::UnaryOp::PrefixInc;
    break;
  case TokenType::MinusMinus:
    advance();
    uOp = Faith::UnaryOp::PrefixDec;
    break;

  // if not all these then its a postfix expr
  default:
    return parsePostfixExpr();
  }

  // Recursively parse Unary expression
  auto uExpr = parseUnaryExpr();
  if (uExpr == nullptr)
    return nullptr;

  return std::make_unique<Faith::UnaryExpr>(curr, uOp, std::move(uExpr));
}

std::unique_ptr<Faith::Expr> FaithParser::parsePostfixExpr() {
  // First, parse the "base" of the expression (a primary expression).
  auto expr = parsePrimaryExpr();
  if (!expr) {
    return nullptr;
  }

  // Loop continuously to parse any chained postfix operations.
  while (peek().has_value()) {
    token_view curr = peek();
    switch (curr->type) {
    // Member access: expr.member
    case TokenType::Dot: {
      advance();
      token_view member = match(TokenType::Identifier);
      if (!member) {
        createError("Expected member name after '.'!");
        return nullptr;
      }
      expr = std::make_unique<Faith::MemberAccessExpr>(std::move(expr), curr,
                                                       member);
      break;
    }
    // Pointer member access: expr->member
    case TokenType::Arrow: {
      advance();
      token_view member = match(TokenType::Identifier);
      if (!member) {
        createError("Expected member name after '->'.");
        return nullptr;
      }
      expr = std::make_unique<Faith::MemberAccessExpr>(std::move(expr), curr,
                                                       member);
      break;
    }
    // Function call: expr(args...)
    case TokenType::LeftParen: {
      advance();
      std::unique_ptr<Faith::argList> args = nullptr;

      if (peek().has_value() && peek()->type != TokenType::RightParen)
        args = parseArgList();

      if (!match(TokenType::RightParen)) {
        createError("Expected ')' after function arguments!");
        return nullptr;
      }
      expr = std::make_unique<Faith::CallExpr>(std::move(expr), std::move(args),
                                               curr);
      break;
    }
    // Array index: expr[index]
    case TokenType::LeftBracket: {
      advance();
      if (isAtEnd()) {
        createError("Expected a index after '[' but got end of file!");
        return nullptr;
      }

      auto indexExpr = parseExpr(); // Parse the inner expression
      if (!indexExpr)
        return nullptr;

      if (!match(TokenType::RightBracket)) {
        createError("Expected ']' after array index.");
        return nullptr;
      }

      expr = std::make_unique<Faith::IndexAccessExpr>(
          std::move(expr), std::move(indexExpr), curr);
      break;
    }
    // Postfix increment: expr++
    case TokenType::PlusPlus: {
      advance();
      expr =
          std::make_unique<Faith::PostfixStepExpr>(std::move(expr), curr, true);
      break;
    }
    // Postfix decrement: expr--
    case TokenType::MinusMinus: {
      advance();
      expr = std::make_unique<Faith::PostfixStepExpr>(std::move(expr), curr,
                                                      false);
      break;
    }
    // No more postfix operators, return the fully formed expression.
    default:
      return expr;
    }
  }

  return expr;
}

std::unique_ptr<Faith::Expr> FaithParser::parsePrimaryExpr() {
  token_view tok = advance();

  using ilFormat = Faith::IntLiteralFormat;
  switch (tok->type) {
  case TokenType::LeftParen: {
    if (isAtEnd()) {
      createError("Expected an expression after '(' got end of file!");
      return nullptr;
    }

    auto expr = parseExpr();
    if (expr == nullptr)
      return nullptr;

    if (!match(TokenType::RightParen)) {
      createError("Expected ')' after expression!");
      return nullptr;
    }

    return std::make_unique<Faith::GroupedExpr>(std::move(expr));
  }
  case TokenType::Identifier: {
    // We found an identifier, 'tok'
    // Check if it's followed by a '{'
    if (peek().has_value() && peek()->type == TokenType::LeftBrace) {
      // Unconsume the struct ident consumed earlier
      m_current--;
      return parseStructInit();
    }

    // If it's not a struct init (no '{' or it's a code block),
    // then it's just a plain variable/identifier.
    return std::make_unique<Faith::Identifier>(tok);
  } break;
  case TokenType::StringLiteral:
    return std::make_unique<Faith::StringLiteral>(tok);
  case TokenType::CharLiteral:
    return std::make_unique<Faith::CharLiteral>(tok);
  case TokenType::BinaryLiteral:
    return std::make_unique<Faith::IntLiteral>(tok, ilFormat::Binary);
  case TokenType::IntLiteral:
    return std::make_unique<Faith::IntLiteral>(tok, ilFormat::Normal);
  case TokenType::HexLiteral:
    return std::make_unique<Faith::IntLiteral>(tok, ilFormat::Hex);
  case TokenType::FloatLiteral:
    return std::make_unique<Faith::FloatLiteral>(tok);
  case TokenType::Kw_True:
    return std::make_unique<Faith::BoolLiteral>(tok, true);
  case TokenType::Kw_False:
    return std::make_unique<Faith::BoolLiteral>(tok, false);
  case TokenType::Kw_Null:
    return std::make_unique<Faith::NullLiteral>(tok);
  default:
    break;
  }

  createError("Invalid expression found!");
  return nullptr;
}

std::unique_ptr<Faith::argList> FaithParser::parseArgList() {
  auto argList = std::make_unique<Faith::argList>();

  auto arg0 = parseExpr();
  if (arg0 == nullptr)
    return nullptr;
  argList->push_back(std::move(arg0));

  while (match(TokenType::Comma)) {
    auto argN = parseExpr();
    if (argN == nullptr)
      return nullptr;

    argList->push_back(std::move(argN));
  }

  return std::move(argList);
}

std::unique_ptr<Faith::StructInit> FaithParser::parseStructInit() {
  token_view structName = advance();

  if (!match(TokenType::LeftBrace)) {
    createError("Expected a '{' after struct name!");
    return nullptr;
  }

  std::unique_ptr<Faith::structInitFieldList> structInitList = nullptr;

  if (!isAtEnd() && peek()->type != TokenType::RightBrace) {
    structInitList = parseStructInitFieldList();
    if (structInitList == nullptr)
      return nullptr;
  }

  if (!match(TokenType::RightBrace)) {
    createError("Expected a '}' after struct init field list!");
    return nullptr;
  }
  return std::make_unique<Faith::StructInit>(structName,
                                             std::move(structInitList));
}

std::unique_ptr<Faith::structInitFieldList>
FaithParser::parseStructInitFieldList() {
  auto sInitFieldlist = std::make_unique<Faith::structInitFieldList>();

  auto field0 = parseStructInitField();
  if (field0 == nullptr)
    return nullptr;
  sInitFieldlist->push_back(std::move(field0));

  while (match(TokenType::Comma)) {
    auto fieldN = parseStructInitField();
    if (fieldN == nullptr)
      return nullptr;
    sInitFieldlist->push_back(std::move(fieldN));
  }

  return std::move(sInitFieldlist);
}

std::unique_ptr<Faith::StructInitField> FaithParser::parseStructInitField() {
  auto memberName = match(TokenType::Identifier);
  if (!memberName) {
    createError("Expected a member name after '{' in struct initializer.!");
    return nullptr;
  }

  if (!match(TokenType::Colon)) {
    createError("Expected a ':' after member name!");
    return nullptr;
  }

  if (isAtEnd()) {
    createError("Expected a init expression but got end of file!");
    return nullptr;
  }

  auto initExpr = parseConditionalExpr();
  if (initExpr == nullptr)
    return nullptr;

  return std::make_unique<Faith::StructInitField>(memberName,
                                                  std::move(initExpr));
}