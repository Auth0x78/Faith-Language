#include "FaithParser.h"

#define NYI(msg) throw std::runtime_error(msg)

using token_view = Faith::TokenView;
using primTy = Faith::PrimitiveTypeKind;

#pragma region Helper & Constructor Functions

FaithParser::FaithParser(const std::vector<Token> &tokens)
    : m_current(0), m_tokens(tokens), m_tokenArrSize(m_tokens.size()),
      m_program(std::make_unique<Faith::Program>()), m_errorStack(),
      m_errNum(0) {}

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
  // TODO: Implement printing of error to console
  NYI("NYI: Print Errors!");
};

Faith::TokenView FaithParser::match(TokenType type) {
  if (isAtEnd() || m_tokens[m_current].type != type)
    return nullptr;

  return advance();
}

Faith::TokenView FaithParser::advance() {
  const Token *addr = &m_tokens[m_current];
  m_current++;

  return Faith::TokenView(addr);
}

Faith::TokenView FaithParser::peekNext() {
  if (m_current + 1 >= m_tokenArrSize)
    return nullptr;

  return Faith::TokenView(&m_tokens[m_current + 1]);
}

Faith::TokenView FaithParser::peekPrev() {
  return Faith::TokenView(&m_tokens[m_current - 1]);
}

Faith::TokenView FaithParser::peek() {
  return Faith::TokenView(&m_tokens[m_current]);
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
      // TODO: Implement error recovery via sync tokens
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
    advance();
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
  // consume the extern token
  token_view staticTok = advance();

  // False => Tell parse func decl to only parse func declarations and if
  // present then defination (decl followed by compound body)
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
    if (paramsVec == nullptr)
      return nullptr;
    else {
      // TODO: Error Recovery
    }
  }

  if (!match(TokenType::RightParen)) {
    createError("Expected a ')' at the end of the function decl!");
    return nullptr;
  }

  std::unique_ptr<Faith::TypeSpec> retType = std::make_unique<Faith::TypeSpec>(
      nullptr, false,
      std::make_unique<Faith::PrimitiveType>(primTy::VOID, nullptr), 0);

  if (match(TokenType::Arrow)) {
    retType = parseTypeSpec();

    if (retType == nullptr)
      return nullptr;
    else {
      // TODO: Error recovery
    }
  }

  return std::make_unique<Faith::FuncDecl>(funcTok, ident, std::move(paramsVec),
                                           std::move(retType));
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

    if (paramN == nullptr) // TODO: Error recovery till next sync token
      return nullptr;

    paramlist->push_back(std::move(paramN));
  }

  return paramlist;
}

std::unique_ptr<Faith::Param> FaithParser::parseParam() {
  auto ident = match(TokenType::Identifier);
  if (!ident) {
    createError("Expected a ident name for the param!");
    return nullptr;
  }

  if (!match(TokenType::Colon)) {
    createError("Expected a ':' after param name!");
    return nullptr;
  }

  // Now a <type> should be present
  if (isAtEnd()) {
    createError("Expected a type for the parameter but got end of file!");
    return nullptr;
  }

  auto type = parseTypeSpec();

  return std::make_unique<Faith::Param>(ident, std::move(type));
}

std::unique_ptr<Faith::VarDecl> FaithParser::parseVarDecl(bool isConst,
                                                          bool isGlobal) {
  // TODO: Implement parsing of var decl
  NYI("NYI: Parse Variable Declaration!");
  return std::unique_ptr<Faith::VarDecl>();
}

std::unique_ptr<Faith::StructDecl> FaithParser::parseStructDecl() {
  // TODO: Implement parsing of struct decl
  NYI("NYI: Parse Struct Declaration!");
  return std::unique_ptr<Faith::StructDecl>();
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

std::unique_ptr<Faith::TypeSpec> FaithParser::parseTypeSpec() {

  auto locToken = peek();
  auto bang = match(TokenType::Bang);

  if (isAtEnd()) {
    createError("Expected a type after '!'");
    return nullptr;
  }
  // Parse base type, if tokens are avail.
  auto base = parseBaseType();

  int arrDim = 0;

  token_view rightBrack = match(TokenType::RightBracket);

  while (rightBrack) {
    if (!match(TokenType::LeftBracket)) {
      // If ']' is missing, this is a syntax error.
      createError("Expected ']' to close array type suffix.");

      return nullptr;
    }

    arrDim++;
    rightBrack = match(TokenType::RightBracket);
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
    return parsePrimitiveType(primTy::F64);
  case TokenType::Kw_Bool:
    return parsePrimitiveType(primTy::CHAR);
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
  if (!match(TokenType::Kw_Func)) {
    // This should not happen if the caller matched '*' and then checked the
    // next token, but included for robust error handling.
    createError("Expected 'func' keyword after pointer operator '*'.");
    return nullptr;
  }

  token_view funcTok = advance(); // Save the consumed func token for location

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
    paramsTy = std::move(parseParamTypeList());

    // An error occured while parsing types param
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

  // Parse the base type that the pointer points to (e.g., 'int' in 'int*').
  //    This is the recursive step.
  std::unique_ptr<Faith::BaseType> baseType = parseBaseType();

  // Propagate error upwards
  if (baseType != nullptr)
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
  if (baseType != nullptr)
    return nullptr;

  return std::make_unique<Faith::RefType>(ampTok, std::move(baseType));
}
