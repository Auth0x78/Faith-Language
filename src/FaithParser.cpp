#include "FaithParser.h"

using token_view = Faith::TokenView;
using tType = TokenType;
using linkage = Faith::LinkageSpecifier;
using primTy = Faith::PrimitiveTypeKind;

#pragma region Helper & Constructor Functions

FaithParser::FaithParser(const std::vector<Token> &tokens)
    : m_current(0), m_tokens(tokens), m_tokenArrSize(m_tokens.size()),
      m_program(std::make_unique<Faith::Program>()), m_errorStack(),
      m_errNum(0) {}

uint32_t FaithParser::createError(Faith::TokenView loc,
                                  std::string &&errMessage) {
  parseError errorStruct = m_errorStack.emplace();

  errorStruct.errorLoc = loc;
  errorStruct.message = errMessage;

  return m_errNum++;
}

uint32_t FaithParser::createError(std::string &&errMessage) {
  m_errorStack.emplace().message = errMessage;
  return m_errNum++;
}

void FaithParser::printErrors() {
  // TODO: Implement printing of error to console
  throw std::runtime_error("NYI: Print Errors!");
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
      // TODO: Error Recovery
    }
  }

  if (m_errNum != 0) {
    createError("Failed to compile program!");
  }

  return std::move(m_program);
}

std::unique_ptr<Faith::Decl> FaithParser::parseDecl() {
  token_view current = peek();
  std::unique_ptr<Faith::Decl> decl;

  switch (current->type) {
  // Guareented to be just declaration
  case tType::Kw_Extern:
    return parseFuncDecl(linkage::EXTERN);
  // Parse function decl or def
  case tType::Kw_Static:
    return parseFuncDecl(linkage::STATIC);
  case tType::Kw_Func:
    return parseFuncDecl(linkage::NONE);
    break;
  case tType::Kw_Let:
    return parseVarDecl(false, true);
  case tType::Kw_Const:
    return parseVarDecl(true, true);
    // Parse global variable decl
  case tType::Kw_Struct:
    // Parse both struct decl as well as forward decl
    return parseStructDecl();
  case tType::Kw_Typealias:
    // Type alias declaration
    return parseTypeAliasDecl();
  default:
    break;
  }

  // Default error exit
  createError("Invalid declaration starting with token: " +
              std::string(current->token));
  return nullptr;
}

std::unique_ptr<Faith::FuncDecl>
FaithParser::parseFuncDecl(Faith::LinkageSpecifier linkage) {
  return std::unique_ptr<Faith::FuncDecl>();
}

std::unique_ptr<Faith::VarDecl> FaithParser::parseVarDecl(bool isConst,
                                                          bool isGlobal) {
  return std::unique_ptr<Faith::VarDecl>();
}

std::unique_ptr<Faith::StructDecl> FaithParser::parseStructDecl() {
  return std::unique_ptr<Faith::StructDecl>();
}

std::unique_ptr<Faith::TypealiasDecl> FaithParser::parseTypeAliasDecl() {
  // consume the initial typealias token
  auto typeAliasTok = advance();

  auto aliasType = match(tType::Identifier);
  if (!aliasType) {
    // Corrected error message to be specific
    createError(typeAliasTok,
                "Expected identifier for type alias name after 'typealias'.");
    return nullptr;
  }

  auto equalTok = match(tType::Equal);
  if (!equalTok) {
    createError(typeAliasTok, "Expected '=' after type alias name.");
    return nullptr;
  }

  // Parse the type specification that the alias refers to
  if (isAtEnd()) {
    createError(equalTok, "Expected a type for target type after '='");
    return nullptr;
  }
  auto targetType = parseTypeSpec();
  if (!targetType)
    return nullptr;

  // Expect the terminating semicolon ';'
  if (!match(tType::Semicolon)) {
    createError(targetType->locToken,
                "Expected ';' after type alias definition.");
    return nullptr;
  }

  // Return the constructed AST node
  return std::make_unique<Faith::TypealiasDecl>(aliasType,
                                                std::move(targetType));
}

std::unique_ptr<Faith::TypeSpec> FaithParser::parseTypeSpec() {

  auto locToken = peek();
  auto bang = match(tType::Bang);

  if (isAtEnd()) {
    createError(bang, "Expected a type after '!'");
    return nullptr;
  }
  // Parse base type, if tokens are avail.
  auto base = parseBaseType();

  int arrDim = 0;

  token_view rightBrack = match(tType::RightBracket);

  while (rightBrack) {
    if (!match(tType::LeftBracket)) {
      // If ']' is missing, this is a syntax error.
      createError("Expected ']' to close array type suffix.");

      return nullptr;
    }

    arrDim++;
    rightBrack = match(tType::RightBracket);
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
  case TokenType::Identifier:
    return parseStructType();

  // Parse base type recursively
  case TokenType::Star: {
    // Distinguish between ptr to base type and func ptr
    auto lookupFunc = peekNext();
    if (lookupFunc.has_value() && lookupFunc->type == tType::Kw_Func)
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
  if (!match(tType::Kw_Func)) {
    // This should not happen if the caller matched '*' and then checked the
    // next token, but included for robust error handling.
    createError(starTok, "Expected 'func' keyword after pointer operator '*'.");
    return nullptr;
  }

  token_view funcTok = advance(); // Save the consumed func token for location

  // Expect and consume the opening parenthesis '('
  token_view leftParen = match(tType::LeftParen);
  if (!leftParen) {
    createError(funcTok,
                "Expected a '(' after 'func' but reached end of file.");
    return nullptr;
  }

  // Check if we can proceed ahead
  if (isAtEnd()) {
    createError(leftParen, "Expected a ')' or params type list after '(' but "
                           "reached end of file.");
    return nullptr;
  }

  // Check if the parameter list is empty (i.e., we see ')')

  std::unique_ptr<Faith::paramsTypeVec> paramsTy = nullptr;
  if (peek()->type != tType::RightParen) {
    // If not ')' then it must be start of list
    paramsTy = std::move(parseParamTypeList());

    // An error occured while parsing types param
    if (paramsTy == nullptr)
      return nullptr;
  }

  token_view rightParen = match(tType::RightParen);
  if (!rightParen) {
    createError(leftParen, "Expected a ')' at the end!");
    return nullptr;
  }

  token_view arrow = match(tType::Arrow);
  if (!arrow) {
    createError(rightParen, "Expected a '->' after ')'!");
    return nullptr;
  }

  if (isAtEnd()) {
    createError(arrow,
                "Expected a return type after '->' but got end of file!");
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
  if(type0 == nullptr)
    return nullptr;
  paramsTyList->push_back(std::move(type0));

  // Parse the all the "," <type> till we either reach end
  // Or we till we dont have any more "," to match
  while(match(tType::Comma)) {
    auto typeN = parseTypeSpec();
    
    if(typeN == nullptr) // Todo: Error recovery till next sync token
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
