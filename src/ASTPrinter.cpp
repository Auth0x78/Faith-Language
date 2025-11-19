#include "ASTPrinter.h"

#include <iostream>
#include <utility>

using namespace Faith;

long long ASTPrinter::nextNodeId = 1;

ASTPrinter::ASTPrinter(bool enableColor)
    : tempString(), colorEnabled(enableColor), C_NODE(nullptr), C_DECL(nullptr),
      C_TYPE(nullptr), C_STMT(nullptr), C_EXPR(nullptr), C_PATTERN(nullptr),
      C_IDENT(nullptr), C_LITERAL(nullptr), C_KEY(nullptr), RESET(nullptr) {
  if (colorEnabled)
    enableColors();
  else
    disableColors();
  indentStack.reserve(32);
}

// -------------------- Colors --------------------
void ASTPrinter::enableColors() {
  C_NODE = "\033[38;2;235;111;146m";
  C_DECL = "\033[38;2;246;193;119m";
  C_TYPE = "\033[38;2;156;207;216m";
  C_STMT = "\033[38;2;86;148;159m";
  C_EXPR = "\033[38;2;224;175;104m";
  C_PATTERN = "\033[38;2;235;188;186m";
  C_IDENT = "\033[38;2;245;169;127m";
  C_LITERAL = "\033[38;2;150;205;197m";
  C_KEY = "\033[38;2;163;190;140m";
  C_PARAM = "\033[38;2;223;142;142m";
  RESET = "\033[0m";
}

void ASTPrinter::disableColors() {
  C_NODE = C_DECL = C_TYPE = C_STMT = C_EXPR = C_PATTERN = C_IDENT = C_LITERAL =
      C_KEY = C_PARAM = RESET = "";
}

// -------------------- Printing helpers --------------------
void ASTPrinter::print(const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);
}

void ASTPrinter::printIndent() {
  for (auto &s : indentStack)
    fputs(s.c_str(), stdout);
}

void ASTPrinter::indent() { indentStack.push_back("   "); }

void ASTPrinter::dedent() {
  if (!indentStack.empty())
    indentStack.pop_back();
}

long long ASTPrinter::assignId(Node *node) {
  auto it = nodeIds.find(node);
  if (it != nodeIds.end())
    return it->second;
  long long id = nextNodeId++;
  nodeIds[node] = id;
  return id;
}

// Convert TokenView (const Token*) with std::string_view to safe c-string.
// tempString is overwritten each call — use its result immediately.
const char *ASTPrinter::safeToken(const TokenView &tv) {
  if (!tv)
    return "<unnamed>";
  // Token::lexeme is std::string_view — convert to std::string
  tempString = std::string(tv->token);
  return tempString.c_str();
}

// -------------------- Entry point --------------------
void ASTPrinter::print(Program *program) {
  printIndent();
  print("%sProgram%s (id=%lld)\n", C_NODE, RESET, assignId(program));
  indent();
  program->accept(*this);
  dedent();
}

// -------------------- Visitors --------------------

// Program
void ASTPrinter::visit(Program *node) {
  if (!node)
    return;
  for (auto &decl : node->declarations) {
    printIndent();
    // each declaration prints its own header
    decl->accept(*this);
  }
}

// Declarations
void ASTPrinter::visit(FuncDecl *node) {
  printIndent();
  print("%sFuncDecl%s (id=%lld, name=%s)\n", C_DECL, RESET, assignId(node),
        node->name ? safeToken(node->name) : "<unnamed>");
  indent();

  if (node->params) {
    printIndent();
    print("%sparams:%s\n", C_KEY, RESET);
    indent();
    for (auto &p : *node->params) {
      p->accept(*this);
    }
    dedent();
  }

  if (node->returnType) {
    printIndent();
    print("%sreturnType:%s\n", C_KEY, RESET);
    indent();
    node->returnType->accept(*this);
    dedent();
  }

  dedent();
}

void ASTPrinter::visit(FuncDef *node) {
  printIndent();
  print("%sFuncDef%s (id=%lld, name=%s)\n", C_DECL, RESET, assignId(node),
        node->name ? safeToken(node->name) : "<unnamed>");
  indent();

  if (node->params) {
    printIndent();
    print("%sparams:%s\n", C_KEY, RESET);
    indent();
    for (auto &p : *node->params)
      p->accept(*this);
    dedent();
  }

  if (node->returnType) {
    printIndent();
    print("%sreturnType:%s\n", C_KEY, RESET);
    indent();
    node->returnType->accept(*this);
    dedent();
  }

  printIndent();
  print("%sbody:%s\n", C_KEY, RESET);
  indent();
  node->body->accept(*this);
  dedent();

  dedent();
}

void ASTPrinter::visit(StaticDef *node) {
  printIndent();
  print("%sStaticDef%s (id=%lld)\n", C_DECL, RESET, assignId(node));
  indent();
  node->funcDef->accept(*this);
  dedent();
}

void ASTPrinter::visit(ExternDecl *node) {
  printIndent();
  print("%sExternDecl%s (id=%lld, name=%s)\n", C_DECL, RESET, assignId(node),
        node->funcDecl && node->funcDecl->name ? safeToken(node->funcDecl->name)
                                               : "<unnamed>");
  indent();
  if (node->StringLiteral) {
    // StringLiteral stored in Token::lexeme (string_view)
    printIndent();
    print("link name: %s\n", safeToken(node->StringLiteral));
  }
  node->funcDecl->accept(*this);
  dedent();
}

void ASTPrinter::visit(TypealiasDecl *node) {
  printIndent();
  print("%sTypeAlias%s (id=%lld, name=%s)\n", C_TYPE, RESET, assignId(node),
        node->name ? safeToken(node->name) : "<unnamed>");
  indent();
  node->aliasedType->accept(*this);
  dedent();
}

void ASTPrinter::visit(StructDecl *node) {
  printIndent();
  print("%sStructDecl%s (id=%lld, name=%s)\n", C_TYPE, RESET, assignId(node),
        node->structName ? safeToken(node->structName) : "<unnamed>");
  indent();

  if (node->body) {
    printIndent();
    print("%sfields:%s\n", C_KEY, RESET);
    indent();
    for (auto &f : *node->body) {
      f->accept(*this);
    }
    dedent();
  }

  dedent();
}

void ASTPrinter::visit(StructField *node) {
  printIndent();
  print("%sStructField%s (id=%lld, name=%s)\n", C_TYPE, RESET, assignId(node),
        node->name ? safeToken(node->name) : "<unnamed>");
  indent();

  printIndent();
  print("type:\n");
  indent();
  node->type->accept(*this);
  dedent();

  if (node->defaultValue) {
    printIndent();
    print("default:\n");
    indent();
    node->defaultValue->accept(*this);
    dedent();
  }

  dedent();
}

void ASTPrinter::visit(Param *node) {
  printIndent();
  print("%sParam%s (id=%lld, name=%s)\n", C_PARAM, RESET, assignId(node),
        node->name ? safeToken(node->name) : "<unnamed>");
  indent();
  node->type->accept(*this);
  dedent();
}

void ASTPrinter::visit(VarDecl *node) {
  printIndent();
  print("%sVarDecl%s (id=%lld, name=%s, %s)\n", C_DECL, RESET, assignId(node),
        node->name ? safeToken(node->name) : "<unnamed>",
        node->isConst ? "const" : "let");
  indent();

  printIndent();
  print("type:\n");
  indent();
  node->type->accept(*this);
  dedent();

  if (node->initializer) {
    printIndent();
    print("initializer:\n");
    indent();
    node->initializer->accept(*this);
    dedent();
  }

  dedent();
}

// Types
void ASTPrinter::visit(TypeSpec *node) {
  printIndent();
  print("%sTypeSpec%s (id=%lld, dims=%u)\n", C_TYPE, RESET, assignId(node),
        node->arrayDimensions);
  indent();
  if (node->baseTy)
    node->baseTy->accept(*this);
  dedent();
}

void ASTPrinter::visit(PrimitiveType *node) {
  printIndent();
  print("%sPrimitiveType%s (id=%lld, kind=(%d, %s))\n", C_TYPE, RESET,
        assignId(node), (int)node->kind, safeToken(node->typeToken));
}

void ASTPrinter::visit(StructType *node) {
  printIndent();
  print("%sStructType%s (id=%lld, name=%s)\n", C_TYPE, RESET, assignId(node),
        node->name ? safeToken(node->name) : "<unnamed>");
}

void ASTPrinter::visit(FuncPtrType *node) {
  printIndent();
  print("%sFuncPtrType%s (id=%lld)\n", C_TYPE, RESET, assignId(node));
  indent();
  if (node->paramsTy) {
    for (auto &t : *node->paramsTy)
      t->accept(*this);
  }
  printIndent();
  print("return:\n");
  indent();
  node->returnType->accept(*this);
  dedent();
  dedent();
}

void ASTPrinter::visit(PtrType *node) {
  printIndent();
  print("%sPtrType%s (id=%lld)\n", C_TYPE, RESET, assignId(node));
  indent();
  node->pointedType->accept(*this);
  dedent();
}

void ASTPrinter::visit(RefType *node) {
  printIndent();
  print("%sRefType%s (id=%lld)\n", C_TYPE, RESET, assignId(node));
  indent();
  node->referencedType->accept(*this);
  dedent();
}

// Statements
void ASTPrinter::visit(CompoundStmt *node) {
  printIndent();
  print("%sCompoundStmt%s (id=%lld)\n", C_STMT, RESET, assignId(node));
  indent();
  if (node->statements) {
    for (auto &s : *node->statements)
      s->accept(*this);
  }
  dedent();
}

void ASTPrinter::visit(ExprStmt *node) {
  printIndent();
  print("%sExprStmt%s (id=%lld)\n", C_STMT, RESET, assignId(node));
  indent();
  node->expression->accept(*this);
  dedent();
}

void ASTPrinter::visit(IfStmt *node) {
  printIndent();
  print("%sIfStmt%s (id=%lld)\n", C_STMT, RESET, assignId(node));
  indent();

  printIndent();
  print("condition:\n");
  indent();
  node->condition->accept(*this);
  dedent();

  printIndent();
  print("then:\n");
  indent();
  node->thenBranch->accept(*this);
  dedent();

  if (node->elseBranch) {
    printIndent();
    print("else:\n");
    indent();
    node->elseBranch->accept(*this);
    dedent();
  }

  dedent();
}

void ASTPrinter::visit(WhileStmt *node) {
  printIndent();
  print("%sWhileStmt%s (id=%lld)\n", C_STMT, RESET, assignId(node));
  indent();

  printIndent();
  print("condition:\n");
  indent();
  node->condition->accept(*this);
  dedent();

  printIndent();
  print("body:\n");
  indent();
  node->body->accept(*this);
  dedent();

  dedent();
}

void ASTPrinter::visit(ForStmt *node) {
  printIndent();
  print("%sForStmt%s (id=%lld)\n", C_STMT, RESET, assignId(node));
  indent();

  if (node->initializer.has_value()) {
    printIndent();
    print("init:\n");
    indent();
    if (std::holds_alternative<UPtr<VarDecl>>(node->initializer.value())) {
      std::get<UPtr<VarDecl>>(node->initializer.value())->accept(*this);
    } else {
      std::get<UPtr<Expr>>(node->initializer.value())->accept(*this);
    }
    dedent();
  }

  if (node->condition) {
    printIndent();
    print("cond:\n");
    indent();
    node->condition->accept(*this);
    dedent();
  }

  if (node->update) {
    printIndent();
    print("update:\n");
    indent();
    node->update->accept(*this);
    dedent();
  }

  printIndent();
  print("body:\n");
  indent();
  node->body->accept(*this);
  dedent();

  dedent();
}

void ASTPrinter::visit(ReturnStmt *node) {
  printIndent();
  print("%sReturnStmt%s (id=%lld)\n", C_STMT, RESET, assignId(node));
  if (node->value) {
    indent();
    node->value->accept(*this);
    dedent();
  }
}

void ASTPrinter::visit(DeferStmt *node) {
  printIndent();
  print("%sDeferStmt%s (id=%lld)\n", C_STMT, RESET, assignId(node));
  indent();
  node->deferExpr->accept(*this);
  dedent();
}

void ASTPrinter::visit(SwitchStmt *node) {
  printIndent();
  print("%sSwitchStmt%s (id=%lld)\n", C_STMT, RESET, assignId(node));
  indent();

  printIndent();
  print("expr:\n");
  indent();
  node->expression->accept(*this);
  dedent();

  printIndent();
  print("cases:\n");
  indent();
  for (auto &c : node->cases)
    c->accept(*this);
  dedent();

  if (node->defaultBody) {
    printIndent();
    print("default:\n");
    indent();
    node->defaultBody->accept(*this);
    dedent();
  }

  dedent();
}

void ASTPrinter::visit(CaseBlock *node) {
  printIndent();
  print("%sCaseBlock%s (id=%lld)\n", C_STMT, RESET, assignId(node));
  indent();
  node->value->accept(*this);
  node->body->accept(*this);
  dedent();
}

void ASTPrinter::visit(MatchStmt *node) {
  printIndent();
  print("%sMatchStmt%s (id=%lld)\n", C_STMT, RESET, assignId(node));
  indent();

  printIndent();
  print("expr:\n");
  indent();
  node->expression->accept(*this);
  dedent();

  printIndent();
  print("arms:\n");
  indent();
  for (auto &a : node->arms)
    a->accept(*this);
  dedent();

  dedent();
}

void ASTPrinter::visit(MatchArm *node) {
  printIndent();
  print("%sMatchArm%s (id=%lld)\n", C_STMT, RESET, assignId(node));
  indent();

  printIndent();
  print("patterns:\n");
  indent();
  for (auto &p : node->patterns)
    p->accept(*this);
  dedent();

  printIndent();
  print("body:\n");
  indent();
  node->body->accept(*this);
  dedent();

  dedent();
}

void ASTPrinter::visit(BreakStmt *node) {
  printIndent();
  print("%sBreakStmt%s (id=%lld)\n", C_STMT, RESET, assignId(node));
}

void ASTPrinter::visit(ContinueStmt *node) {
  printIndent();
  print("%sContinueStmt%s (id=%lld)\n", C_STMT, RESET, assignId(node));
}

void ASTPrinter::visit(EmptyStmt *node) {
  printIndent();
  print("%sEmptyStmt%s (id=%lld)\n", C_STMT, RESET, assignId(node));
}

// Patterns
void ASTPrinter::visit(ConstPattern *node) {
  printIndent();
  print("%sConstPattern%s (id=%lld)\n", C_PATTERN, RESET, assignId(node));
  indent();
  node->value->accept(*this);
  dedent();
}

void ASTPrinter::visit(WildcardPattern *node) {
  printIndent();
  print("%sWildcardPattern%s (id=%lld)\n", C_PATTERN, RESET, assignId(node));
}

// Expressions
void ASTPrinter::visit(Identifier *node) {
  printIndent();
  print("%sIdentifier%s (id=%lld, name=%s)\n", C_IDENT, RESET, assignId(node),
        node->name ? safeToken(node->name) : "<unnamed>");
}

void ASTPrinter::visit(IntLiteral *node) {
  printIndent();
  print("%sIntLiteral%s (id=%lld, val=%s)\n", C_LITERAL, RESET, assignId(node),
        node->value ? safeToken(node->value) : "<null>");
}

void ASTPrinter::visit(FloatLiteral *node) {
  printIndent();
  print("%sFloatLiteral%s (id=%lld, val=%s)\n", C_LITERAL, RESET,
        assignId(node), node->value ? safeToken(node->value) : "<null>");
}

void ASTPrinter::visit(StringLiteral *node) {
  printIndent();
  print("%sStringLiteral%s (id=%lld, val=%s)\n", C_LITERAL, RESET,
        assignId(node), node->value ? safeToken(node->value) : "<null>");
}

void ASTPrinter::visit(CharLiteral *node) {
  printIndent();
  print("%sCharLiteral%s (id=%lld, val=%s)\n", C_LITERAL, RESET, assignId(node),
        node->value ? safeToken(node->value) : "<null>");
}

void ASTPrinter::visit(BoolLiteral *node) {
  printIndent();
  print("%sBoolLiteral%s (id=%lld, val=%s)\n", C_LITERAL, RESET, assignId(node),
        node->boolValue ? "true" : "false");
}

void ASTPrinter::visit(NullLiteral *node) {
  printIndent();
  print("%sNullLiteral%s (id=%lld)\n", C_LITERAL, RESET, assignId(node));
}

void ASTPrinter::visit(GroupedExpr *node) {
  printIndent();
  print("%sGroupedExpr%s (id=%lld)\n", C_EXPR, RESET, assignId(node));
  indent();
  node->inner->accept(*this);
  dedent();
}

void ASTPrinter::visit(StructInitField *node) {
  printIndent();
  print("%sStructInitField%s (id=%lld, name=%s)\n", C_EXPR, RESET,
        assignId(node), node->name ? safeToken(node->name) : "<unnamed>");
  indent();
  node->value->accept(*this);
  dedent();
}

void ASTPrinter::visit(StructInit *node) {
  printIndent();
  print("%sStructInit%s (id=%lld, type=%s)\n", C_EXPR, RESET, assignId(node),
        node->structName ? safeToken(node->structName) : "<unnamed>");
  indent();
  if (node->fields) {
    for (auto &f : *node->fields)
      f->accept(*this);
  }
  dedent();
}

void ASTPrinter::visit(UnaryExpr *node) {
  printIndent();
  print("%sUnaryExpr%s (id=%lld, op='%s')\n", C_EXPR, RESET, assignId(node),
        node->opTok ? safeToken(node->opTok) : "<op>");
  indent();
  node->operand->accept(*this);
  dedent();
}

void ASTPrinter::visit(BinaryExpr *node) {
  printIndent();
  print("%sBinaryExpr%s (id=%lld, op='%s')\n", C_EXPR, RESET, assignId(node),
        node->opTok ? safeToken(node->opTok) : "<op>");
  indent();
  node->left->accept(*this);
  node->right->accept(*this);
  dedent();
}

void ASTPrinter::visit(AssignmentExpr *node) {
  printIndent();
  print("%sAssignmentExpr%s (id=%lld, op='%s')\n", C_EXPR, RESET,
        assignId(node), node->opTok ? safeToken(node->opTok) : "<op>");
  indent();
  node->left->accept(*this);
  node->right->accept(*this);
  dedent();
}

void ASTPrinter::visit(ConditionalExpr *node) {
  printIndent();
  print("%sConditionalExpr%s (id=%lld)\n", C_EXPR, RESET, assignId(node));
  indent();

  printIndent();
  print("cond:\n");
  indent();
  node->condition->accept(*this);
  dedent();

  printIndent();
  print("then:\n");
  indent();
  node->thenExpr->accept(*this);
  dedent();

  printIndent();
  print("else:\n");
  indent();
  node->elseExpr->accept(*this);
  dedent();

  dedent();
}

void ASTPrinter::visit(CastExpr *node) {
  printIndent();
  print("%sCastExpr%s (id=%lld)\n", C_EXPR, RESET, assignId(node));
  indent();
  printIndent();
  print("type:\n");
  indent();
  node->targetType->accept(*this);
  dedent();
  printIndent();
  print("expr:\n");
  indent();
  node->operand->accept(*this);
  dedent();
  dedent();
}

void ASTPrinter::visit(CallExpr *node) {
  printIndent();
  print("%sCallExpr%s (id=%lld)\n", C_EXPR, RESET, assignId(node));
  indent();

  printIndent();
  print("callee:\n");
  indent();
  node->callee->accept(*this);
  dedent();

  if (node->arguments) {
    printIndent();
    print("args:\n");
    indent();
    for (auto &a : *node->arguments)
      a->accept(*this);
    dedent();
  }

  dedent();
}

void ASTPrinter::visit(MemberAccessExpr *node) {
  printIndent();
  print("%sMemberAccessExpr%s (id=%lld, member=%s)\n", C_EXPR, RESET,
        assignId(node), node->member ? safeToken(node->member) : "<unnamed>");
  indent();
  node->object->accept(*this);
  dedent();
}

void ASTPrinter::visit(IndexAccessExpr *node) {
  printIndent();
  print("%sIndexAccessExpr%s (id=%lld)\n", C_EXPR, RESET, assignId(node));
  indent();
  printIndent();
  print("array:\n");
  indent();
  node->array->accept(*this);
  dedent();
  printIndent();
  print("index:\n");
  indent();
  node->index->accept(*this);
  dedent();
  dedent();
}

void ASTPrinter::visit(PostfixStepExpr *node) {
  printIndent();
  print("%sPostfixStepExpr%s (id=%lld, op='%s')\n", C_EXPR, RESET,
        assignId(node), node->opTok ? safeToken(node->opTok) : "<op>");
  indent();
  node->operand->accept(*this);
  dedent();
}
