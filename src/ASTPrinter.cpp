#include "ASTPrinter.h"
#include <format>
#include <print>

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

// -------------------- printIndent / indent helpers --------------------
void ASTPrinter::printIndent() {
  for (auto &s : indentStack)
    std::print("{}", s); // s already contains spaces
}

void ASTPrinter::indent() { indentStack.push_back("   "); }
void ASTPrinter::dedent() {
  if (!indentStack.empty())
    indentStack.pop_back();
}

// -------------------- Utilities --------------------
long long ASTPrinter::assignId(Node *node) {
  auto it = nodeIds.find(node);
  if (it != nodeIds.end())
    return it->second;
  long long id = nextNodeId++;
  nodeIds[node] = id;
  return id;
}

const char *ASTPrinter::safeToken(const TokenView &tv) {
  if (!tv)
    return "<unnamed>";
  tempString = std::string(tv->token);
  return tempString.c_str();
}

// -------------------- Entry --------------------
void ASTPrinter::print(Program *program) {
  printIndent();
  std::println("{}Program{} (id={})", C_NODE, RESET, assignId(program));
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
    decl->accept(*this);
  }
}

// Declarations
void ASTPrinter::visit(FuncDecl *node) {
  printIndent();
  std::println("{}FuncDecl{} (id={}, name={})", C_DECL, RESET, assignId(node),
               node->name ? safeToken(node->name) : "<unnamed>");
  indent();

  if (node->params) {
    printIndent();
    std::println("{}params:{}", C_KEY, RESET);
    indent();
    for (auto &p : *node->params)
      p->accept(*this);
    dedent();
  }

  if (node->returnType) {
    printIndent();
    std::println("{}returnType:{}", C_KEY, RESET);
    indent();
    node->returnType->accept(*this);
    dedent();
  }

  dedent();
}

void ASTPrinter::visit(FuncDef *node) {
  printIndent();
  std::println("{}FuncDef{} (id={}, name={})", C_DECL, RESET, assignId(node),
               node->name ? safeToken(node->name) : "<unnamed>");
  indent();

  if (node->params) {
    printIndent();
    std::println("{}params:{}", C_KEY, RESET);
    indent();
    for (auto &p : *node->params)
      p->accept(*this);
    dedent();
  }

  if (node->returnType) {
    printIndent();
    std::println("{}returnType:{}", C_KEY, RESET);
    indent();
    node->returnType->accept(*this);
    dedent();
  }

  printIndent();
  std::println("{}body:{}", C_KEY, RESET);
  indent();
  node->body->accept(*this);
  dedent();

  dedent();
}

void ASTPrinter::visit(StaticDef *node) {
  printIndent();
  std::println("{}StaticDef{} (id={})", C_DECL, RESET, assignId(node));
  indent();
  node->funcDef->accept(*this);
  dedent();
}

void ASTPrinter::visit(ExternDecl *node) {
  printIndent();
  std::println("{}ExternDecl{} (id={}, name={})", C_DECL, RESET, assignId(node),
               node->funcDecl && node->funcDecl->name
                   ? safeToken(node->funcDecl->name)
                   : "<unnamed>");
  indent();

  if (node->StringLiteral) {
    printIndent();
    std::println("link name: {}", safeToken(node->StringLiteral));
  }

  node->funcDecl->accept(*this);
  dedent();
}

void ASTPrinter::visit(TypealiasDecl *node) {
  printIndent();
  std::println("{}TypeAlias{} (id={}, name={})", C_TYPE, RESET, assignId(node),
               node->name ? safeToken(node->name) : "<unnamed>");

  indent();
  node->aliasedType->accept(*this);
  dedent();
}

void ASTPrinter::visit(StructDecl *node) {
  printIndent();
  std::println("{}StructDecl{} (id={}, name={})", C_TYPE, RESET, assignId(node),
               node->structName ? safeToken(node->structName) : "<unnamed>");

  indent();

  if (node->body) {
    printIndent();
    std::println("{}fields:{}", C_KEY, RESET);
    indent();
    for (auto &f : *node->body)
      f->accept(*this);
    dedent();
  }

  dedent();
}

void ASTPrinter::visit(StructField *node) {
  printIndent();
  std::println("{}StructField{} (id={}, name={})", C_TYPE, RESET,
               assignId(node),
               node->name ? safeToken(node->name) : "<unnamed>");

  indent();

  printIndent();
  std::println("type:");
  indent();
  node->type->accept(*this);
  dedent();

  if (node->defaultValue) {
    printIndent();
    std::println("default:");
    indent();
    node->defaultValue->accept(*this);
    dedent();
  }

  dedent();
}

void ASTPrinter::visit(Param *node) {
  printIndent();
  std::println("{}Param{} (id={}, name={})", C_PARAM, RESET, assignId(node),
               node->name ? safeToken(node->name) : "<unnamed>");
  indent();
  node->type->accept(*this);
  dedent();
}

void ASTPrinter::visit(VarDecl *node) {
  printIndent();
  std::println("{}VarDecl{} (id={}, name={}, {})", C_DECL, RESET,
               assignId(node), node->name ? safeToken(node->name) : "<unnamed>",
               node->isConst ? "const" : "let");

  indent();

  printIndent();
  std::println("type:");
  indent();
  node->type->accept(*this);
  dedent();

  if (node->initializer) {
    printIndent();
    std::println("initializer:");
    indent();
    node->initializer->accept(*this);
    dedent();
  }

  dedent();
}

// -------------------- Types --------------------
void ASTPrinter::visit(TypeSpec *node) {
  printIndent();
  std::println("{}TypeSpec{} (id={}, dims={})", C_TYPE, RESET, assignId(node),
               node->arrayDimensions);
  indent();
  if (node->baseTy)
    node->baseTy->accept(*this);
  dedent();
}

void ASTPrinter::visit(PrimitiveType *node) {
  printIndent();
  std::println("{}PrimitiveType{} (id={}, kind=({}, {}))", C_TYPE, RESET,
               assignId(node), (int)node->kind, safeToken(node->typeToken));
}

void ASTPrinter::visit(StructType *node) {
  printIndent();
  std::println("{}StructType{} (id={}, name={})", C_TYPE, RESET, assignId(node),
               node->name ? safeToken(node->name) : "<unnamed>");
}

void ASTPrinter::visit(FuncPtrType *node) {
  printIndent();
  std::println("{}FuncPtrType{} (id={})", C_TYPE, RESET, assignId(node));

  indent();
  if (node->paramsTy) {
    for (auto &t : *node->paramsTy)
      t->accept(*this);
  }

  printIndent();
  std::println("return:");
  indent();
  node->returnType->accept(*this);
  dedent();

  dedent();
}

void ASTPrinter::visit(PtrType *node) {
  printIndent();
  std::println("{}PtrType{} (id={})", C_TYPE, RESET, assignId(node));
  indent();
  node->pointedType->accept(*this);
  dedent();
}

void ASTPrinter::visit(RefType *node) {
  printIndent();
  std::println("{}RefType{} (id={})", C_TYPE, RESET, assignId(node));
  indent();
  node->referencedType->accept(*this);
  dedent();
}

// -------------------- Statements --------------------
void ASTPrinter::visit(CompoundStmt *node) {
  printIndent();
  std::println("{}CompoundStmt{} (id={})", C_STMT, RESET, assignId(node));
  indent();
  if (node->statements) {
    for (auto &s : *node->statements)
      s->accept(*this);
  }
  dedent();
}

void ASTPrinter::visit(ExprStmt *node) {
  printIndent();
  std::println("{}ExprStmt{} (id={})", C_STMT, RESET, assignId(node));
  indent();
  node->expression->accept(*this);
  dedent();
}

void ASTPrinter::visit(IfStmt *node) {
  printIndent();
  std::println("{}IfStmt{} (id={})", C_STMT, RESET, assignId(node));
  indent();

  printIndent();
  std::println("condition:");
  indent();
  node->condition->accept(*this);
  dedent();

  printIndent();
  std::println("then:");
  indent();
  node->thenBranch->accept(*this);
  dedent();

  if (node->elseBranch) {
    printIndent();
    std::println("else:");
    indent();
    node->elseBranch->accept(*this);
    dedent();
  }

  dedent();
}

void ASTPrinter::visit(WhileStmt *node) {
  printIndent();
  std::println("{}WhileStmt{} (id={})", C_STMT, RESET, assignId(node));
  indent();

  printIndent();
  std::println("condition:");
  indent();
  node->condition->accept(*this);
  dedent();

  printIndent();
  std::println("body:");
  indent();
  node->body->accept(*this);
  dedent();

  dedent();
}

void ASTPrinter::visit(ForStmt *node) {
  printIndent();
  std::println("{}ForStmt{} (id={})", C_STMT, RESET, assignId(node));
  indent();

  if (node->initializer.has_value()) {
    printIndent();
    std::println("init:");
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
    std::println("cond:");
    indent();
    node->condition->accept(*this);
    dedent();
  }

  if (node->update) {
    printIndent();
    std::println("update:");
    indent();
    node->update->accept(*this);
    dedent();
  }

  printIndent();
  std::println("body:");
  indent();
  node->body->accept(*this);
  dedent();

  dedent();
}

void ASTPrinter::visit(ReturnStmt *node) {
  printIndent();
  std::println("{}ReturnStmt{} (id={})", C_STMT, RESET, assignId(node));
  if (node->value) {
    indent();
    node->value->accept(*this);
    dedent();
  }
}

void ASTPrinter::visit(DeferStmt *node) {
  printIndent();
  std::println("{}DeferStmt{} (id={})", C_STMT, RESET, assignId(node));
  indent();
  node->deferExpr->accept(*this);
  dedent();
}

void ASTPrinter::visit(SwitchStmt *node) {
  printIndent();
  std::println("{}SwitchStmt{} (id={})", C_STMT, RESET, assignId(node));
  indent();

  printIndent();
  std::println("expr:");
  indent();
  node->expression->accept(*this);
  dedent();

  printIndent();
  std::println("patterns:");
  indent();

  if (node->switchArm) {
    for (auto &c : *node->switchArm)
      c->accept(*this);
  }

  dedent();

  if (node->defaultBody) {
    printIndent();
    std::println("default:");
    indent();
    node->defaultBody->accept(*this);
    dedent();
  }

  dedent();
}

void ASTPrinter::visit(SwitchArm *node) {
  printIndent();
  std::println("{}SwitchArm{} (id={})", C_STMT, RESET, assignId(node));
  indent();

  printIndent();
  std::println("patterns:");
  indent();

  if (node->patterns) {
    for (auto &p : *node->patterns)
      p->accept(*this);
  }

  dedent();

  printIndent();
  if (node->fatArrowTok)
    std::println("{}{}{}", C_KEY, safeToken(node->fatArrowTok), RESET);
  else
    std::println("{}=>{}", C_KEY, RESET);

  if (node->body) {
    indent();
    node->body->accept(*this);
    dedent();
  }

  dedent();
}

void ASTPrinter::visit(BreakStmt *node) {
  printIndent();
  std::println("{}BreakStmt{} (id={})", C_STMT, RESET, assignId(node));
}

void ASTPrinter::visit(ContinueStmt *node) {
  printIndent();
  std::println("{}ContinueStmt{} (id={})", C_STMT, RESET, assignId(node));
}

void ASTPrinter::visit(EmptyStmt *node) {
  printIndent();
  std::println("{}EmptyStmt{} (id={})", C_STMT, RESET, assignId(node));
}

// -------------------- Patterns --------------------
void ASTPrinter::visit(ConstPattern *node) {
  printIndent();
  std::println("{}ConstPattern{} (id={})", C_PATTERN, RESET, assignId(node));
  indent();
  node->value->accept(*this);
  dedent();
}

void ASTPrinter::visit(WildcardPattern *node) {
  printIndent();
  std::println("{}WildcardPattern{} (id={})", C_PATTERN, RESET, assignId(node));
}

// -------------------- Expressions --------------------
void ASTPrinter::visit(Identifier *node) {
  printIndent();
  std::println("{}Identifier{} (id={}, name={})", C_IDENT, RESET,
               assignId(node),
               node->name ? safeToken(node->name) : "<unnamed>");
}

void ASTPrinter::visit(IntLiteral *node) {
  printIndent();
  std::println("{}IntLiteral{} (id={}, val={})", C_LITERAL, RESET,
               assignId(node), node->value ? safeToken(node->value) : "<null>");
}

void ASTPrinter::visit(FloatLiteral *node) {
  printIndent();
  std::println("{}FloatLiteral{} (id={}, val={})", C_LITERAL, RESET,
               assignId(node), node->value ? safeToken(node->value) : "<null>");
}

void ASTPrinter::visit(StringLiteral *node) {
  printIndent();
  std::println("{}StringLiteral{} (id={}, val={})", C_LITERAL, RESET,
               assignId(node), node->value ? safeToken(node->value) : "<null>");
}

void ASTPrinter::visit(CharLiteral *node) {
  printIndent();
  std::println("{}CharLiteral{} (id={}, val={})", C_LITERAL, RESET,
               assignId(node), node->value ? safeToken(node->value) : "<null>");
}

void ASTPrinter::visit(BoolLiteral *node) {
  printIndent();
  std::println("{}BoolLiteral{} (id={}, val={})", C_LITERAL, RESET,
               assignId(node), node->boolValue ? "true" : "false");
}

void ASTPrinter::visit(NullLiteral *node) {
  printIndent();
  std::println("{}NullLiteral{} (id={})", C_LITERAL, RESET, assignId(node));
}

void ASTPrinter::visit(GroupedExpr *node) {
  printIndent();
  std::println("{}GroupedExpr{} (id={})", C_EXPR, RESET, assignId(node));
  indent();
  node->inner->accept(*this);
  dedent();
}

void ASTPrinter::visit(StructInitField *node) {
  printIndent();
  std::println("{}StructInitField{} (id={}, name={})", C_EXPR, RESET,
               assignId(node),
               node->name ? safeToken(node->name) : "<unnamed>");
  indent();
  node->value->accept(*this);
  dedent();
}

void ASTPrinter::visit(StructInit *node) {
  printIndent();
  std::println("{}StructInit{} (id={}, type={})", C_EXPR, RESET, assignId(node),
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
  std::println("{}UnaryExpr{} (id={}, op='{}')", C_EXPR, RESET, assignId(node),
               node->opTok ? safeToken(node->opTok) : "<op>");
  indent();
  node->operand->accept(*this);
  dedent();
}

void ASTPrinter::visit(BinaryExpr *node) {
  printIndent();
  std::println("{}BinaryExpr{} (id={}, op='{}')", C_EXPR, RESET, assignId(node),
               node->opTok ? safeToken(node->opTok) : "<op>");
  indent();
  node->left->accept(*this);
  node->right->accept(*this);
  dedent();
}

void ASTPrinter::visit(AssignmentExpr *node) {
  printIndent();
  std::println("{}AssignmentExpr{} (id={}, op='{}')", C_EXPR, RESET,
               assignId(node), node->opTok ? safeToken(node->opTok) : "<op>");
  indent();
  node->left->accept(*this);
  node->right->accept(*this);
  dedent();
}

void ASTPrinter::visit(ConditionalExpr *node) {
  printIndent();
  std::println("{}ConditionalExpr{} (id={})", C_EXPR, RESET, assignId(node));
  indent();

  printIndent();
  std::println("cond:");
  indent();
  node->condition->accept(*this);
  dedent();

  printIndent();
  std::println("then:");
  indent();
  node->thenExpr->accept(*this);
  dedent();

  printIndent();
  std::println("else:");
  indent();
  node->elseExpr->accept(*this);
  dedent();

  dedent();
}

void ASTPrinter::visit(CastExpr *node) {
  printIndent();
  std::println("{}CastExpr{} (id={})", C_EXPR, RESET, assignId(node));
  indent();

  printIndent();
  std::println("type:");
  indent();
  node->targetType->accept(*this);
  dedent();

  printIndent();
  std::println("expr:");
  indent();
  node->operand->accept(*this);
  dedent();

  dedent();
}

void ASTPrinter::visit(CallExpr *node) {
  printIndent();
  std::println("{}CallExpr{} (id={})", C_EXPR, RESET, assignId(node));
  indent();

  printIndent();
  std::println("callee:");
  indent();
  node->callee->accept(*this);
  dedent();

  if (node->arguments) {
    printIndent();
    std::println("args:");
    indent();
    for (auto &a : *node->arguments)
      a->accept(*this);
    dedent();
  }

  dedent();
}

void ASTPrinter::visit(MemberAccessExpr *node) {
  printIndent();
  std::println("{}MemberAccessExpr{} (id={}, member={})", C_EXPR, RESET,
               assignId(node),
               node->member ? safeToken(node->member) : "<unnamed>");
  indent();
  node->object->accept(*this);
  dedent();
}

void ASTPrinter::visit(IndexAccessExpr *node) {
  printIndent();
  std::println("{}IndexAccessExpr{} (id={})", C_EXPR, RESET, assignId(node));
  indent();

  printIndent();
  std::println("array:");
  indent();
  node->array->accept(*this);
  dedent();

  printIndent();
  std::println("index:");
  indent();
  node->index->accept(*this);
  dedent();

  dedent();
}

void ASTPrinter::visit(PostfixStepExpr *node) {
  printIndent();
  std::println("{}PostfixStepExpr{} (id={}, op='{}')", C_EXPR, RESET,
               assignId(node), node->opTok ? safeToken(node->opTok) : "<op>");
  indent();
  node->operand->accept(*this);
  dedent();
}
