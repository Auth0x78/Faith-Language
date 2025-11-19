#ifndef FAITH_AST_PRINTER_H
#define FAITH_AST_PRINTER_H

#include "FaithAST.h"
#include <cstdarg>
#include <cstdio>
#include <string>
#include <unordered_map>
#include <vector>

namespace Faith {

class ASTPrinter : public ASTVisitor {
public:
  ASTPrinter(bool enableColor = true);

  // Entry point
  void print(Program *program);

  // ─────────────────────────────
  // Visitor interface overrides
  // ─────────────────────────────

  // Program
  void visit(Program *node) override;

  // Declarations
  void visit(FuncDecl *node) override;
  void visit(FuncDef *node) override;
  void visit(StaticDef *node) override;
  void visit(ExternDecl *node) override;
  void visit(TypealiasDecl *node) override;
  void visit(StructDecl *node) override;
  void visit(StructField *node) override;
  void visit(Param *node) override;
  void visit(VarDecl *node) override;

  // Types
  void visit(TypeSpec *node) override;
  void visit(PrimitiveType *node) override;
  void visit(StructType *node) override;
  void visit(FuncPtrType *node) override;
  void visit(PtrType *node) override;
  void visit(RefType *node) override;

  // Statements
  void visit(CompoundStmt *node) override;
  void visit(ExprStmt *node) override;
  void visit(IfStmt *node) override;
  void visit(WhileStmt *node) override;
  void visit(ForStmt *node) override;
  void visit(ReturnStmt *node) override;
  void visit(DeferStmt *node) override;
  void visit(SwitchStmt *node) override;
  void visit(CaseBlock *node) override;
  void visit(MatchStmt *node) override;
  void visit(MatchArm *node) override;
  void visit(BreakStmt *node) override;
  void visit(ContinueStmt *node) override;
  void visit(EmptyStmt *node) override;

  // Patterns
  void visit(ConstPattern *node) override;
  void visit(WildcardPattern *node) override;

  // Expressions
  void visit(Identifier *node) override;
  void visit(IntLiteral *node) override;
  void visit(FloatLiteral *node) override;
  void visit(StringLiteral *node) override;
  void visit(CharLiteral *node) override;
  void visit(BoolLiteral *node) override;
  void visit(NullLiteral *node) override;
  void visit(GroupedExpr *node) override;
  void visit(StructInitField *node) override;
  void visit(StructInit *node) override;
  void visit(UnaryExpr *node) override;
  void visit(BinaryExpr *node) override;
  void visit(AssignmentExpr *node) override;
  void visit(ConditionalExpr *node) override;
  void visit(CastExpr *node) override;
  void visit(CallExpr *node) override;
  void visit(MemberAccessExpr *node) override;
  void visit(IndexAccessExpr *node) override;
  void visit(PostfixStepExpr *node) override;

private:
  // ─────────────────────────────
  // Internal helpers
  // ─────────────────────────────

  // Safe printing (ANSI aware)
  void print(const char *fmt, ...);
  void printIndent();

  // indentation
  void indent();
  void dedent();
  std::vector<std::string> indentStack;

  // node id tracking
  long long assignId(Node *node);
  std::unordered_map<Node *, long long> nodeIds;
  static long long nextNodeId;

  // token → safe c-string
  const char *safeToken(const TokenView &tv);
  std::string tempString; // reused buffer

  // ANSI colors (disable if needed)
  bool colorEnabled;
  const char *C_NODE;
  const char *C_DECL;
  const char *C_TYPE;
  const char *C_STMT;
  const char *C_EXPR;
  const char *C_PARAM;
  const char *C_PATTERN;
  const char *C_IDENT;
  const char *C_LITERAL;
  const char *C_KEY;
  const char *RESET;

  void enableColors();
  void disableColors();
};

} // namespace Faith

#endif
