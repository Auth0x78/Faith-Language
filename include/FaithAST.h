﻿#ifndef FAITH_AST_H
#define FAITH_AST_H

#include <memory>
#include <optional>
#include <variant>
#include <vector>

#include "Lexer.h"

namespace Faith {

// =======================================================
// TOKENVIEW
// =======================================================

class TokenView {
public:
  // Token View Constructors
  TokenView() : m_token(nullptr) {}
  TokenView(const Token *token) : m_token(token) {}

  TokenView(const TokenView &other) = default;

  TokenView(TokenView &&other) noexcept : m_token(other.m_token) {
    other.m_token =
        nullptr; // Leave the moved-from object in a safe, null state
  }

  // Move and Copy Assignments
  TokenView &operator=(const TokenView &other) = default;

  TokenView &operator=(TokenView &&other) noexcept {
    if (this != &other) {
      m_token = other.m_token;
      other.m_token = nullptr;
    }
    return *this;
  }

  TokenView &operator=(const Token *token) {
    m_token = token;
    return *this;
  }

  // Allows using '->' to access the underlying token's members
  const Token *operator->() const { return m_token; }

  // Allows using '*' to get a reference to the underlying token
  const Token &operator*() const { return *m_token; }

  // Checks if the view is non-null (i.e., it points to a token
  explicit operator bool() const { return m_token != nullptr; }

  bool has_value() const { return m_token != nullptr; }

private:
  const Token *m_token;
};

// =======================================================
// FORWARD DECLARATIONS
// =======================================================

// Visitor
class ASTVisitor;

// Base Nodes
class Node;
class Expr;
class Stmt;
class Decl;
class TypeNode;
class BaseType;
class Pattern;

// Type Nodes
class TypeSpec;
class PrimitiveType;
class StructType;
class FuncPtrType;
class PtrType;
class RefType;
class Param;

// Expressions
class Identifier;
class NumberLiteral;
class StringLiteral;
class CharLiteral;
class BoolLiteral;
class NullLiteral;
class GroupedExpr;
class StructInit;
class StructInitField;
class UnaryExpr;
class BinaryExpr;
class AssignmentExpr;
class ConditionalExpr;
class CastExpr;
class CallExpr;
class MemberAccessExpr;
class IndexAccessExpr;
class PostfixStepExpr;

// Statements
class CompoundStmt;
class ExprStmt;
class IfStmt;
class WhileStmt;
class ForStmt;
class ReturnStmt;
class DeferStmt;
class SwitchStmt;
class CaseBlock;
class MatchStmt;
class MatchArm;
class BreakStmt;
class ContinueStmt;
class EmptyStmt;

// Patterns
class ConstPattern;
class WildcardPattern;

// Declarations
class Program;
class FuncDecl;
class VarDecl; // This will inherit from Stmt and Decl
class StructDecl;
class StructField;
class TypealiasDecl;

// =======================================================
// TYPE ALIASES
// =======================================================

template <typename T> using UPtr = std::unique_ptr<T>;

template <typename T> using Vec = std::vector<T>;

using paramsTypeVec = std::vector<std::unique_ptr<Faith::TypeSpec>>;

// =======================================================
// ENUMS FOR OPERATORS
// =======================================================

enum class LinkageSpecifier {
  NONE,
  STATIC,
  EXTERN,
};

enum class PrimitiveTypeKind {
  U8,
  U16,
  U32,
  U64,
  I8,
  I16,
  I32,
  I64,
  F32,
  F64,
  CHAR,
  BOOL,
  VOID
};

enum class AssignmentOp {
  Assign,       // =
  AddAssign,    // +=
  SubAssign,    // -=
  MulAssign,    // *=
  DivAssign,    // /=
  ModAssign,    // %=
  BitAndAssign, // &=
  BitOrAssign,  // |=
  BitXorAssign  // ^=
};

enum class BinaryOp {
  LogicalOr,    // ||
  LogicalAnd,   // &&
  BitwiseOr,    // |
  BitwiseXor,   // ^
  BitwiseAnd,   // &
  Equal,        // ==
  NotEqual,     // !=
  Less,         // <
  LessEqual,    // <=
  Greater,      // >
  GreaterEqual, // >=
  ShiftLeft,    // <<
  ShiftRight,   // >>
  Add,          // +
  Subtract,     // -
  Multiply,     // *
  Divide,       // /
  Modulo        // %
};

enum class UnaryOp {
  Plus,        // +
  Minus,       // -
  LogicalNot,  // !
  Dereference, // *
  AddressOf,   // &
  PrefixInc,   // ++
  PrefixDec    // --
};

// =======================================================
// VISITOR INTERFACE
// =======================================================

class ASTVisitor {
public:
  virtual ~ASTVisitor() = default;

  // Program
  virtual void visit(Program *node) = 0;

  // Decls
  virtual void visit(FuncDecl *node) = 0;
  virtual void visit(VarDecl *node) = 0;
  virtual void visit(StructDecl *node) = 0;
  virtual void visit(StructField *node) = 0;
  virtual void visit(TypealiasDecl *node) = 0;
  virtual void visit(Param *node) = 0;

  // Types
  virtual void visit(TypeSpec *node) = 0;
  virtual void visit(PrimitiveType *node) = 0;
  virtual void visit(StructType *node) = 0;
  virtual void visit(FuncPtrType *node) = 0;
  virtual void visit(PtrType *node) = 0;
  virtual void visit(RefType *node) = 0;

  // Stmts
  virtual void visit(CompoundStmt *node) = 0;
  virtual void visit(ExprStmt *node) = 0;
  virtual void visit(IfStmt *node) = 0;
  virtual void visit(WhileStmt *node) = 0;
  virtual void visit(ForStmt *node) = 0;
  virtual void visit(ReturnStmt *node) = 0;
  virtual void visit(DeferStmt *node) = 0;
  virtual void visit(SwitchStmt *node) = 0;
  virtual void visit(CaseBlock *node) = 0;
  virtual void visit(MatchStmt *node) = 0;
  virtual void visit(MatchArm *node) = 0;
  virtual void visit(BreakStmt *node) = 0;
  virtual void visit(ContinueStmt *node) = 0;
  virtual void visit(EmptyStmt *node) = 0;

  // Patterns
  virtual void visit(ConstPattern *node) = 0;
  virtual void visit(WildcardPattern *node) = 0;

  // Exprs
  virtual void visit(Identifier *node) = 0;
  virtual void visit(NumberLiteral *node) = 0;
  virtual void visit(StringLiteral *node) = 0;
  virtual void visit(CharLiteral *node) = 0;
  virtual void visit(BoolLiteral *node) = 0;
  virtual void visit(NullLiteral *node) = 0;
  virtual void visit(GroupedExpr *node) = 0;
  virtual void visit(StructInit *node) = 0;
  virtual void visit(StructInitField *node) = 0;
  virtual void visit(UnaryExpr *node) = 0;
  virtual void visit(BinaryExpr *node) = 0;
  virtual void visit(AssignmentExpr *node) = 0;
  virtual void visit(ConditionalExpr *node) = 0;
  virtual void visit(CastExpr *node) = 0;
  virtual void visit(CallExpr *node) = 0;
  virtual void visit(MemberAccessExpr *node) = 0;
  virtual void visit(IndexAccessExpr *node) = 0;
  virtual void visit(PostfixStepExpr *node) = 0;
};

// =======================================================
// BASE NODES
// =======================================================

/**
 * @brief Base class for all AST nodes.
 * We use virtual inheritance to solve the 'VarDecl' diamond
 * (it's both a Stmt and a Decl).
 */
class Node {
public:
  virtual ~Node() = default;
  virtual void accept(ASTVisitor &visitor) = 0;
};

class Expr : public virtual Node {};
class Stmt : public virtual Node {};
class Decl : public virtual Node {};
class TypeNode : public virtual Node {};
class Pattern : public virtual Node {};

// =======================================================
// PROGRAM STRUCTURE
// =======================================================

class Program final : public Node {
public:
  Vec<UPtr<Decl>> declarations;

  Program() = default;

  Program(Vec<UPtr<Decl>> decls) : declarations(std::move(decls)) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class Param final : public Node {
public:
  TokenView name;
  UPtr<TypeSpec> type;

  Param(TokenView name, UPtr<TypeSpec> type)
      : name(name), type(std::move(type)) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

// =======================================================
// DECLARATIONS
// =======================================================

class FuncDecl final : public Decl {
public:
  // Linkage specifier for the function decl
  LinkageSpecifier linkageSpec;

  TokenView funcTok; // The 'func' keyword
  TokenView name;
  Vec<UPtr<Param>> params;
  UPtr<TypeSpec> returnType; // nullptr if void
  // The function body.
  // NOTE: A nullptr 'body' implies a function declaration without a definition.
  UPtr<CompoundStmt> body;

  FuncDecl(LinkageSpecifier linkage, TokenView funcTok, TokenView name,
           Vec<UPtr<Param>> params, UPtr<TypeSpec> retType,
           UPtr<CompoundStmt> body)
      : linkageSpec(linkage), funcTok(funcTok), name(name),
        params(std::move(params)), returnType(std::move(retType)),
        body(std::move(body)) {}

  // --- Visitor Pattern ---
  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class VarDecl final : public Decl, public Stmt {
public:
  bool isGlobal;
  bool isConst;
  TokenView name;
  UPtr<TypeSpec> type;
  UPtr<Expr> initializer; // nullptr if no initializer

  VarDecl(bool isConst, TokenView name, UPtr<TypeSpec> type, UPtr<Expr> init)
      : isConst(isConst), name(name), type(std::move(type)),
        initializer(std::move(init)) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class StructField final : public Node {
public:
  TokenView name;
  UPtr<TypeSpec> type;
  UPtr<Expr> defaultValue; // nullptr if no default

  StructField(TokenView name, UPtr<TypeSpec> type, UPtr<Expr> defVal)
      : name(name), type(std::move(type)), defaultValue(std::move(defVal)) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

using StructBody = Vec<UPtr<StructField>>;

class StructDecl final : public Decl {
public:
  TokenView structName;
  UPtr<StructBody> optBody;

  StructDecl(TokenView name, UPtr<StructBody> body)
      : structName(name), optBody(std::move(body)) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class TypealiasDecl final : public Decl {
public:
  TokenView name;
  UPtr<TypeSpec> aliasedType;

  TypealiasDecl(TokenView name, UPtr<TypeSpec> type)
      : name(name), aliasedType(std::move(type)) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

// =======================================================
// TYPE SYSTEM
// =======================================================

class BaseType : public TypeNode {};

class PrimitiveType final : public BaseType {
public:
  PrimitiveTypeKind kind;
  TokenView typeToken;

  PrimitiveType(PrimitiveTypeKind k, TokenView tok) : kind(k), typeToken(tok) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class StructType final : public BaseType {
public:
  TokenView name;

  StructType(TokenView name) : name(name) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class FuncPtrType final : public BaseType {
public:
  TokenView starTok;
  UPtr<Vec<UPtr<TypeSpec>>> paramsTy;
  UPtr<TypeSpec> returnType;

  FuncPtrType(TokenView ptok, UPtr<Vec<UPtr<TypeSpec>>> p, UPtr<TypeSpec> ret)
      : starTok(ptok), paramsTy(std::move(p)), returnType(std::move(ret)) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class PtrType final : public BaseType {
public:
  TokenView ptrTok;
  UPtr<BaseType> pointedType;

  PtrType(TokenView p, UPtr<BaseType> pType)
      : ptrTok(p), pointedType(std::move(pType)) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class RefType final : public BaseType {
public:
  TokenView ampersandTok;
  UPtr<BaseType> referencedType; // The type it refers to

  RefType(TokenView amp, UPtr<BaseType> ref)
      : ampersandTok(amp), referencedType(std::move(ref)) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class TypeSpec final : public TypeNode {
public:
  // Points to start of the type token
  TokenView locToken;
  bool isErrorReturn;

  // Base Type
  UPtr<BaseType> baseTy;

  // if is array, then how many dimensional
  uint8_t arrayDimensions;
  /*
   * 0 = Not an Array
   * 1 = 1D array, T[]
   * 2 = 2D array, T[][]
   */

  TypeSpec(TokenView loc, bool isError, UPtr<BaseType> base, int arrDims)
      : locToken(loc), isErrorReturn(isError), baseTy(std::move(base)),
        arrayDimensions(arrDims) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

// =======================================================
// STATEMENTS
// =======================================================

class CompoundStmt final : public Stmt {
public:
  TokenView openBrace;
  Vec<UPtr<Stmt>> statements;

  CompoundStmt(TokenView open, Vec<UPtr<Stmt>> stmts)
      : openBrace(open), statements(std::move(stmts)) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

// =======================================================
// EXPRESSIONS
// =======================================================

class ExprStmt final : public Stmt {
public:
  UPtr<Expr> expression;

  ExprStmt(UPtr<Expr> expr) : expression(std::move(expr)) {}
  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class IfStmt final : public Stmt {
public:
  TokenView ifTok;
  UPtr<Expr> condition;
  UPtr<CompoundStmt> thenBranch;
  UPtr<CompoundStmt> elseBranch; // nullptr if no else branch

  IfStmt(TokenView ifTok, UPtr<Expr> cond, UPtr<CompoundStmt> thenB,
         UPtr<CompoundStmt> elseB)
      : ifTok(ifTok), condition(std::move(cond)), thenBranch(std::move(thenB)),
        elseBranch(std::move(elseB)) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

/** @brief <while_stmt> ::= "while" [ "(" <expr> ")" ] <stmt> */
class WhileStmt final : public Stmt {
public:
  TokenView whileTok;
  UPtr<Expr> condition;
  UPtr<CompoundStmt> body;

  WhileStmt(TokenView whileTok, UPtr<Expr> cond, UPtr<CompoundStmt> body)
      : whileTok(whileTok), condition(std::move(cond)), body(std::move(body)) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

using ForInit = std::variant<UPtr<VarDecl>, UPtr<Expr>>;

/** @brief <for_stmt> ::= "for" "(" ... ")" <stmt> */
class ForStmt final : public Stmt {
public:
  TokenView forTok;
  // std::nullopt if absent, No VarDecl or Expr for init
  std::optional<ForInit> initializer;
  UPtr<Expr> condition; // nullptr if absent
  UPtr<Expr> update;    // nullptr if absent
  UPtr<CompoundStmt> body;

  ForStmt(TokenView forTok, std::optional<ForInit> init, UPtr<Expr> cond,
          UPtr<Expr> update, UPtr<CompoundStmt> body)
      : forTok(forTok), initializer(std::move(init)),
        condition(std::move(cond)), update(std::move(update)),
        body(std::move(body)) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class ReturnStmt final : public Stmt {
public:
  TokenView returnTok;
  UPtr<Expr> value; // nullptr for "return;"

  ReturnStmt(TokenView retTok, UPtr<Expr> val)
      : returnTok(retTok), value(std::move(val)) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class DeferStmt final : public Stmt {
public:
  TokenView deferTok;
  UPtr<Expr> deferExpr;

  DeferStmt(TokenView deferTok, UPtr<Expr> expr)
      : deferTok(deferTok), deferExpr(std::move(expr)) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class BreakStmt final : public Stmt {
public:
  TokenView breakTok;
  BreakStmt(TokenView tok) : breakTok(tok) {}
  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

/** @brief <continue_stmt> ::= "continue" ";" */
class ContinueStmt final : public Stmt {
public:
  TokenView continueTok;
  ContinueStmt(TokenView tok) : continueTok(tok) {}
  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

// TODO: Dont consider empty stmts like ';' for now
// Maybe empty stmt can act like NOP
// For now they are ignored in AST Parsing

// --- Switch/Match ---

/** @brief <case_block> ::= "case" <const_expr> "=>" { <stmt> } */
class CaseBlock final : public Node {
public:
  TokenView caseTok;
  UPtr<Expr> value; // const_expr
  Vec<UPtr<Stmt>> body;

  CaseBlock(TokenView caseTok, UPtr<Expr> val, Vec<UPtr<Stmt>> body)
      : caseTok(caseTok), value(std::move(val)), body(std::move(body)) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

/** @brief <switch_stmt> ::= "switch" ... "{" ... "}" */
class SwitchStmt final : public Stmt {
public:
  TokenView switchTok;
  UPtr<Expr> expression;
  Vec<UPtr<CaseBlock>> cases;
  Vec<UPtr<Stmt>> defaultBody; // Empty if no default
  bool hasDefault = false;

  SwitchStmt(TokenView swTok, UPtr<Expr> expr, Vec<UPtr<CaseBlock>> cases,
             Vec<UPtr<Stmt>> defBody)
      : switchTok(swTok), expression(std::move(expr)), cases(std::move(cases)),
        defaultBody(std::move(defBody)), hasDefault(!defaultBody.empty()) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class MatchArm final : public Node {
public:
  Vec<UPtr<Pattern>> patterns;
  Vec<UPtr<Stmt>> body;
  TokenView arrowTok; // "=>"

  MatchArm(Vec<UPtr<Pattern>> pats, Vec<UPtr<Stmt>> body, TokenView arrow)
      : patterns(std::move(pats)), body(std::move(body)), arrowTok(arrow) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class MatchStmt final : public Stmt {
public:
  TokenView matchTok;
  UPtr<Expr> expression;
  Vec<UPtr<MatchArm>> arms;

  MatchStmt(TokenView matchTok, UPtr<Expr> expr, Vec<UPtr<MatchArm>> arms)
      : matchTok(matchTok), expression(std::move(expr)), arms(std::move(arms)) {
  }

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

// =======================================================
// PATTERNS (for Match)
// =======================================================

/** @brief <pattern> ::= <const_expr> */
class ConstPattern final : public Pattern {
public:
  UPtr<Expr> value; // literal or identifier

  ConstPattern(UPtr<Expr> val) : value(std::move(val)) {}
  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

/** @brief <pattern> ::= "_" */
class WildcardPattern final : public Pattern {
public:
  TokenView underscoreTok;
  WildcardPattern(TokenView tok) : underscoreTok(tok) {}
  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

// =======================================================
// EXPRESSIONS
// =======================================================

// --- Primary / Literals ---

class Identifier final : public Expr {
public:
  TokenView name;
  Identifier(TokenView name) : name(name) {}
  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class NumberLiteral final : public Expr {
public:
  TokenView value;

  NumberLiteral(TokenView val) : value(val) {}
  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class StringLiteral final : public Expr {
public:
  TokenView value;
  StringLiteral(TokenView val) : value(val) {}
  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class CharLiteral final : public Expr {
public:
  TokenView value;
  CharLiteral(TokenView val) : value(val) {}
  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class BoolLiteral final : public Expr {
public:
  TokenView value;
  bool boolValue;

  BoolLiteral(TokenView val, bool b) : value(val), boolValue(b) {}
  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class NullLiteral final : public Expr {
public:
  TokenView nullTok;
  NullLiteral(TokenView tok) : nullTok(tok) {}
  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class GroupedExpr final : public Expr {
public:
  UPtr<Expr> inner;
  GroupedExpr(UPtr<Expr> inner) : inner(std::move(inner)) {}
  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class StructInitField final : public Node {
public:
  TokenView dotTok;
  TokenView name;
  UPtr<Expr> value;

  StructInitField(TokenView dot, TokenView name, UPtr<Expr> val)
      : dotTok(dot), name(name), value(std::move(val)) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

/** @brief <struct_init> ::= <identifier> "{" ... "}" */
class StructInit final : public Expr {
public:
  TokenView structName;
  Vec<UPtr<StructInitField>> fields;
  // Remaining, Struct fields that are not initalised
  // Assume thier, default stated value
  // Or garbage value, if even default value also empty

  StructInit(TokenView name, Vec<UPtr<StructInitField>> fields)
      : structName(name), fields(std::move(fields)) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

// --- Operators ---
class UnaryExpr final : public Expr {
public:
  TokenView opTok;
  UnaryOp op;
  UPtr<Expr> operand;

  UnaryExpr(TokenView opTok, UnaryOp op, UPtr<Expr> operand)
      : opTok(opTok), op(op), operand(std::move(operand)) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class BinaryExpr final : public Expr {
public:
  UPtr<Expr> left;
  TokenView opTok;
  BinaryOp op;
  UPtr<Expr> right;

  BinaryExpr(UPtr<Expr> left, TokenView opTok, BinaryOp op, UPtr<Expr> right)
      : left(std::move(left)), opTok(opTok), op(op), right(std::move(right)) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class AssignmentExpr final : public Expr {
public:
  UPtr<Expr> left;
  TokenView opTok;
  AssignmentOp op;
  UPtr<Expr> right;

  AssignmentExpr(UPtr<Expr> left, TokenView opTok, AssignmentOp op,
                 UPtr<Expr> right)
      : left(std::move(left)), opTok(opTok), op(op), right(std::move(right)) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class ConditionalExpr final : public Expr {
public:
  UPtr<Expr> condition;
  TokenView questionTok;
  UPtr<Expr> thenExpr;
  UPtr<Expr> elseExpr;

  ConditionalExpr(UPtr<Expr> cond, TokenView qTok, UPtr<Expr> thenE,
                  UPtr<Expr> elseE)
      : condition(std::move(cond)), questionTok(qTok),
        thenExpr(std::move(thenE)), elseExpr(std::move(elseE)) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class CastExpr final : public Expr {
public:
  UPtr<TypeSpec> targetType;
  UPtr<Expr> operand;

  CastExpr(UPtr<TypeSpec> type, UPtr<Expr> expr)
      : targetType(std::move(type)), operand(std::move(expr)) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class CallExpr final : public Expr {
public:
  UPtr<Expr> callee;
  Vec<UPtr<Expr>> arguments;
  TokenView closeParen; // Good for location info

  CallExpr(UPtr<Expr> callee, Vec<UPtr<Expr>> args, TokenView closeParen)
      : callee(std::move(callee)), arguments(std::move(args)),
        closeParen(closeParen) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class MemberAccessExpr final : public Expr {
public:
  UPtr<Expr> object;
  TokenView opTok; // '.' or '->'
  TokenView member;
  bool isPointerAccess; // true if '->'

  MemberAccessExpr(UPtr<Expr> obj, TokenView opTok, TokenView member,
                   bool isPtr)
      : object(std::move(obj)), opTok(opTok), member(member),
        isPointerAccess(isPtr) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class IndexAccessExpr final : public Expr {
public:
  UPtr<Expr> array;
  UPtr<Expr> index;
  TokenView closeBracket; // Good for location info

  IndexAccessExpr(UPtr<Expr> arr, UPtr<Expr> idx, TokenView closeBracket)
      : array(std::move(arr)), index(std::move(idx)),
        closeBracket(closeBracket) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

class PostfixStepExpr final : public Expr {
public:
  UPtr<Expr> operand;
  TokenView opTok; // '++' or '--'
  bool isIncrement;

  PostfixStepExpr(UPtr<Expr> operand, TokenView opTok, bool increment)
      : operand(std::move(operand)), opTok(opTok), isIncrement(increment) {}

  void accept(ASTVisitor &visitor) override { visitor.visit(this); }
};

} // namespace Faith

#endif