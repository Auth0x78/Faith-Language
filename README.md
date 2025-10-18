# 🕊️ Faith Language Compiler

> A lightweight, C-inspired systems programming language designed for simplicity, determinism, and safety — built from scratch in C++.

---

## 🌟 Overview

**Faith** is a statically typed, compiled programming language inspired by C, Zig, and Rust.  
Its primary goal is to combine the **clarity of C**, the **error safety of Zig**, and the **expressiveness of modern design** — while staying close to the metal.

This repository contains the **compiler front-end** and will eventually include:

- Lexer and Parser (custom or ANTLR-based)
- AST representation
- Semantic analyzer and type checker
- IR generator and backend code emitter (targeting C or LLVM IR)
- Standard library and runtime utilities

---

## ⚙️ Current Status

| Component         | Status         | Description                                         |
| ----------------- | -------------- | --------------------------------------------------- |
| Lexer             | ✅ In Progress | Tokenizes Faith source into lexemes                 |
| Parser            | 🔧 Partial     | Grammar defined (BNF + ANTLR versions)              |
| AST               | 🔜 Planned     | To represent parsed syntax tree                     |
| Semantic Analysis | 🔜 Planned     | Type checking and symbol resolution                 |
| IR/Codegen        | 🔜 Planned     | Output backend for C/LLVM IR                        |
| Runtime           | 🔜 Planned     | Small runtime layer for error handling, defer, etc. |

---

## 📜 Language Features (current draft)

- **C-like syntax** with type inference and static typing.
- **Explicit mutability** via `let` (mutable) and `const` (immutable).
- **Pointers and references** using `*` and `&` syntax.
- **Error-aware types** using `!Type` (similar to Zig’s error unions).
- **Structs with forward declarations** and initialization syntax.
- **Functions with explicit return types:**

```c
  func add(a: i32, b: i32) -> i32 {
      return a + b;
  }
```

- **Function pointers and array types.**
- **Defer keyword** for scope-based cleanup before early exits.
- **Switch/Match expressions** for compile-time and value-based branching.
- **Entry point:** special `main` function is invoked automatically at runtime.

---

## 🧠 Example Code

```c
struct Point {
    x: f32;
    y: f32;
};

func distance(p1: &Point, p2: &Point) -> f32 {
    let dx: f32 = p2->x - p1->x;
    let dy: f32 = p2->y - p1->y;
    return (dx * dx + dy * dy);
}

func main() -> i32 {
    let a: Point = Point { x: 1.0, y: 2.0 };
    let b: Point = Point { x: 4.0, y: 6.0 };
    defer print("Exiting...\n");
    print(distance(&a, &b));
    return 0;
}
```

---

## 🧩 Project Structure (Intended)

```
Faith/
 ├── src/
 │   ├── lexer.cpp         # Lexical analysis
 │   ├── parser/           # Parsing logic
 │   ├── ast/              # Abstract syntax tree nodes
 │   ├── semantic/         # Type checking, sym emitter
 │   ├── runtime/          # Built-in runtime features
 │   └── main.cpp          # Compiler entry
 ├── include/              # Public headers
 ├── resoruces             # Resource Files
 ├── grammar/
 │   └── Faith.bnf         # Human-readable grammar
 ├── tests/                # Test programs
 ├── examples/             # Example Faith programs
 ├── docs/                 # Design and notes
 └── README.md
```

---

## 🧰 Build Instructions

Faith is written in **C++23** and built with **CMake**.

```bash
# Clone repo
git clone https://github.com/Auth0x78/Faith-Language.git
cd Faith-Language

# Build
cmake -B build
cmake --build build

# Run the compiler
./build/faith <source.ft>
```

---

## 🧾 TODO — Personal Roadmap

### Lexer

- [x] Define token types (`TokenType`)
- [x] Implement scanning logic for keywords, literals, and operators
- [x] Add support for multi-line comments
- [x] Add support for multi-line strings
- [ ] Test support for multi-line strings
- [ ] Improve error reporting (with line and column tracking)

### Parser

- [x] Write BNF grammar
- [ ] Build manual recursive-descent parser version
- [ ] Implement struct parsing and initialization logic
- [ ] Add switch/match expression parsing

### AST & Semantic Analysis

- [ ] Define AST node types
- [ ] Implement symbol table for variables/functions
- [ ] Type checking and coercion rules
- [ ] Error handling for invalid operations
- [ ] Function overloads and extern declarations

### Code Generation

- [ ] Basic IR or C-code emission backend
- [ ] Integrate LLVM for optimized IR (optional)
- [ ] Support for function calls, structs, and defer

### Runtime

- [ ] Implement error union (`!type`) behavior
- [ ] Add minimal runtime library for IO and memory
- [ ] Add panic/defer runtime behavior

### Testing & Examples

- [ ] Unit tests for lexer and parser
- [ ] Create sample Faith programs
- [ ] Test struct, pointer, defer, and error handling features

---

## 💡 Design Notes

- The **`!Type`** system mimics Zig’s error unions for explicit error handling.
- The **`defer`** keyword ensures deterministic cleanup.
- Functions cannot be both `extern` and `static` (enforced by parser).
- The compiler front-end is modular — later backends can target C or LLVM IR.

---

## 🔒 Note

This repository is **private** and intended for personal compiler development and experimentation.
Not yet ready for public use or contribution.

---

## 🪶 Author

**Akshat Dighade**: Compiler Design, Systems Programming, and Language Engineering Enthusiast & Learner
