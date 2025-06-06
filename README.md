# CHS Lexer

A fast and flexible lexer for building programming language tools, compilers, or interpreters in Rust.

Usage:
```rust
fn main(){
  let mut lexer = Lexer::new("Hello, world");
  loop {
    let token = lexer.next_token();
    if token.is_eof() {
      break;
    }
    println("{token}");
  }
}
```
