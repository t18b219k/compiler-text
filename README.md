# [CoreML](https://github.com/AtsushiOhori/compiler-text) to WebAssembly compiler

## Improved points 
   
   * type inference module emits typed syntax tree called TypedSyntax
   * compiling phase now take TypedSyntax as input 
   * better error reporting 
      * missing identifier 
      * various type errors
      * infinite size type 
      * function applycation to invalid expression
   * generate repl debug info for Wasm



