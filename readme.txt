==========================
M++ Compiler

James Sullivan
sullivan.james.f@gmail.com
==========================

    Compilation
    ===========
    
    make        -> Emit the binary ./mpp in the root directory. 
    make clean  -> Remove the binary and all compilation artifacts.

    Usage
    =====

    ./mpp file output 

    Emit the stack code for the given M++ source file to `output`. 
    If the file has any of the following three issues:
        1) Lexical errors - Unrecognized symbols, sequences, etc.
        2) Grammatical errors - Noncompliant code layout, ill-formed
            code, etc.
        3) Semantic errors - Type errors, undefined variables, etc.
    Then an error message is written to stdout and no file is produced.

Components
==============

Lexer : monadic lexer using Alex
   gettokens str -- Tokenize `str`, checking for lexical errors
Parser : bottom-up LALR(1) parser using Happy
    parse toks -- Parse `toks` to an AST, checking for grammar errors
Symbol Table : Chaining hash table with header that details levels and
               keeps track of the current constructor number
    sym_insert k t -- Insert symbol type `k`
    sym_lookup s t -- Lookup topmost definition of string `s`
    sym_empty      -- New symbol table
    sym_getlevel t -- Retrieve current level header
    sym_curlevel t -- Retrieve just the current level number
    sym_returntype t -- Retrieve the current return type
    sym_funscope_up t -- Increase function scope
    sym_blockscope_up t -- Increase block scope
Semantic Analyzer : See plumbing diagrams
    ir ast -- Generate IR for `ast`, checking for semantic errors
Code Generator : AM Stack machine code production
    gen_code ir -- Generate the stack machine code for `ir`.

