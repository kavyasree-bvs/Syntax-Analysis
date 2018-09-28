# Syntax-Analysis

CSCE 434 (Compiler Design) Course Project

By: Venkata Satya Kavya Sree Bondapalli

Semester: Fall 2018

Instructor: Glen Hordemann

School: Texas A&M University

A Parser for the Decaf programming language in Bison to handle the Syntax Analysis phase, the second task of the front-end.

The parser.y, scanner.l and the ast files overall provides an idea how the parser works.

The parser reads Decaf source programs and constructs a parse tree. If no syntax errors were found, the compiler will print the completed parse tree at the end of parsing. The language supports global variables and functions, classes and interfaces, variables of various types including arrays and objects, arithmetic and boolean expressions, constructs such as if, while, etc. 

Additionally, extended the language constructs to support C-style post-increment and decrement expressions along with switch statements.
