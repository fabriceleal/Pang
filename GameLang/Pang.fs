module Pang

open System.IO
open Microsoft.FSharp.Text.Lexing

open AstObject
open Runtime
open Env
open ParseAst
open Runtime

//let ParseString text =
//    Parser.start Lexer.tokenstream (LexBuffer<char>.FromString text)

type Pang = 
    val input : TextReader
    val output : TextWriter
    new (input : TextReader, output : TextWriter) = { input = input; output = output; }

//    member this.ParseString (text : string) =
//        // Read file
//        let tree = Parser.Start Lexer.tokenstream (LexBuffer<char>.FromString text)
//        // Parse AST
//        let results = List.map (ParseAst this.baseEnv) tree
//        // Only return the result of the last expression
//        List.rev results |> List.head

    member this.ParseStringCPS (k : SObject -> unit) (text : string) =
        let baseEnv = CoreEnv this.input this.output;
        // Read file
        let tree = Parser.Start Lexer.tokenstream (LexBuffer<char>.FromString text)
        // Parse AST
        ParseAstCPS baseEnv tree k
;;