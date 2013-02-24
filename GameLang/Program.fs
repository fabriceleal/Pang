module Main

open Ast

open System
open System.IO
open Microsoft.FSharp.Text.Lexing

let testString text =
    Parser.start Lexer.tokenstream (LexBuffer<char>.FromString text)

let test = testString " (print \"hello world\")  "

printfn "Printing AST ..."
List.map (printfn "%s") (List.map PrintSexp test) |> ignore

printfn "Executing code ..."
List.map (ParseAst (CoreEnv())) test |> ignore

printfn "Press any key to continue..."
System.Console.ReadLine() |> ignore


