module Main

open Ast

open System
open System.IO
open Microsoft.FSharp.Text.Lexing

let testString text =
    Parser.start Lexer.tokenstream (LexBuffer<char>.FromString text)

let test = testString " (print \"hello world\")  (print (+ 1 1)) (print (/ 1 2 3)) (print (* 2 (+ 1 1))) (print \"length:\") (print (length (list 1 2 3)))"

printfn "Printing AST ..."
List.map (printfn "%s") (List.map PrintSexp test) |> ignore

printfn "Executing code ..."
List.map (ParseAst (CoreEnv())) test |> ignore


printfn "Press any key to continue..."
System.Console.ReadLine() |> ignore


