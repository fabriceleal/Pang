module Main

open Ast

open System
open System.IO
open Microsoft.FSharp.Text.Lexing

let ParseString text =
    Parser.start Lexer.tokenstream (LexBuffer<char>.FromString text)


let test = ParseString ""

//printfn "displaying AST ..."
//List.map (printfn "%s") (List.map displaySexp test) |> ignore

printfn "Executing code ..."
List.map (ParseAst (CoreEnv())) test |> ignore


printfn "Press any key to continue..."
System.Console.ReadLine() |> ignore


