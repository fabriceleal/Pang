module Main

open Ast

open System
open System.IO
open Microsoft.FSharp.Text.Lexing

let testString text =
    Parser.start Lexer.tokenstream (LexBuffer<char>.FromString text)

//let test = testString " (if (list 1 2 3)
//                            (print #T)
//                            (print NIL))
//                        (print \"hello world\")
//                        (print (+ 1 1))
//                        (print (/ 1 2 3))
//                        (print (* 2 (+ 1 1)))
//                        (print \"length:\")
//                        (print (length (list 1 2 3)))"
//let test = testString "(print (cons 1 2))
//                        (print (cons 1 (cons 2 (cons 3 nil))))"
//let test = testString "(let ((x 10) (y 20) (z (+ 15 15)))
//                            (print (+ x y z)) )"
//let test = testString "(print
//                            (let ((x 10))
//                                (print (/ x 2))
//                                x) )"
let test = testString "(print
                            (let* ((x 10) (y (+ x 10)) (z (+ y 10))) 
                                (+ x y z) ) )"

//printfn "Printing AST ..."
//List.map (printfn "%s") (List.map PrintSexp test) |> ignore

printfn "Executing code ..."
List.map (ParseAst (CoreEnv())) test |> ignore


printfn "Press any key to continue..."
System.Console.ReadLine() |> ignore


