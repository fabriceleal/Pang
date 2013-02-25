﻿module Main

open Ast

open System
open System.IO
open Microsoft.FSharp.Text.Lexing

let testString text =
    Parser.start Lexer.tokenstream (LexBuffer<char>.FromString text)

//let test = testString " (if (list 1 2 3)
//                            (display #T)
//                            (display NIL))
//                        (display \"hello world\")
//                        (display (+ 1 1))
//                        (display (/ 1 2 3))
//                        (display (* 2 (+ 1 1)))
//                        (display \"length:\")
//                        (display (length (list 1 2 3)))"

//let test = testString "(display (cons 1 2))
//                        (display (cons 1 (cons 2 (cons 3 nil))))"

//let test = testString "(let ((x 10) (y 20) (z (+ 15 15)))
//                            (display (+ x y z)) )"

//let test = testString "(display
//                            (let ((x 10))
//                                (display (/ x 2))
//                                x) )"

//let test = testString "(display
//                            (let* ((x 10) (y (+ x 10)) (z (+ y 10))) 
//                                (+ x y z) ) )"

let test = testString "(display '(hello world))
                        (display (quote (hello world)))
                        (display (list 'hello 'world))
                        (display (cons 'hello (cons 'world nil)))"

//printfn "displaying AST ..."
//List.map (printfn "%s") (List.map displaySexp test) |> ignore

//Cons(SObject.Int(1), Cons(SObject.Int(2), Cons(SObject.Int(3), NIL))) |> ConsToString |> printfn "%s"
//Cons(SObject.Int(1), Cons(Cons(SObject.Int(2), SObject.Int(4)), Cons(SObject.Int(3), NIL))) |> ConsToString |> printfn "%s"
//Cons(Cons(SObject.Int(1), Cons(SObject.Int(2), Cons(SObject.Int(3), NIL))), Cons(Cons(SObject.Int(2), SObject.Int(4)), Cons(SObject.Int(3), NIL))) |> ConsToString |> printfn "%s"
//Cons(Cons(SObject.Int(1), Cons(SObject.Int(2), Cons(SObject.Int(3), NIL))), Cons(Cons(SObject.Int(2), SObject.Int(4)), Cons(SObject.Int(1), Cons(SObject.Int(2), Cons(SObject.Int(3), NIL))))) |> ConsToString |> printfn "%s"
//Cons(Cons(SObject.Int(1), Cons(SObject.Int(2), Cons(SObject.Int(3), NIL))), Cons(Cons(SObject.Int(2), SObject.Int(4)), Cons(Cons(SObject.Int(1), Cons(SObject.Int(2), Cons(SObject.Int(3), NIL))), NIL))) |> ConsToString |> printfn "%s"

printfn "Executing code ..."
List.map (ParseAst (CoreEnv())) test |> ignore


printfn "Press any key to continue..."
System.Console.ReadLine() |> ignore

