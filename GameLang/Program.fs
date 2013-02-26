module Main

open Ast

open System
open System.IO
open Microsoft.FSharp.Text.Lexing

let ParseString text =
    Parser.start Lexer.tokenstream (LexBuffer<char>.FromString text)

let test = ParseString "(define-macro (or . args)
                           (if (null? args) ; zero arg or?
                                #f
                                (if (null? (cdr? arg-exprs)) ; one arg or?
                                   (car arg-exprs)          
                                   `(let ((temp ,(car arg-exprs)))
                                       (if temp
                                           temp
                                           (or ,@(cdr arg-exprs)))))))  "


//(if (null? (cdr? arg-exprs)) ; one arg or?
//                                   (car arg-exprs)          
//                                   `(let ((temp ,(car arg-exprs)))
//                                       (if temp
//                                           temp
//                                           (or ,@(cdr arg-exprs)))))

//let test = ParseString "(define-macro (or . args)
//                           (if (null? args)  ; zero arg or?
//                               #f
//                               (if (null? (cdr? arg-exprs)) ; one arg or?
//                                   (car arg-exprs)          
//                                   `(let ((temp ,(car arg-exprs)))
//                                       (if temp
//                                           temp
//                                           (or ,@(cdr arg-exprs)))))))"

//let test = ParseString "(display 1) ; this is a comment
//                        "

//let test = ParseString "(display 1) ; this is a comment"

//printfn "displaying AST ..."
//List.map (printfn "%s") (List.map displaySexp test) |> ignore

printfn "Executing code ..."
List.map (ParseAst (CoreEnv())) test |> ignore


printfn "Press any key to continue..."
System.Console.ReadLine() |> ignore


