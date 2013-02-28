module Main

open Ast

open System
open System.IO
open Microsoft.FSharp.Text.Lexing

//let ParseString text =
//    Parser.start Lexer.tokenstream (LexBuffer<char>.FromString text)

type Pang = 
    val baseEnv : Env
    new (input : TextReader, output : TextWriter) = { baseEnv = CoreEnv input output; }

    member this.ParseString (text : string) =
        // Read file
        let tree = Parser.start Lexer.tokenstream (LexBuffer<char>.FromString text)
        // Parse AST
        let results = List.map (ParseAst this.baseEnv) tree
        // Only return the result of the last expression
        List.rev results |> List.head
;;


//
//let test = ParseString "(define-macro (unless condition . body)
//                           `(if ,condition
//                                nil
//                                (begin ,@body)))
//
//                        (begin (display '(hello world)) (display 'the-end))
//                        (unless #f (display '(hello world)) (display 'the-end))"

// 

//let test = ParseString "(display `(,@(list 1 2 3) 4 5 6))
//                        (display `(1 2 3 ,@(list 4 5 6)))
//                        (display `(1 2 ,@(list 3 4) 5 6))"

//let test = ParseString "(begin (display 'hello) (display 'world))"

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
//List.map (ParseAst (CoreEnv null null)) test |> ignore

let pang = new Pang(null, null)
//pang.ParseString "(display '(hello world))" |> ignore
pang.ParseString "(let ((x 10) (y (list 1 2 3))) (begin (display x) (display y)))" |> ignore


printfn "Press any key to continue..."
System.Console.ReadLine() |> ignore


