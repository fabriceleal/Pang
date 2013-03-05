module Main

open AstObject
open Ast
open Env
open ParseAst

open System
open System.IO
open Microsoft.FSharp.Text.Lexing

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

    member this.ParseStringCPS (text : string) =
        let baseEnv = CoreEnv this.input this.output;
        // Read file
        let tree = Parser.Start Lexer.tokenstream (LexBuffer<char>.FromString text)
        // Parse AST
        for e in tree do
            ParseAstCPS baseEnv e (fun x -> ignore())

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
//pang.ParseString "(let ((x 10) (y (list 1 2 3))) (begin (display x) (display y)))" |> ignore
//pang.ParseString "(define-macro (unless condition true-return . body)
//                           `(if ,condition
//                                ,true-return
//                                (begin ,@body)))
//
//                        (begin (display '(hello world)) (display 'the-end))
//                        (unless #f nil (display '(hello world)) (display 'the-end))" |> ignore
//pang.ParseString "(let* ((x 10) (y (+ x x))) (display y))" |> ignore
//pang.ParseString "(display '(hello . (world . nil)))
//                    (display '(hello world))
//                    (display (list 'hello 'world))" |> ignore

//pang.ParseString "
//#|
//    The FACT procedure computes the factorial
//    of a non-negative integer.
//|#
//(define fact
//    (lambda (n)
//        (if (= n 0)
//            ;(= n 1)
//            1 ;Base case: return 1
//            (* n (fact (- n 1))))))
//(display (fact 4))
//(display (fact 8))
//(display (fact 15))
//" |> ignore

//SysRead(Cons(SObject.String("qwe"), NIL)) |> (fun x -> x.ToString()) |> Console.WriteLine

//pang.ParseString "
//
//(display \"Write something:\r\n\")
//(write (read))
//
//" |> ignore

//pang.ParseStringCPS "
//(if #t #f #t)
//"
//
//pang.ParseStringCPS "
//(let ((x 10) (y 20) (z 30))
//    (display x)
//    (display y)
//    (display z))
//"
//
//pang.ParseStringCPS "
//(display (length '()))
//(display (length '(1)))
//(display (length '(1 2 3)))
//"
//
//pang.ParseStringCPS "
//(define factorial (lambda (n)
//                    (if (= n 0)
//                        1
//                        (* n (factorial (- n 1))))))
//(display (factorial 5))
//(display (factorial 8))
//"
//
//pang.ParseStringCPS "
//(let* ((x 1) (y (* x 2)) (z (- y -2)))
//    (display x)
//    (display y)
//    (display z))
//"
//
//pang.ParseStringCPS "
//(let* ((x 'hello) (y 'world) (z `(i will print ,x ,y)))
//    (display x)
//    (display y)
//    (display z))
//"

//pang.ParseStringCPS "
//(let* ((x 'hello) (y 'world) (z `(i will print ,x ,y)) )
//    (display x)
//    (display y)
//    (display z)
//    (display (display z)))
//"

//pang.ParseStringCPS "
//(define-macro (unless condition true-return . body)
//    `(if ,condition
//        ,true-return
//        (begin ,@body)))
//
//(begin (display '(hello world)) (display 'the-end))
//(unless #f nil (display '(hello world)) (display 'the-end))"

//pang.ParseStringCPS "
//(define (factorial n)
//            (if (= n 0)
//                1
//                (* n (factorial (- n 1))
//                ) 
//            )
//)
//(display (factorial 5))
//(display (factorial 8))
//"

//pang.ParseStringCPS "
//
//(display (- 1 (call/cc (lambda (return) (begin 1 (return 2) 3)))))
//
//"

//pang.ParseStringCPS "
//
//(display (begin 1 2 3))
//
//"

//pang.ParseStringCPS "
//
//(define return #f) 
//  
//(display (+ 1 (call/cc 
//        (lambda (cont) 
//            (set! return cont) 
//            1))))
//
//(display (return 22))
//
//"

//pang.ParseStringCPS "
//
//(let* ((yin
//         ((lambda (cc) (begin (display \"@\") cc)) (call/cc (lambda (c) c))))
//       (yang
//         ((lambda (cc) (begin (display \"*\") cc)) (call/cc (lambda (c) c)))))
//    (yin yang))
//
//"

pang.ParseStringCPS "
((lambda (yin) 
      ((lambda(yang) (yin yang))
          ((lambda (cc) (display \"*\") cc) (call/cc (lambda (c) c)))
      )
) ((lambda (cc) (display \"@\") cc) (call/cc (lambda (c) c))))
"

printfn "Press any key to continue..."
System.Console.ReadLine() |> ignore


