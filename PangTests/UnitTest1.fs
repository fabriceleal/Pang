namespace ParsingTests

open Microsoft.VisualStudio.TestTools.UnitTesting    
open Parser

//let test = testString " (if (list 1 2 3)
//                            (display #T)
//                            (display NIL))
//                        (display \"hello world\")
//                        (display (+ 1 1))
//                        (display (/ 1 2 3))
//                        (display (* 2 (+ 1 1)))
//                        (display \"length:\")
//                        (display (length (list 1 2 3)))"

//let test = testString "(let ((x 10) (y 20) (z (+ 15 15)))
//                            (display (+ x y z)) )"

//let test = testString "(display
//                            (let ((x 10))
//                                (display (/ x 2))
//                                x) )"

//let test = testString "(display
//                            (let* ((x 10) (y (+ x 10)) (z (+ y 10))) 
//                                (+ x y z) ) )"

//let test = testString "(display '(hello world))
//                        (display (quote (hello world)))
//                        (display (list 'hello 'world))
//                        (display (cons 'hello (cons 'world nil)))"

//let test = testString "(define x 10)
//                        (display x)
//                        (set! x 20)
//                        (display x)"

//let test = testString "(define x (lambda (n) (+ n 12)))
//                        (display (x 12))
//                        (define y (lambda (n) (x n)))
//                        (set! x (lambda (n) (+ n 1)))
//                        (display (y 32))"



[<TestClass>]
type Test_UnitTest1() =

    [<TestMethod>]        
    member this.``Should display (1 . 2) and (1 2 3)`` () =
        Main.ParseString "(display (cons 1 2))
                    (display (cons 1 (cons 2 (cons 3 nil))))"  |> ignore

    [<TestMethod>]
    member this.``Quasiquotation test`` () =
        Main.ParseString "(define x 'world)
                          (define y 'this)
                          (display `(hello ,x (,y is ,(+ 1 2 3))))" |> ignore
        
    [<TestMethod>]
    member this.``Wikipedia test`` () =
        Main.ParseString "(display (define f 10))
                        (display (set! f (+ f f 6)))
                        (set! f (lambda (n) (+ n 12)))
                        (display (f 6))
                        (display (set! f (f 1)))
                        (display (apply + '(1 2 3 4 5 6)))
                        (set! f (lambda(n) (+ n 100)))
                        (display (map f '(1 2 3)))" |> ignore

    [<TestMethod>]
    member this.``Map test`` () =
        Main.ParseString "(define x (lambda (n) (+ n 1)))
                        (display (map x '(1 2 3 4 5)))" |> ignore

    [<TestMethod>]
    member this.``Apply test`` () =
        Main.ParseString "(define x (lambda () (display 1)))
                        (x)
                        (display ((lambda (n) (+ n n)) 1))
                        (display (apply (lambda (a b c) (+ a b c)) '(1 2 3)))" |> ignore

    [<TestMethod>]
    member this.``Another apply test`` () =
        Main.ParseString "(display (apply + '(1 2)))"

    [<TestMethod>]
    member this.``Factorial test`` () =
        Main.ParseString  "(define factorial (lambda (n)
                                            (if (eq n 0)
                                                1
                                                (* n (factorial (- n 1))))))
                            (display (factorial 5))"