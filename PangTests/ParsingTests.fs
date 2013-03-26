namespace ParsingTests

open System
open System.IO
open System.Reflection
open System.Text

open System.Diagnostics

open Microsoft.VisualStudio.TestTools.UnitTesting    
open Pang


[<TestClass>]
type Test_UnitTest1() =

    // 
    member this.RunTest code expectedOutput =
        try
            // Setup a "meta-StdOut"
            let sb = new StringBuilder()
            use out = new StringWriter(sb)
            let pang = new Pang(null, out)
            pang.ParseStringCPS ignore code
            let actualOutput = sb.ToString()
            // Print StdOut to somewhere where we can read
            DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss.fff") |> Debug.Print 
            Debug.Print actualOutput
            Assert.AreEqual(expectedOutput, actualOutput, "Output is not what I expected!")
            out.Close()
        with
            | _ as ex ->
                Debug.Print ex.Message
                Assert.Fail ex.Message

    [<TestMethod>]        
    member this.``Should display (1 . 2) and (1 2 3)`` () =
        this.RunTest "(display (cons 1 2))
                      (display (cons 1 (cons 2 (cons 3 nil))))" "(1 . 2)\r\n(1 2 3)\r\n"
        

    [<TestMethod>]
    member this.``Quasiquotation test`` () =
        this.RunTest "(define x 'world)
                      (define y 'this)
                      (display `(hello ,x (,y is ,(+ 1 2 3))))" "(hello world (this is 6))\r\n"
        
    [<TestMethod>]
    member this.``Wikipedia test`` () =
        this.RunTest "(display (define f 10))
                      (display (set! f (+ f f 6)))
                      (set! f (lambda (n) (+ n 12)))
                      (display (f 6))
                      (display (set! f (f 1)))
                      (display (apply + '(1 2 3 4 5 6)))
                      (set! f (lambda(n) (+ n 100)))
                      (display (map f '(1 2 3)))" "10\r\n26\r\n18\r\n13\r\n21\r\n(101 102 103)\r\n" |> ignore

    [<TestMethod>]
    member this.``Map test`` () =
        this.RunTest "(define x (lambda (n) (+ n 1)))
                      (display (map x '(1 2 3 4 5)))" "(2 3 4 5 6)\r\n" |> ignore

    [<TestMethod>]
    member this.``Apply test`` () =
        this.RunTest "(define x (lambda () (display 1)))
                      (x)
                      (display ((lambda (n) (+ n n)) 1))
                      (display (apply (lambda (a b c) (+ a b c)) '(1 2 3)))" "1\r\n2\r\n6\r\n" |> ignore

    [<TestMethod>]
    member this.``Another apply test`` () =
        this.RunTest "(display (apply + '(1 2)))" "3\r\n" |> ignore

    [<TestMethod>]
    member this.``Factorial test`` () =
        this.RunTest  "(define factorial (lambda (n)
                                            (if (= n 0)
                                                1
                                                (* n (factorial (- n 1))))))
                       (display (factorial 5))" "120\r\n" |> ignore

    [<TestMethod>]
    member this.``Closures test`` () =
        this.RunTest  "(define x (lambda (n) (+ n 12)))
                       (display (x 12))
                       (define y (lambda (n) (x n)))
                       (set! x (lambda (n) (+ n 1)))
                       (display (y 32))" "24\r\n33\r\n" |> ignore

    [<TestMethod>]
    member this.``Set! test`` () =
        this.RunTest "(define x 10)
                      (display x)
                      (set! x 20)
                      (display x)" "10\r\n20\r\n" |> ignore

    [<TestMethod>]
    member this.``Hello World test`` () =
        this.RunTest "(display '(hello world))
                      (display (quote (hello world)))
                      (display (list 'hello 'world))
                      (display (cons 'hello (cons 'world nil)))" "(hello world)\r\n(hello world)\r\n(hello world)\r\n(hello world)\r\n" |> ignore

    [<TestMethod>]
    member this.``Let* test`` () =
        this.RunTest "(display
                            (let* ((x 10) (y (+ x 10)) (z (+ y 10))) 
                                (+ x y z) ) )" "60\r\n" |> ignore
                                
    [<TestMethod>]
    member this.``If test`` () =
        this.RunTest " (if (list 1 2 3)
                            (display #T)
                            (display NIL))
                       (display \"hello world\")
                       (display (+ 1 1))
                       (display (/ 1 2 3))
                       (display (* 2 (+ 1 1)))
                       (display \"length:\")
                       (display (length (list 1 2 3)))" "T\r\n\"hello world\"\r\n2\r\n1,5\r\n4\r\n\"length:\"\r\n3\r\n" |> ignore

    [<TestMethod>]
    member this.``Let test`` () =
        this.RunTest "(let ((x 10) (y 20) (z (+ 15 15)))
                            (display (+ x y z)) )" "60\r\n" |> ignore

    [<TestMethod>]
    member this.``Display test`` () =
        this.RunTest "(display
                            (let ((x 10))
                                (display (/ x 2))
                                x) )" "5\r\n10\r\n" |> ignore

    [<TestMethod>]
    member this.``Begin test`` () =
        this.RunTest "(begin
                      (define x 10)
                      (display x)
                      (define y 20)
                      (display y)
                      (+ x y))" "10\r\n20\r\n" |> ignore
;;
