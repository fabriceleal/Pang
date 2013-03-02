module Ast

open System
open System.IO
open System.Collections.Generic
open AstObject
open Env
open Microsoft.FSharp.Text.Lexing

let rec Last = 
    function
    | Cons(x, NIL) -> x
    | Cons(_, t) -> Last t
    | a -> failwith "Expected a cons cell!"

// 
// We already receive the arguments as a cons list,
// just return the arguments :)
let SysList (args: SObject) : SObject =
    args;;

let SysCons (args: SObject) : SObject =
    match args with
    | Cons(car, Cons(cdr, NIL)) ->
        Cons(car, cdr)
    | _ -> failwith "cons expects two arguments!";;


let rec SysLength (args: SObject) : SObject =
    match args with
    | Cons(to_length, NIL) ->
        match to_length with
        | NIL -> Number(0.0)
        | Cons(_, tail) ->
            // We need to wrap the argument in another cons
            match SysLength(Cons(tail, NIL)) with
            | Number(i) -> Number(1.0 + i)
            | _ -> failwith "length should return number!"
        | _ -> failwith "length expects a cons cell or nil!"
    | _ -> failwith "length expects one argument!";;


let SysCar (args: SObject) : SObject =
    match args with
    | Cons(to_car, NIL) ->
        match to_car with
        | Cons(car, _) -> car
        | _ -> failwith "car expects a cons cell!"
    | _ -> failwith "car expects one argument!";;


let SysCdr (args: SObject) : SObject =
    match args with
    | Cons(to_cdr, NIL) ->
        match to_cdr with
        | Cons(_, cdr) -> cdr
        | _ -> failwith "cdr expects a cons cell!"
    | _ -> failwith "cdr expects one argument!";;


// The Console.Out property is changed only once this
// is called. You can overide it as you want
let MakeSysDisplay (newTextWriter : TextWriter) : SObject -> SObject = 
    // Change default out 
    match newTextWriter with
    | null -> ignore()
    | _ -> Console.SetOut(newTextWriter)

    let __SysDisplay (args : SObject) =
        match args with
        | Cons(to_display, NIL) ->
            let displayed = to_display.ToString()            
            Console.WriteLine displayed |> ignore
            String(displayed)
        | _ -> failwith "display expects one argument!"
    __SysDisplay;;


// This does not evaluate exactly 
// like common lisp
let SysArith (name : String) (nullelement : float) (func : float -> float -> float) (args: SObject) : SObject =
    let rec __SysArith (args: SObject) = 
        match args with
        | Cons(NIL, NIL) ->
            Number(nullelement)
        | Cons(Number(add1), NIL) ->
            Number(add1)        
        | Cons(Number(add1), tail) -> 
            match __SysArith tail with
            | Number(v) -> Number(func add1 v)
            | _ ->  failwith ("Invalid return of " + name)
        | _ -> failwith ("Invalid arguments to " + name)
    
    __SysArith args;;

let SysDiv = SysArith "/" 1.0 (/);;

let SysMult = SysArith "*" 1.0 (*);;

let SysSub = SysArith "-" 0.0 (-);;

let SysAdd = SysArith "+" 0.0 (+);;

let ``Sys=`` (args : SObject) = 
    match args with
    | Cons(arg1, Cons(arg2, NIL)) ->
        match arg1, arg2 with
        | Number(i1), Number(i2) when i1 = i2 -> True
        | String(s1), String(s2) when s1 = s2 -> True
        | Atom(a1), Atom(a2) when a1 = a2 -> True
        // TODO put here more stuff!
        | _, _ -> False
    | _ -> failwith "Expecting 2 arguments!";;


let SysApply (args : SObject) =
    match args with
    | Cons(fun_to_call, arguments) ->
        match fun_to_call with
        | Function(fn) -> 
            match arguments with
            | Cons(args, NIL) -> fn(args)
            | _ -> failwith "Expecting a list with all the args!"
        | _ -> failwith "Expecting a function!"
    | _ -> failwith "apply expects 2 arguments!";;
    

let SysMap (args : SObject) = 
    match args with
    | Cons(fun_to_call, Cons(list, NIL)) -> 
        match fun_to_call with
        // If we want this to adhere to our convention of
        // every function receiving a cons cell, we need to
        // treat our list!
        | Function(fn) ->
            let treated = list.ConsMap (fun x -> Cons(x, NIL)) 
            treated.ConsMap fn
        | _ -> failwith "Expecting a function!"
    | _ -> failwith "Expecting 2 arguments!";;


let ``SysNull?`` (args : SObject) =
    match args with
    | Cons(arg, NIL) ->
        match arg with
        | NIL -> True
        | _ -> False
    | _ -> failwith "Expecting 1 argument!";;


let SysRead (args : SObject) = 
    match args with
    | NIL ->
        // Read from standard input
        let input = Console.In
        Parser.Sexpr Lexer.tokenstream (LexBuffer<char>.FromTextReader input)
    | _ -> failwith "Expecting no arguments!";;

let SysReadChar (args : SObject) =
    match args with
    | NIL ->
        // Read a single char
        let buffer = [| new Char() |]
        let read = Console.In.Read(buffer, 0, 1)
        if read <> 1 then
            failwith "Error reading char!"
        else
            Char(buffer.[0])
    | _ -> failwith "Expecting no arguments!";;


let SysReadLine (args : SObject) =
    match args with
    | NIL ->
        // Read a line
        String(Console.In.ReadLine())
    | _ -> failwith "Expecting no arguments!";;

// Type testing

let ``SysBoolean?`` (args : SObject) =
    False

let ``SysChar``(args : SObject) =
    False

let ``SysPair?`` (args : SObject) =
    False

let ``SysProcedure?`` (args : SObject) =
    False

let ``SysSymbol?`` (args : SObject) =
    False

let ``SysByteVector?`` (args : SObject) =
    False

let ``SysEofObject?`` (args : SObject) =
    False

let ``SysNumber?`` (args : SObject) =
    False

let ``SysPort?`` (args : SObject) =
    False

let ``SysString?`` (args : SObject) =
    False

let ``SysVector?`` (args : SObject) =
    False

// Makes the core environment for our language
let CoreEnv newIn newOut =
    let e = new Env();
    e.Put("display", Function(MakeSysDisplay newOut)).
      Put("+", Function(SysAdd)).
      Put("*", Function(SysMult)).
      Put("-", Function(SysSub)).
      Put("/", Function(SysDiv)).
      Put("car", Function(SysCar)).
      Put("cdr", Function(SysCdr)).
      Put("list", Function(SysList)).
      Put("length", Function(SysLength)).
      Put("cons", Function(SysCons)).
      Put("=", Function(``Sys=``)).
      Put("apply", Function(SysApply)).
      Put("map", Function(SysMap)).
      Put("read", Function(SysRead)).
      Put("read-char", Function(SysReadChar)).
      Put("read-line", Function(SysReadLine)).
      Put("null?", Function(``SysNull?``));;


let rec AppendForSplice (cons : SObject) (tail : SObject) =
    match cons with
    | Cons(h, NIL) -> Cons(h, tail)
    | Cons(h, t) -> Cons(h, AppendForSplice t tail)
    | _ -> failwith "Unexpected object in AppendForSplice!";;


// Let this fail if arguments are invalid
let rec PrintSexp = function  
    // Im too lazy ...
    | Syntax(_) -> "Syntax *"
    | UnquoteSplicing(sexpr) -> 
        String.Format(",@{0}", PrintSexp sexpr)
    // OK
    | Quasiquote(sexpr) ->
        String.Format("`{0}", PrintSexp sexpr)
    | Unquote(sexpr) ->
        String.Format(",{0}", PrintSexp sexpr)     
    | Quote(sexpr) ->
        String.Format("'{0}", PrintSexp sexpr)
    | Cons(head, tail) -> 
        String.Format("Cons({0}, {1})", PrintSexp head, PrintSexp tail)
    | Atom(name) -> String.Format("<{0}>", name)                 
    | Number(n) -> String.Format("{0}", n)
    | String(s) -> String.Format("\"{0}\"", s)
    | Function(_) -> "Native Function"
    | True -> "#True"
    | False -> "#False"
    | NIL -> "NIL";; 


let PrintTree tree = 
    let newline = Environment.NewLine
    let rec __PrintTree indent tree = 
        match tree with
        | Cons(a, b) ->
            String.Format("{0}Cons{1}", System.String('.', indent), newline) +
            String.Format("{0}{1}", System.String('.', indent), __PrintTree (indent + 1) a) +
            String.Format("{0}{1}", System.String('.', indent), __PrintTree (indent + 1) b)
        | t -> 
            String.Format("{0}{1}{2}", System.String('.', indent), PrintSexp t, newline)
    //--
    __PrintTree 0 tree;;


let rec ParseAst (env : Env) ast =
    match ast with
    | Quasiquote(sexpr) ->
        // Find all unquotes inside sexpr, 
        // parse them. Any unquote outside a quasiquote is invalid!!!
        let rec WalkQuasiquote x =
            match x with
            // Here looks the same, but the container (hopefully, a cons(_, _))
            // must be carefull!
            | UnquoteSplicing(sexpr) | Unquote(sexpr) -> ParseAst env sexpr
            // Constructs
            | Cons(a, b) -> 
                //Cons(WalkQuasiquote a, WalkQuasiquote b)
                // We need to *know* that the 
                // result of a UnquoteSplicing is a consed list
                match a, b with
                | UnquoteSplicing(_), UnquoteSplicing(_) ->
                    failwith "Implement (a, b) match (UnquoteSplicing(_), UnquoteSplicing(_))"
                | UnquoteSplicing(_), _ -> 
                    let head_list = WalkQuasiquote(a) 
                    match head_list with
                    | Cons(_, _) -> AppendForSplice head_list (WalkQuasiquote b)
                    |_ -> failwith "Result of UnquoteSplicing should be a list!"
                | _, UnquoteSplicing(_) -> 
                    failwith "Implement (_, b) match (_, UnquoteSplicing(_))"
                | _, _ -> 
                    // do regular stuff ...
                    Cons(WalkQuasiquote a, WalkQuasiquote b)
            // Return everything else as-is
            | x -> x
        WalkQuasiquote sexpr
    | Quote(sexpr) -> sexpr
    | Cons(Atom("let*"), args) ->
        match args with
        | Cons(bindings, sexpr) -> 
            // Create a lambda / application for each binding
            // ((lambda (id-1) X) value-1)
            // X = ((lambda (id-2) Y) value-2)
            // Y = sexpr

            let rec transformBindings binds = 
                match binds with
                | Cons(pair, NIL) ->
                    match pair with
                    | Cons(id, Cons(value, NIL)) ->
                        Cons(Cons(Atom("lambda"), Cons(Cons(id, NIL), Cons(Cons(Atom("begin"), sexpr), NIL))), Cons(value, NIL))
                    | _ -> failwith "Unexpected pair!"
                | Cons(pair, tail) ->
                    match pair with
                    | Cons(id, Cons(value, NIL)) ->
                        Cons(Cons(Atom("lambda"), Cons(Cons(id, NIL), Cons(transformBindings tail, NIL))), Cons(value, NIL))
                    | _ -> failwith "Unexpected pair!"
                | _ -> failwith "Unexpected bindings!"
                            
            let transformed = transformBindings bindings            
            ParseAst env transformed
        | _ -> failwith "Invalid arguments to let*!"
    | Cons(Atom("let"), args) ->
        match args with
        | Cons(bindings, sexpr) ->
            // Create a lambda. let is a non-native special-form
            // so we will implement it with simpler native forms
            
            // First we need to split bindings into two lists:
            // - one with the arguments' names
            // - another with the actual values
            let args = ref NIL
            let values = ref NIL

            bindings.MapDiscard (fun pair ->
                match pair with
                | Cons(id, Cons(value, NIL)) -> 
                    args := AppendCons !args id
                    values := AppendCons !values value
                | _ -> failwith "Unexpected pair!"
            ) |> ignore

            // If we have args, wrap everything with a lambda
            // Otherwise, just execute sexpr
            match !args with
            | NIL -> 
                ParseAst env sexpr
            | _ -> 
                // create a lambda cons list
                let as_lambda = Cons(Atom("lambda"), Cons(!args, Cons(Cons(Atom("begin"), sexpr), NIL)))
                // and the use it for a function application
                let transformed = Cons(as_lambda, !values)        
                // and then we parse it!
                ParseAst env transformed

        | _ -> failwith "Invalid arguments to let!"        
    | Cons(Atom("begin"), ls) ->
        ls.ConsMap (ParseAst env) |> Last
    | Cons(Atom("set!"), args) ->
        match args with
        | Cons(id, Cons(sexpr, _)) ->
            match id with
            | Atom(name) -> 
                let parsed = ParseAst env sexpr
                env.Change(name, parsed) |> ignore
                parsed
            | _ -> failwith "Invalid set!"
        | _ -> failwith "Invalid arguments to set!"
    | Cons(Atom("define-macro"), args) ->
        match args with
        | Cons(macro_args, Cons(body, NIL)) -> 
            match macro_args with
            | Cons(id, args) -> 
                match id with
                | Atom(name) -> 
                    // Create a Syntax object
                    let parse = Syntax(args, body)
                    // Bind that to the name
                    env.Put(name, parse) |> ignore
                    parse
                | _ -> failwith "Name should be an atom!"
            | _ -> failwith "Expected name of the macro!"
        | _ -> failwith "Invalid args to define-macro"
    | Cons(Atom("define"), args) ->
        // Special form define
        match args with
        | Cons(identifier, Cons(exp, NIL)) ->
            match identifier with
            | Atom(name) ->
                let parse = ParseAst env exp
                env.Put(name, parse) |> ignore
                parse
            | _ -> failwith "Invalid identifier for define!"
        | _ -> failwith "Invalid args for define!"
    | Cons(Atom("lambda"), ls) ->
        // Lambda special form
        match ls with
        | Cons(arguments, Cons(body, NIL)) ->
            // Wrap executon of body in a function
            // We can create a copy of the current env
            // so we dont forget the values of vars
            // that might be changed
            //let env_for_fun = env.Copy()
            let env_for_fun = env
            Function(fun x ->
                // x holds already evaled arguments
                // create a new environment

                // we copy the captured environment
                // to avoid the need to UnWrap() and
                // to allow reentrant functions
                let new_env = env_for_fun.Copy().Wrap()
                arguments.ZipDiscard x (fun name sexpr -> 
                    match name with
                    | Atom(name) -> new_env.Put(name, sexpr) |> ignore
                                    sexpr
                    | _ -> failwith "Invalid argument!")
                // Parse body with the new environment
                ParseAst new_env body)
        | _ -> failwith "Invalid arguments to lambda!"
    | Cons(Atom("if"), args) ->
        match args with
        | Cons(_condition, Cons(_true_branch, Cons(_false_branch, NIL))) ->
            match ParseAst env _condition with
            | False -> ParseAst env _false_branch
            | _ -> ParseAst env _true_branch
        | _ -> failwith "Invalid args to if!"
    // Function application
    | Cons(_fn, _args) ->
        let _fn = ParseAst env _fn
        match _fn with
        | Function(native) ->
            _args.ConsMap (ParseAst env) |> native
        | Syntax(args, body) ->
            // Create new env., 
            // bind *unparsed* arguments
            let new_env = env.Copy().Wrap()
            
            let rec parseMacroArgs args pars =
                // args contains the names of the vars 
                // pars contains the values of the vars               
                match args with                
                | Cons(args_head, args_tail) ->
                    // args_head will contain the var to bind
                    // t has:
                    // - the tail, if args is a list
                    // - the 'rest', if args is a dotted-pair
                    match args_tail with                    
                    | Cons(_, _) ->
                        // t is a list,
                        // so args is a list
                        match pars with
                        | Cons(pars_head, pars_tail) ->
                            match args_head with
                            | Atom(a) ->
                                new_env.Put(a, pars_head) |> ignore
                                parseMacroArgs args_tail pars_tail
                            | _ -> failwith ""
                        | _ -> failwith ""
                    | _ ->
                        // t is not a list,
                        // so args is a dotted pair
                        match pars with
                        | Cons(pars_head, pars_tail) ->
                            match args_tail with
                            | Atom(t) -> 
                                match args_head with
                                | Atom(a) ->
                                    new_env.Put(a, pars_head) |> ignore
                                | _ -> failwith ""
                                new_env.Put(t, pars_tail) |> ignore
                            | _ -> failwith "not what was expected!"                       
                        | _ -> failwith "not what was expected!"                     
                | _ -> failwith "not what was expected!"

            parseMacroArgs args _args

            // Create the tree
            let parsed = ParseAst new_env body

            // And parse it!
            ParseAst env parsed
        | _ -> failwith "Expected a function!"
    | Unquote(_) -> failwith "Unquote is only valid inside a quasiquote!"
    | Atom(name) -> env.Lookup(name)    
    // Scalar literals evaluate to themselves
    | String(_) | Number(_) | NIL | True | False -> ast
    | _ -> failwith "Error!";;
    
