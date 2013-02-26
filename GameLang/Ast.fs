module Ast

open System
open System.Collections.Generic

// AST Element
type SObject =
    // Language Constructs
    | Quasiquote of SObject
    | Unquote of SObject
    | UnquoteSplicing of SObject
    | Let_Star of SObject * list<SObject>
    | Let of SObject * list<SObject>
    | If of SObject * SObject * SObject
    | Define of SObject * SObject
    | Set of SObject * SObject
    | Quote of SObject    
    | Begin of list<SObject>
    | Rest of SObject
    | DefineMacro of SObject * SObject
    // This might sound like a native, but will actually
    // be translated to a Function(...)
    | Lambda of SObject * SObject
    // Native Values
    | Syntax of SObject * SObject
    | Number of float    
    | Function of (SObject -> SObject)
    | Cons of SObject * SObject
    | Atom of string
    | String of string
    | NIL
    | True
    | False
    override this.ToString() =
        match this with
        // This is a list
        | Cons(_, Cons(_, _)) -> 
            let (stringified : list<string>) = this.Map (fun x -> x.ToString())
            String.Format("({0})", List.fold (fun tot it -> tot + " " + it) stringified.Head stringified.Tail)
        // A one element list
        | Cons(h, NIL) -> String.Format("({0})", h.ToString())
        // A cons cell
        | Cons(fst, snd) -> String.Format("({0} . {1})", fst.ToString(), snd.ToString())
        // Scalar types
        | Atom(s) -> s
        | Number(i) -> i.ToString()
        | Lambda(args, body) -> String.Format("<lambda {0} {1}>", args, body)
        | Function(_) -> "<system function>"
        | String(s) -> String.Format("\"{0}\"", s)
        | NIL -> "NIL"
        | True -> "T"
        | _ -> failwith "Only for literals and values!"

    member this.Map (f : SObject -> _) =
        match this with
        | Cons(something, NIL) -> [ f something ]
        | Cons(something, tail) -> f something :: tail.Map f
        | NIL -> []
        | _ -> failwith "Only for cons lists!"

    member this.Zip (another : SObject) (f : SObject -> SObject -> _) =
        match this with
        | Cons(sth, NIL) -> 
            match another with
            | Cons(a_sth, _) -> Cons(f sth a_sth, NIL)
            | _ -> failwith "another is not a cons cell!"
        | Cons(sth, sth_tail) ->
            match another with
            | Cons(a_sth, t_sth) -> Cons(f sth a_sth, sth_tail.Zip t_sth f)
            | _ -> failwith "another is not a cons cell!"
        | NIL -> NIL
        | _ -> failwith "this is not a cons cell!"

    member this.ConsMap (f : SObject -> SObject) =
        match this with
        | Cons(something, NIL) -> Cons(f something, NIL)
        | Cons(something, tail) -> Cons(f something, tail.ConsMap f)
        | NIL -> NIL
        | _ -> failwith "Only for cons lists!";;

//let rec ConsToString cell = 
//    match cell with
//    // This is a list
//    | Cons(_, Cons(_, _)) -> 
//        let stringified = cell.Map ConsToString
//        String.Format("({0})", List.fold (fun tot it -> tot + " " + it) stringified.Head stringified.Tail)
//    // A one element list
//    | Cons(h, NIL) -> String.Format("({0})", h.ToString())
//    // A cons cell
//    | Cons(fst, snd) -> String.Format("({0} . {1})", fst.ToString(), snd.ToString())
//    | _ -> cell.ToString();;

type EnvImpl = 
    val inner : Option<EnvImpl>
    val dict : Dictionary<string, SObject>

    new() = {
        inner = None;
        dict = new Dictionary<string, SObject>(); 
    }
    
    // used by Wrap()
    private new(newInner) = {
        inner = Some(newInner);
        dict = new Dictionary<string, SObject>(); 
    }

    // used by Copy()
    private new(innerE, dictE) = {
        inner = innerE; 
        dict = dictE;        
    }

    // Call this when entering a new scope
    member this.Wrap() =
        new EnvImpl(this)

    member this.Copy() =
        // We are cloning only the dictionary, not the values
        // if there are SObjects with mutating state, we need to
        // clone them too
        match this.inner with
        | None -> new EnvImpl(None, new Dictionary<string, SObject>(this.dict))
        | Some(env) ->
            new EnvImpl(Some(env.Copy()), new Dictionary<string, SObject>(this.dict))

    // Call this when exiting a scope (if necessary)
    member this.Unwrap() =
        match this.inner with
        | Some(e) -> e
        | None -> failwith "No inner env!"

    // Lookups up for a given key 
    // recursively in the environment
    member this.Lookup(key) =
        if this.dict.ContainsKey(key) then
            this.dict.[key]
        else 
            match this.inner with
            | Some(env) -> env.Lookup(key);
            | None -> failwith ("Unable to find " + key)

    // Put a new element in the environment
    // Return the environment itself
    member this.Put(key, ob) =
        if this.dict.ContainsKey(key) then
            failwith "You can't mutate an existing var with put!"
        else
            this.dict.Add(key, ob)
        this

    member this.Change(key, new_value) =
        if this.dict.ContainsKey(key) then
            this.dict.[key] <- new_value
        else
            failwith "You can't change something that doesnt exist!"
        this;;

// Environment
type Env = 
    val mutable e : EnvImpl

    new() = { e = new EnvImpl(); }

    // A copy of the environment
    private new(env) = { 
        e = env;          
    }

    member this.Wrap() =
        this.e <- this.e.Wrap()
        this

    member this.Copy() = 
        new Env(this.e.Copy())

    member this.Unwrap() =
        this.e <- this.e.Unwrap()
        this

    member this.Lookup(key) =
        this.e.Lookup(key)

    member this.Put(key, ob) =
        this.e.Put(key, ob) |> ignore
        this
        
    member this.Change(key, new_val) = 
        this.e.Change(key, new_val) |> ignore
        this;;


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


let SysDisplay (args : SObject) : SObject = 
    match args with
    | Cons(to_display, NIL) ->
        let displayed = to_display.ToString()
        Console.WriteLine displayed |> ignore
        String(displayed)
    | _ -> failwith "display expects one argument!";;


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

let SysEq (args : SObject) = 
    match args with
    | Cons(arg1, Cons(arg2, NIL)) ->
        match arg1, arg2 with
        | Number(i1), Number(i2) when i1 = i2 -> True
        | String(s1), String(s2) when s1 = s2 -> True
        | Atom(a1), Atom(a2) when a1 = a2 -> True
        // TODO put here more stuff!
        | _, _ -> NIL
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


// Makes the core environment for our language
let CoreEnv () =
    let e = new Env();
    e.Put("display", Function(SysDisplay)).
      Put("+", Function(SysAdd)).
      Put("*", Function(SysMult)).
      Put("-", Function(SysSub)).
      Put("/", Function(SysDiv)).
      Put("car", Function(SysCar)).
      Put("cdr", Function(SysCdr)).
      Put("list", Function(SysList)).
      Put("length", Function(SysLength)).
      Put("cons", Function(SysCons)).
      Put("eq", Function(SysEq)).
      Put("apply", Function(SysApply)).
      Put("map", Function(SysMap)).
      Put("null?", Function(``SysNull?``));;


let rec AppendCons (cons : SObject) (tail : SObject) =
    match cons with
    | Cons(h, NIL) -> Cons(h, Cons(tail, NIL))
    | Cons(h, t) -> Cons(h, AppendCons t tail)
    | _ -> failwith "Unexpected object in append_cos!";;


let rec PrintSexp = function    
    | DefineMacro(_, _) -> "Define-Macro ..."
    | Rest(_) -> "Rest ..."
    | Begin(ls) ->
        String.Format("Begin({0})", List.map PrintSexp ls)
    | Quasiquote(sexpr) ->
        String.Format("Quasiquote({0})", [PrintSexp sexpr])
    | Unquote(sexpr) ->
        String.Format("Unquote({0})", [PrintSexp sexpr])
    | Set(id, sexpr) ->
        match id with
        | Atom(name) -> String.Format("Set!({0}, {1})", name, PrintSexp sexpr)
        | _ -> failwith "Invalid set!"
    | Quote(sexpr) ->
        String.Format("Quote({0})", [PrintSexp sexpr])
    | Let_Star(bindings, sexpr) ->
        String.Format("Let({0}, {1})", 
            PrintSexp bindings, 
            List.map PrintSexp sexpr)
    | Let(bindings, sexpr) ->
        String.Format("Let({0}, {1})", 
            PrintSexp bindings, 
            List.map PrintSexp sexpr)
    | Define(id, exp) ->
        String.Format("Define({0}, {1})", PrintSexp id, PrintSexp exp)
    | If(condition, trueBr, falseBr) ->
        String.Format("If({0}, {1}, {2})", PrintSexp condition, PrintSexp trueBr, PrintSexp falseBr)
    | Cons(head, tail) -> 
        String.Format("Cons({0}, {1})", PrintSexp head, PrintSexp tail)
    | Atom(name) -> String.Format("Atom({0})", name)                 
    | Number(n) -> String.Format("Number({0})", n)
    | String(s) -> String.Format("String({0})", s)
    | Lambda(args, body) -> 
        String.Format("Lambda({0}, {1})", PrintSexp args, PrintSexp body)
    | Function(_) -> "Native Function"
    | True -> "#True"
    | False -> "#False"
    | NIL -> "NIL";; 


let rec ParseAst (env : Env) ast =
    match ast with
    | DefineMacro(macro_args, body) ->
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
    | Begin(ls) ->
        List.map (ParseAst env) ls |> List.rev |> List.head
    | Quasiquote(sexpr) ->
        // Find all unquotes inside sexpr, 
        // parse them. Any unquote outside a quasiquote is invalid!!!
        let rec WalkQuasiquote x =
            match x with
            | Cons(head, tail) -> Cons(WalkQuasiquote head, WalkQuasiquote tail)
            | Unquote(sexpr) -> ParseAst env sexpr
            | x -> x
        WalkQuasiquote sexpr
    | Set(id, sexpr) ->
        match id with
        | Atom(name) -> 
            let parsed = ParseAst env sexpr
            env.Change(name, parsed) |> ignore
            parsed
        | _ -> failwith "Invalid set!"
    | Quote(sexpr) -> sexpr
    | Let_Star(bindings, sexpr) ->
        let new_env = env.Wrap()
        // Parse and add bindings to new environment
        bindings.ConsMap (
            function
            | Cons(Atom(name), value) ->
                // Parse and add bindings to new environment
                let parsed = ParseAst new_env value
                new_env.Put(name, parsed) |> ignore 
                // Create a clean env before continuing
                new_env.Wrap() |> ignore             
                parsed
            | _ -> failwith "WTF???") |> ignore
        // Return last parsed element
        List.map (ParseAst new_env) sexpr |> List.rev |> List.head
    | Let(bindings, sexpr) ->
        let new_env = env.Wrap()
        bindings.ConsMap (
            function
            | Cons(Atom(name), value) ->
                // Parse and add bindings to new environment
                let parsed = ParseAst env value
                new_env.Put(name, parsed) |> ignore
                parsed
            | _ -> failwith "WTF???") |> ignore
        // Make a clean env before parsing sexpr
        new_env.Wrap() |> ignore
        // Parse; 
        // Return last parsed element
        List.map (ParseAst new_env) sexpr |> List.rev |> List.head
    | Define(identifier, exp) ->
        match identifier with
        | Atom(name) ->
            let parse = ParseAst env exp
            env.Put(name, parse) |> ignore
            parse
        | _ -> failwith "Invalid define!"
    // if
    | If(_condition, _true_branch, _false_branch) ->
        match ParseAst env _condition with
        | False -> ParseAst env _false_branch
        | _ -> ParseAst env _true_branch
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
            
            ParseAst new_env body            
        | _ -> failwith "Expected a function!"
    // Lookup identifiers
    | Lambda(arguments, body) ->
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
            arguments.Zip x (fun name sexpr -> 
                match name with
                | Atom(name) -> new_env.Put(name, sexpr) |> ignore
                                sexpr
                | _ -> failwith "Invalid argument!") |> ignore
            // Parse body with the new environment
            ParseAst new_env body)
    | Unquote(_) -> failwith "Unquote is only valid inside a quasiquote!"
    | Atom(name) -> env.Lookup(name)    
    // Atomic literals evalutate to themselves
    | String(_) | Number(_) | NIL | True -> ast
    | _ -> failwith "Error!";;
    
