module Ast

open System
open System.Collections.Generic

// AST Element
type SObject =
    | Cons of SObject * SObject
    | Atom of string
    | Int of int
    | Let_Star of SObject * list<SObject>
    | Let of SObject * list<SObject>
    | Float of float
    | Lambda of SObject * SObject
    | Function of (SObject -> SObject)
    | If of SObject * SObject * SObject
    | Define of SObject * SObject
    | String of string
    | NIL
    | True
    override this.ToString() =
        match this with
        | Cons(c, NIL) ->
            String.Format("({0})", c.ToString())
        | Cons(c, t) ->
            match t with
            | Cons(_, _) -> String.Format("({0} {1})", c.ToString(), t.ToString())
            | _ -> String.Format("({0} . {1})", c.ToString(), t.ToString())
        | Atom(s) -> s
        | Int(i) -> i.ToString()
        | Float(f) -> f.ToString()
        | Lambda(args, body) -> String.Format("<lambda {0} {1}>", args, body)
        | Function(_) -> "<system function>"
        | String(s) -> String.Format("\"{0}\"", s)
        | NIL -> "NIL"
        | True -> "T"
        | _ -> failwith "Only for literals and values!"

    member this.ConsMap (f : SObject -> SObject) =
        match this with
        | Cons(something, NIL) -> Cons(f something, NIL)
        | Cons(something, tail) -> Cons(f something, tail.ConsMap f)
        | _ -> failwith "Only for cons lists!";;


// Environment
type Env = 
    val inner : Option<Env>
    val dict : Dictionary<string, SObject>

    new() = { inner = None; dict = new Dictionary<string, SObject>(); }
    private new(x) = { inner = Some(x); dict = new Dictionary<string, SObject>(); }
    
    // Call this when entering a new scope
    member this.Wrap() =
        new Env(this)

    // Call this when exiting a scope (if necessary)
    member this.Unwrap() =
        match this.inner with
        | Some(e) -> e
        | None -> failwith "No inner env!"

    // Lookups up for a given key 
    // recursively in the environment
    member this.lookup(key) =
        if this.dict.ContainsKey(key) then
            this.dict.[key]
        else 
            match this.inner with
            | Some(env) -> env.lookup(key);
            | None -> failwith ("Unable to find " + key)

    // Put a new element in the environment
    // Return the environment itself
    member this.put(key, ob) =
        if this.dict.ContainsKey(key) then
            this.dict.[key] <- ob            
        else
            this.dict.Add(key, ob)
        this;;


// We already receive the arguments as a cons list,
// just return the arguments :)
let SysList (args: SObject) : SObject =
    args;;


let SysCons (args: SObject) : SObject =
    match args with
    | Cons(car, Cons(cdr, NIL)) ->
        Cons(car, cdr)
    | _ -> failwith "WTF?";;


let rec SysLength (args: SObject) : SObject =
    match args with
    | Cons(to_length, _) ->
        match to_length with
        | NIL -> Int(0)
        | Cons(_, tail) ->
            // We need to wrap the argument in another cons
            match SysLength(Cons(tail, NIL)) with
            | Int(i) -> Int(1 + i)
            | _ -> failwith "Oh boy, WTF?!?!?!"
        | _ -> failwith "Really, WTF???"
    | _ -> failwith "WTF?";;


let SysCar (args: SObject) : SObject =
    match args with
    | Cons(to_car, _) ->
        match to_car with
        | Cons(car, _) -> car
        | _ -> failwith "Really, WTF???"
    | _ -> failwith "WTF?";;


let SysCdr (args: SObject) : SObject =
    match args with
    | Cons(to_car, _) ->
        match to_car with
        | Cons(_, cdr) -> cdr
        | _ -> failwith "Really, WTF???"
    | _ -> failwith "WTF?";;


let SysPrint (args : SObject) : SObject = 
    match args with
    | Cons(to_print, _) ->
        let printed = to_print.ToString()
        Console.WriteLine printed |> ignore
        String(printed)
    | _ -> failwith "WTF?";;


// This does not evaluate exactly 
// like common lisp
let SysArith (name : String) (nullelement : float) (func : float -> float -> float) (args: SObject) : SObject =
    let rec __SysArith (args: SObject) = 
        match args with
        | Cons(NIL, NIL) ->
            Float(nullelement)
        | Cons(Float(add1), NIL) ->
            Float(add1)
        | Cons(Int(add1), NIL) ->
            Float(add1 |> float)
        | Cons(Float(add1), tail) -> 
            match __SysArith tail with
            | Float(v) -> Float(func add1 v)
            | _ ->  failwith ("Invalid return of " + name)
        | Cons(Int(add1), tail) -> 
            match __SysArith tail with
            | Float(v) -> Float(func (add1 |> float) v)
            | _ ->  failwith ("Invalid return of " + name)
        | _ -> failwith ("Invalid arguments to " + name)
    
    __SysArith args;;

let SysDiv = SysArith "/" 1.0 (/);;

let SysMult = SysArith "*" 1.0 (*);;

let SysSub = SysArith "-" 0.0 (-);;

let SysAdd = SysArith "+" 0.0 (+);;

let CoreEnv () =
    let e = new Env();
    e.put("print", Function(SysPrint)).
      put("+", Function(SysAdd)).
      put("*", Function(SysMult)).
      put("-", Function(SysSub)).
      put("/", Function(SysDiv)).
      put("car", Function(SysCar)).
      put("cdr", Function(SysCdr)).
      put("list", Function(SysList)).
      put("length", Function(SysLength)).
      put("cons", Function(SysCons));;


let rec AppendCons (cons : SObject) (tail : SObject) =
    match cons with
    | Cons(h, NIL) -> Cons(h, Cons(tail, NIL))
    | Cons(h, t) -> Cons(h, AppendCons t tail)
    | _ -> failwith "Unexpected object in append_cos!";;


let rec PrintSexp = function
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
    | Int(n) -> String.Format("Int({0})", n)
    | Float(f) -> String.Format("Float{0})", f)
    | String(s) -> String.Format("String({0})", s)
    | Lambda(args, body) -> 
        String.Format("Lambda({0}, {1})", PrintSexp args, PrintSexp body)
    | Function(_) -> String.Format("Native Function")
    | True -> String.Format("#True")
    | NIL -> String.Format("NIL");; 


let rec ParseAst (env : Env) ast =
    match ast with
    | Let_Star(bindings, sexpr) ->
        let new_env = env.Wrap()
        // Parse and add bindings to new environment
        bindings.ConsMap (
            function
            | Cons(Atom(name), value) ->
                let parsed = ParseAst new_env value
                new_env.put(name, parsed) |> ignore                
                parsed
            | _ -> failwith "WTF???") |> ignore
        // Return last parsed element
        List.map (ParseAst new_env) sexpr |> List.rev |> List.head
    | Let(bindings, sexpr) ->
        let new_env = env.Wrap()
        // Parse and add bindings to new environment
        bindings.ConsMap (
            function
            | Cons(Atom(name), value) ->
                let parsed = ParseAst env value
                new_env.put(name, parsed) |> ignore
                parsed
            | _ -> failwith "WTF???") |> ignore
        // Return last parsed element
        List.map (ParseAst new_env) sexpr |> List.rev |> List.head
    | Define(identifier, exp) ->
        match identifier with
        | Atom(name) ->
            let parse = ParseAst env exp
            env.put(name, parse) |> ignore
            parse
        | _ -> failwith "Invalid define!"
    // if
    | If(_condition, _true_branch, _false_branch) ->
        match ParseAst env _condition with
        | NIL -> ParseAst env _false_branch
        | _ -> ParseAst env _true_branch
    // Function application
    | Cons(_fn, _args) ->
        match _fn with 
        | Atom(name) -> 
            // Try to find function in environment
            match env.lookup(name) with
            | Function(native) ->
                // Evaluate arguments and call native function
                _args.ConsMap (ParseAst env) |> native
            | _ -> failwith "Expected a function!"
        | _ -> failwith "Unexpected head of list in function application!"
    // Lookup identifiers
    | Atom(name) -> env.lookup(name)
    // Atomic literals evalutate to themselves
    | String(_) | Int(_) | Float(_) | NIL | True -> ast
    | _ -> failwith "Error!";;
    
