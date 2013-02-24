module Ast

open System
open System.Collections.Generic

// AST Element
type SObject =
    | Cons of SObject * SObject
    | Atom of string
    | Int of int
    | Float of float
    | Lambda of SObject * SObject
    | Function of (SObject -> SObject)
    | String of string
    | NIL
    | True
    override this.ToString() =
        match this with
        | Cons(h, t) -> "<cons>"
        | Atom(s) -> s
        | Int(i) -> i.ToString()
        | Float(f) -> f.ToString()
        | Lambda(args, body) -> String.Format("<lambda {0} {1}>", args, body)
        | Function(_) -> "<system function>"
        | String(s) -> String.Format("\"{0}\"", s)
        | NIL -> "NIL"
        | True -> "T"

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


let SysPrint (args : SObject) : SObject = 
    match args with
    | Cons(to_print, _) ->
        let printed = to_print.ToString()
        Console.WriteLine printed |> ignore
        String(printed)
    | _ -> failwith "WTF?";;


let rec SysAdd (args: SObject) : SObject =
    match args with
    | Cons(NIL, NIL) ->
        Float(0.0)
    | Cons(Float(add1), NIL) ->
        Float(add1)
    | Cons(Int(add1), NIL) ->
        Float(add1 |> float)
    | Cons(Float(add1), tail) -> 
        match SysAdd(tail) with
        | Float(v) -> Float(add1 + v)
        | _ ->  failwith "Invalid return of +"
    | Cons(Int(add1), tail) -> 
        match SysAdd(tail) with
        | Float(v) -> Float((add1 |> float) + v)
        | _ ->  failwith "Invalid return of +"
    | _ -> failwith "Invalid arguments to +";;


let CoreEnv () =
    let e = new Env();
    e.put("print", Function(SysPrint)).
      put("+", Function(SysAdd));;


let rec AppendCons (cons : SObject) (tail : SObject) =
    match cons with
    | Cons(h, NIL) -> Cons(h, Cons(tail, NIL))
    | Cons(h, t) -> Cons(h, AppendCons t tail)
    | _ -> failwith "Unexpected object in append_cos!";;


let rec PrintSexp = function
    | Cons(head, tail) -> String.Format("Cons({0}, {1})", PrintSexp head, PrintSexp tail)
    | Atom(name) -> String.Format("Atom({0})", name)                 
    | Int(n) -> String.Format("Int({0})", n)
    | Float(f) -> String.Format("Float{0})", f)
    | String(s) -> String.Format("String({0})", s)
    | Lambda(args, body) -> String.Format("Lambda({0}, {1})", PrintSexp args, PrintSexp body)
    | Function(_) -> String.Format("Native Function")
    | True -> String.Format("#True")
    | NIL -> String.Format("NIL");; 


let rec ParseAst (env : Env) ast =
    match ast with
    // Function application
    | Cons(_fn, _args) ->
        match _fn with 
        | Atom(name) -> 
            match env.lookup(name) with
            | Function(native) -> native(_args)
            | _ -> failwith "Expected a function!"
        | _ -> failwith "Unexpected head of list in function application!"
    | _ -> failwith "Error!";;
    
