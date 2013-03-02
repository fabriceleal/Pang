﻿module Ast

open System
open System.IO
open System.Collections.Generic
open AstObject
open Env
open ParseAst
open Microsoft.FSharp.Text.Lexing

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

// Write

let SysWrite (args : SObject) = 
    match args with
    | Cons(to_write, NIL) ->
        let to_display = to_write.ToString()
        to_display |> Console.WriteLine
        String(to_display)
    | _ -> failwith "Expected 1 argument!";;


let SysEval (args : SObject) = 
    NIL;;

// Read

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
    match args with
    | Cons(arg, NIL) ->
        match arg with
        | False | True -> True
        | _ -> False
    | _ -> failwith "Expecting 1 argument!";;


let ``SysChar``(args : SObject) =
    match args with
    | Cons(arg, NIL) ->
        match arg with
        | Char(_) -> True
        | _ -> False
    | _ -> failwith "Expecting 1 argument!";;


let ``SysPair?`` (args : SObject) =
    match args with
    | Cons(arg, NIL) ->
        match arg with
        | Cons(_, _) -> True
        | _ -> False
    | _ -> failwith "Expecting 1 argument!";;


let ``SysProcedure?`` (args : SObject) =
    match args with
    | Cons(arg, NIL) ->
        match arg with
        | Function(_) -> True
        | _ -> False
    | _ -> failwith "Expecting 1 argument!";;

let ``SysSymbol?`` (args : SObject) =
    match args with
    | Cons(arg, NIL) ->
        match arg with
        | Atom(_) -> True
        | _ -> False
    | _ -> failwith "Expecting 1 argument!";;


// TODO
let ``SysByteVector?`` (args : SObject) =
    False;;


// TODO
let ``SysEofObject?`` (args : SObject) =
    False;;


let ``SysNumber?`` (args : SObject) =
    match args with
    | Cons(arg, NIL) ->
        match arg with
        | Number(_) -> True
        | _ -> False
    | _ -> failwith "Expecting 1 argument!";;


// TODO
let ``SysPort?`` (args : SObject) =
    False;;


let ``SysString?`` (args : SObject) =
    match args with
    | Cons(arg, NIL) ->
        match arg with
        | String(_) -> True
        | _ -> False
    | _ -> failwith "Expecting 1 argument!";;


// TODO
let ``SysVector?`` (args : SObject) =
    False;;


let ``SysNull?`` (args : SObject) =
    match args with
    | Cons(arg, NIL) ->
        match arg with
        | NIL -> True
        | _ -> False
    | _ -> failwith "Expecting 1 argument!";;

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
      Put("write", Function(SysWrite)).
      Put("eval", Function(SysEval)).
      Put("read", Function(SysRead)).
      Put("read-char", Function(SysReadChar)).
      Put("read-line", Function(SysReadLine)).
      Put("boolean?", Function(``SysBoolean?``)).
      Put("char?", Function(``SysChar``)).
      Put("pair?", Function(``SysPair?``)).
      Put("procedure?", Function(``SysProcedure?``)).
      Put("symbol?", Function(``SysSymbol?``)).
      Put("bytevector?", Function(``SysByteVector?``)).
      Put("eof-object?", Function(``SysEofObject?``)).
      Put("number?", Function(``SysNumber?``)).
      Put("port?", Function(``SysPort?``)).
      Put("string?", Function(``SysString?``)).
      Put("vector?", Function(``SysVector?``)).
      Put("null?", Function(``SysNull?``));;



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
    | Char(c) -> String.Format("'{0}'", c)
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

