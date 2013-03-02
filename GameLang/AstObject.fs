module AstObject

open System

// AST Element
type SObject =
    // Language Constructs
    | Quasiquote of SObject
    | Unquote of SObject
    | UnquoteSplicing of SObject
    | Quote of SObject    
    // Native Values
    | Syntax of SObject * SObject
    | Number of float    
    | Function of (SObject -> SObject)
    | Cons of SObject * SObject    
    | Atom of string
    | String of string
    | Char of char
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
        | Function(_) -> "<system function>"
        | String(s) -> String.Format("\"{0}\"", s)
        | Char(c) -> String.Format("'{0}'", c)
        | NIL -> "NIL"
        | True -> "T"
        | _ -> failwith "Only for literals and values!"

    member this.Map (f : SObject -> _) =
        match this with        
        | Cons(something, tail) -> f something :: tail.Map f
        | NIL -> []
        | _ -> failwith "Only for cons lists!"

    member this.MapDiscard (f : SObject -> _) =
        match this with
        | Cons(something, tail) -> 
            f something |> ignore 
            tail.MapDiscard f
        | NIL -> ignore()
        | _ -> failwith "Only for cons lists!"

    member this.Zip (another : SObject) (f : SObject -> SObject -> _) =
        match this with
        | Cons(sth, sth_tail) ->
            match another with
            | Cons(a_sth, t_sth) -> f sth a_sth :: sth_tail.Zip t_sth f
            | _ -> failwith "another is not a cons cell!"
        | NIL -> []
        | _ -> failwith "this is not a cons cell!"
            
    member this.ZipDiscard (another : SObject) (f : SObject -> SObject -> _) =
        match this with       
        | Cons(sth, sth_tail) ->
            match another with
            | Cons(a_sth, t_sth) -> 
                f sth a_sth |> ignore
                sth_tail.ZipDiscard t_sth f
            | _ -> failwith "another is not a cons cell!"
        | NIL -> ignore()
        | _ -> failwith "this is not a cons cell!"

    // The List.Map for Cons lists
    member this.ConsMap (f : SObject -> SObject) =
        match this with
        | Cons(something, tail) -> Cons(f something, tail.ConsMap f)
        | NIL -> NIL
        | _ -> failwith "Only for cons lists!";;


// this does not appends lists!
// AppendCons (1 2 3) (4 5 6)
// will return (1 2 3 (4 5 6))
let rec AppendCons (cons : SObject) (tail : SObject) =
    match cons with
    | NIL -> Cons(tail, NIL)
    | Cons(h, NIL) -> Cons(h, AppendCons NIL tail)
    | Cons(h, t) -> Cons(h, AppendCons t tail)
    | _ -> failwith "Unexpected object in AppendCons!";;


let rec SetCdrOfLast (cons : SObject) (newcdr : SObject) =
    match cons with
    | Cons(h, NIL) -> Cons(h, newcdr)
    | Cons(h, t) -> Cons(h, SetCdrOfLast t newcdr)
    | _ -> failwith "Unexpected object in SetCdrOfLast!";;

