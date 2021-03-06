﻿module ParseAst

open AstObject
open Env
open System

// Let this fail if arguments are invalid
[<System.Diagnostics.DebuggerStepThroughAttribute()>]
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
    | Function_CPS(_) -> "<Function>"
    | Char(c) -> String.Format("'{0}'", c)
    | True -> "#True"
    | False -> "#False"
    | NIL -> "NIL";; 


[<System.Diagnostics.DebuggerStepThroughAttribute()>]
let rec Last = 
    function
    | Cons(x, NIL) -> x
    | Cons(_, t) -> Last t
    | a -> failwith "Expected a cons cell!";;


[<System.Diagnostics.DebuggerStepThroughAttribute()>]
let rec AppendForSplice (cons : SObject) (tail : SObject) =
    match cons with
    | Cons(h, NIL) -> Cons(h, tail)
    | Cons(h, t) -> Cons(h, AppendForSplice t tail)
    | _ -> failwith "Unexpected object in AppendForSplice!";;

    
let rec ParseAstCPS (env : Env) ast (kont : SObject -> unit) : unit =
    match ast with
    | Quasiquote(sexpr) ->
        // Find all unquotes inside sexpr, 
        // parse them. Any unquote outside a quasiquote is invalid!!!
        let rec WalkQuasiquote x k =
            match x with
            // Here looks the same, but the container (hopefully, a cons(_, _))
            // must be carefull!
            | UnquoteSplicing(sexpr) | Unquote(sexpr) -> 
                ParseAstCPS env sexpr k
            // Constructs
            | Cons(a, b) -> 
                //Cons(WalkQuasiquote a, WalkQuasiquote b)
                // We need to *know* that the 
                // result of a UnquoteSplicing is a consed list
                match a, b with
                | UnquoteSplicing(_), UnquoteSplicing(_) ->
                    failwith "Implement (a, b) match (UnquoteSplicing(_), UnquoteSplicing(_))"
                | UnquoteSplicing(_), _ -> 
                    WalkQuasiquote a (fun head_list -> 
                        match head_list with
                        | Cons(_, _) -> 
                            WalkQuasiquote b (fun tail_list -> 
                                AppendForSplice head_list tail_list |> k
                            )
                        |_ -> failwith "Result of UnquoteSplicing should be a list!"
                    )                    
                | _, UnquoteSplicing(_) -> 
                    failwith "Implement (_, b) match (_, UnquoteSplicing(_))"
                | _, _ -> 
                    // do regular stuff ...
                    WalkQuasiquote a (fun h -> 
                        WalkQuasiquote b (fun t -> 
                            Cons(h, t) |> k
                        )
                    )                    
            // Return everything else as-is
            | x -> k x
        WalkQuasiquote sexpr kont
    | Quote(sexpr) -> kont sexpr
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
            ParseAstCPS env transformed kont
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
                ParseAstCPS env sexpr kont
            | _ -> 
                // create a lambda cons list
                let as_lambda = Cons(Atom("lambda"), Cons(!args, Cons(Cons(Atom("begin"), sexpr), NIL)))
                // and the use it for a function application
                let transformed = Cons(as_lambda, !values)        
                // and then we parse it!
                ParseAstCPS env transformed kont

        | _ -> failwith "Invalid arguments to let!"        
    | Cons(Atom("begin"), ls) ->
        
        let rec begin_parser = (fun args acc -> 
            match args with
            | Cons(h, tail) ->
                ParseAstCPS env h (fun h_parsed -> 
                    begin_parser tail (AppendCons acc h_parsed)
                )
            | NIL -> Last acc |> kont
            | _ -> failwith "Unexpected arg!")

        begin_parser ls NIL

    | Cons(Atom("set!"), args) ->
        match args with
        | Cons(id, Cons(sexpr, _)) ->
            match id with
            | Atom(name) -> 
                ParseAstCPS env sexpr (fun parsed -> 
                    env.Change(name, parsed) |> ignore
                    kont parsed
                )
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
                    kont parse
                | _ -> failwith "Name should be an atom!"
            | _ -> failwith "Expected name of the macro!"
        | _ -> failwith "Invalid args to define-macro"
    | Cons(Atom("define"), args) ->
        // Special form define
        match args with
        // ABREV. LAMBDA FORM
        | Cons(Cons(fun_name, fun_args), body) ->
            // transform this into a (define fun_name (lambda fun_args body))
            let transf = Cons(Atom("define"), Cons(fun_name, Cons(Cons(Atom("lambda"), Cons(fun_args, body)), NIL)))
            ParseAstCPS env transf kont
        // GENERAL FORM
        | Cons(identifier, Cons(exp, NIL)) ->
            match identifier with
            | Atom(name) ->
                ParseAstCPS env exp (fun parse -> 
                    env.Put(name, parse) |> ignore
                    kont parse
                )
            | _ -> failwith "Invalid identifier for define!"
        | _ -> failwith "Invalid args for define!"
    | Cons(Atom("lambda"), ls) ->
        // Lambda special form
        match ls with
        | Cons(arguments, Cons(body, NIL)) ->
            // Wrap executon of body in a function
            // We can create a copy of the current env
            // The semantics says this should be like this!!!
            let env_for_fun = env
            kont (Function_CPS(fun x k ->
                // x holds already evaled arguments
                // create a new environment
                                
                let new_env = env_for_fun.Wrap() //.Wrap()
                arguments.ZipDiscard x (fun name sexpr -> 
                    match name with
                    | Atom(name) -> new_env.Put(name, sexpr) |> ignore
                                    sexpr
                    | _ -> failwith "Invalid argument!")
                // Parse body with the new environment                
                ParseAstCPS new_env body k))
        | _ -> failwith "Invalid arguments to lambda!"
    | Cons(Atom("if"), args) ->
        match args with
        | Cons(_condition, Cons(_true_branch, Cons(_false_branch, NIL))) ->
            ParseAstCPS env _condition (fun conditionValue -> 
                match conditionValue with
                | False -> 
                    ParseAstCPS env _false_branch kont // (function branchValue -> kont branchValue)
                | _ -> 
                    ParseAstCPS env _true_branch kont //(function branchValue -> kont branchValue)
            )
        | _ -> failwith "Invalid args to if!"
    // Function application
    | Cons(_head, _args) ->
        ParseAstCPS env _head (fun _fn -> 

            match _fn with
            | Function_CPS(native) ->
                //_args.ConsMap (ParseAst env) |> native
                let rec parseArgsThenFunction args acc = 
                    match args with
                    | NIL ->
                        //acc.ToString() |> printfn "ARGUMENTS TO FUNCTION: %s"
                        native acc kont |> ignore
                    | Cons(h, tail) ->
                        ParseAstCPS env h (fun h -> parseArgsThenFunction tail (AppendCons acc h))
                    | _ ->
                        failwith "Unexpected"
                parseArgsThenFunction _args NIL

            | Syntax(args, body) ->
                // Create new env., 
                // bind *unparsed* arguments
                let new_env = env.Wrap()
            
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

                // Create the tree, then parse it!
                ParseAstCPS new_env body (function tree -> ParseAstCPS env tree kont)

            | _ -> failwith "Expected a function!"
        )
    | Unquote(_) -> failwith "Unquote is only valid inside a quasiquote!"
    | Atom(name) -> env.Lookup(name) |> kont
    // Scalar literals evaluate to themselves
    | String(_) | Number(_) | NIL | True | False -> kont ast
    | _ -> failwith "Error!";;
