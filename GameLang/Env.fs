module Env

open AstObject
open System.Collections.Generic

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
            this
        else
            match this.inner with
            | Some(env) -> env.Change(key, new_value)
            | None -> failwith "You can't change something that doesnt exist!"
        ;;

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
