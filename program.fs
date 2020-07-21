


let facts = [
  ["parent"; "alice"; "bob"];
  ["parent"; "alice"; "bill"];
  ["parent"; "bob"; "carol"];
  ["parent"; "carol"; "dennis"];
  ["parent"; "carol"; "david"]
]

let rules = [
  [["ancestor"; "X"; "Y"]; ["parent"; "X"; "Y"]];
  [["ancestor"; "X"; "Y"]; ["ancestor"; "X"; "Z"];
                           ["ancestor"; "Z"; "Y"]];
  [["family"; "X"; "Y"]; ["ancestor"; "X"; "Y"]];
  [["family"; "X"; "Y"]; ["family"; "Y"; "X"]]
]

let isVariable(identifier:string) =
    System.Char.ToUpper(identifier.[0]) = identifier.[0]




type Pred=string list
type Fact=string list
type Rule=(string list) list
type Envr=(string*string) list option


let mergebind (env:Envr) vp=
    if env.IsNone then
        None
    elif (List.tryFind  (fun (x,y) -> x=fst vp) env.Value).IsNone then
        Some (vp :: env.Value)    
    elif (List.tryFind  (fun (x,y) -> x=fst vp && y=snd vp) env.Value).IsSome then
        env
    else
        None

let asignbind (env:Envr) (pred:Pred)=
    assert (env.IsSome)
    let pred= pred |> List.map ( fun x ->
            let xx =List.tryFind  (fun (v,p) -> x=v) env.Value
            if xx.IsSome then
                snd xx.Value
            else
                x
        )
    pred



let unifypf (env:Envr) (pred:Pred) (fact:Fact)=
    if pred.Length <> fact.Length then
        None
    elif pred.[0] <> fact.[0] then
        None 
    else
        let rec test env ppred pfact =
            match ppred,pfact with
            | x::xs, y::ys   when x=y ->
                                        test env xs ys
            | x::xs, y::ys   when isVariable x ->
                                        let env=mergebind env (x,y)
                                        if env.IsNone then
                                            None
                                        else
                                            test env xs ys
            | x::xs, y::ys   when x<>y ->
                                        None
            | [],[]                   ->
                                        env

            | _,_                      -> None
        test env pred.Tail fact.Tail

let matchrule (rule:Rule) (facts:Fact list)=
    let body=rule.Tail
    let endpos=body.Length - 1

    let fss=Array.init body.Length  ( fun x -> facts )

    let rec next (envs:Envr list) (oldenv:Envr) (pos:int)=

        if pos=0 && fss.[pos].IsEmpty then
            envs
        else
            let newenv= unifypf oldenv body.[pos] fss.[pos].Head

            if newenv.IsSome then
                if pos=endpos then
                    let envs= newenv :: envs
                    fss.[0] <- fss.[0].Tail
                    next envs (Some []) 0
                else
                    let pos = pos + 1
                    fss.[pos] <- facts
                    next envs newenv pos 
            else
                fss.[pos] <- fss.[pos].Tail
                if fss.[pos].Length>0 then                
                    next envs oldenv pos
                else
                    if pos=0 && fss.[pos].IsEmpty then
                        envs
                    else
                        fss.[0] <- fss.[0].Tail
                        next envs (Some []) 0

    let envs = next [] (Some []) 0
    let nfacts = envs |> List.map ( fun x -> asignbind x rule.Head)

    let nfacts = List.distinct nfacts@facts
    nfacts


let rec buildDb rules facts =
    let nfacts = rules |> List.map (fun x-> matchrule x facts)  |> List.concat
    let nfacts = List.distinct nfacts

    if nfacts.Length=facts.Length then 
        nfacts
    else
        buildDb rules nfacts


let queryDb (pred:Pred)  (nfacts:Fact list)=
    let res = nfacts |> List.choose ( fun x-> 
        if (unifypf (Some []) pred x).IsSome then
            Some x
        else
            None        
        ) 
    res


[<EntryPoint>]
let main(args)=

    // printfn "%A" (matchrule rules.[0]  facts)

    let nfacts=buildDb rules  facts
    printfn "%A" nfacts

    let res=queryDb rules.[0].Head nfacts

    printfn "%A" res
    0






