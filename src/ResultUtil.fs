module ResultUtil

let (<@>) a b = Result.map b a  
let (=<<) = Result.bind
let (>>=) a b = Result.bind b a
let (>=>) f1 f2 x = Result.bind f2 (f1 x)
let (>*>) a b = Result.bind b a
let (<*>) (f : Result<'a -> 'b, 'err>) (x : Result<'a, 'err>) = f >>= fun f' -> x <@> f'

type ResultBuilder<'err>() =
    member this.Return<'res> a : Result<'res,'err> = Ok a
    member this.Bind<'a, 'b> (m:Result<'a,'err>, f:'a->Result<'b,'err>) = Result.bind f m
    member this.ReturnFrom<'res> m : Result<'res,'err> = m
    member this.Zero() = Ok ()
    member this.Combine(a:Result<unit,'err>, f:unit -> _) = Result.bind f a
    member this.Yield<'res> a : Result<'res,'err> = Ok a
    member this.YieldFrom<'res> a : Result<'res,'err> = a
    member this.Delay (f : unit -> 'res) = f
    member this.Run<'res> f : Result<'res,'err> = f()
    member this.While (cond, f) =
        if cond() then this.Bind(f(), fun _ -> this.While(cond, f))
        else this.Zero()
    member this.For (xs:seq<'res>, f) =
        using (xs.GetEnumerator()) (fun it ->
            this.While (
                (fun () -> it.MoveNext()),
                (fun () -> f it.Current) )
        )
    member this.TryWith<'res>(m:Result<'res,'err>, h) =
        try this.ReturnFrom(m)
        with e -> h e
    member this.TryFinally<'res>(m:Result<'res,'err>, recover) =
        try this.ReturnFrom(m)
        finally recover()

let result<'a> = ResultBuilder<'a>()

let isOk<'res,'err> : Result<'res,'err> -> bool = function | Ok _ -> true | Error _ -> false
let isError<'res,'err> : Result<'res,'err> -> bool = isOk >> not

let ret = Result.Ok
let throw = Result.Error

let withError (errorMsg : 'err) (opt : 'a option) : Result<'a, 'err> =
    match opt with
    | None   -> Result.Error errorMsg
    | Some x -> Result.Ok x

let traverseResultM f xs =
    let retn = Result.Ok
    let initState = retn []
    let folder h t =
        f h >>= (fun h ->
        t >>= (fun t ->
        retn (h::t)))
    List.foldBack folder xs initState

let traverseResultA f xs : Result<'res list, 'err list> =
    let (<*>) g x =
        match g, x with
        | Ok f, Ok y -> Ok (f y)
        | Error es, Ok _ -> Error es
        | Ok _, Error es -> Error es
        | Error es, Error ees -> Error <| List.append es ees
    let retn = Result.Ok
    let cons h t = h::t
    let initState = retn []
    let folder head tail =
        retn cons <*> (f head) <*> tail
    List.foldBack folder xs initState

let sequenceResultM (xs:Result<'a,'err> list) : Result<'a list, 'err> =
    traverseResultM id xs

let sequenceResultA (xs:Result<'a,'err list> list) : Result<'a list, 'err list> =
    traverseResultA id xs
    
let rec foldM (folder:'s -> 'a -> Result<'s,'err>) (state:'s) (list:'a list) : Result<'s,'err> =
    match list with
    | [] -> ret state
    | hd :: tl -> folder state hd >>= fun s -> foldM folder s tl