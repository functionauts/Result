namespace Functionauts.Result

type Result<'s, 'f when 'f :> exn> =
    | Success of 's
    | Failure of 'f

    static member (>>=) (r: Result<'t, 'e>, f: 't -> Result<'u, 'e>) : Result<'u, 'e> =
        match r with
        | Failure e -> Failure e
        | Success v -> f v

    static member (<^>) (f: 't -> 'u, r: Result<'t, 'e>) : Result<'u, 'e> =
        r >>= fun v -> Success (f v)

    static member (<*>) (f: Result<('t -> 'u), 'e>, r: Result<'t, 'e>) : Result<'u, 'e> =
        f >>= fun f -> f <^> r

[<AutoOpen>]
module Result =

    let unit (v: 't) : Result<'t, 'e> =
        Success v

    let map (f: 't -> 'u) (r: Result<'t, 'e>) : Result<'u, 'e> =
        f <^> r

    let apply (f: Result<('t -> 'u), 'e>) (r: Result<'t, 'e>) : Result<'u, 'e> =
        f <*> r

    let flatMap (r: Result<'t, 'e>) (f: 't -> Result<'u, 'e>) : Result<'u, 'e> =
        r >>= f

    let materialize (f: 't -> 'u) (v: 't) : Result<'u, exn> =
        try Success (f v) with
        | e -> Failure e

    let dematerialize (r: Result<'t, 'e>) : 't =
        match r with
        | Failure e -> raise e
        | Success v -> v
