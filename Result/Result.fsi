namespace Functionauts.Result

/// A Result represents two possibilities: a value in the case of Success or an exception in the case of Failure.
type Result<'s, 'f when 'f :> exn> =

    /// Creates a Result with the given value.
    | Success of 's

    /// Creates a Result with the given exception.
    | Failure of 'f

    /// Functor `fmap`. Applies the function to the value in the case of Success or returns a Result containing an exception in the case of Failure.
    static member (<^>) : f:('t -> 'u) * r:Result<'t, 'e> -> Result<'u, 'e>

    /// Applicative Functor `apply`. Applies the function to the value in the case of Success for both or returns a Result containing an exception in the case of Failure for either.
    static member (<*>) : f:(Result<('t -> 'u), 'e>) * r:(Result<'t, 'e>) -> Result<'u, 'e>

    /// Monadic `bind`. Applies the function to the value in the case of Success or returns a Result containing an exception in the case of Failure.
    static member (>>=) : r:Result<'t, 'e> * f:('t -> Result<'u, 'e>) -> Result<'u, 'e>

[<AutoOpen>]
module Result =

    /// Applicative `pure`. Lifts a value into Success.
    val unit : v:'t -> Result<'t, 'e>

    /// Named function for `Result.(<^>)`.
    val map : f:('t -> 'u) -> r:Result<'t, 'e> -> Result<'u, 'e>

    /// Named function for `Result.(<*>)`.
    val apply : f:Result<('t -> 'u), 'e> -> r:Result<'t, 'e> -> Result<'u, 'e>

    /// Named function for `Result.(>>=)`.
    val flatMap : r:Result<'t, 'e> -> f:('t -> Result<'u, 'e>) -> Result<'u, 'e>

    /// Converts a function from one that raises an exception to one that returns a Result.
    val materialize : f:('t -> 'u) -> v:'t -> Result<'u, exn>

    /// Returns the value in the case of Success or raises the exception in the case of Failure.
    val dematerialize : r:Result<'t, 'e> -> 't
