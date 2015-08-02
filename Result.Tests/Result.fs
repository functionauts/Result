module Functionauts.Result.Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open System
open Functionauts.Result

let exceptionGenerator : Gen<exn> =
    Gen.elements [
        AccessViolationException();
        AggregateException();
        AppDomainUnloadedException();
        ApplicationException();
        ArgumentException();
        ArgumentNullException();
        ArgumentOutOfRangeException();
        ArithmeticException();
        ArrayTypeMismatchException();
        BadImageFormatException();
        CannotUnloadAppDomainException();
        ContextMarshalException();
        DataMisalignedException();
        DivideByZeroException();
        DllNotFoundException();
        DuplicateWaitObjectException();
        EntryPointNotFoundException();
        Exception();
        FieldAccessException();
        FormatException();
        IndexOutOfRangeException();
        InsufficientExecutionStackException();
        InsufficientMemoryException();
        InvalidCastException();
        InvalidOperationException();
        InvalidProgramException();
        InvalidTimeZoneException();
        MemberAccessException();
        MethodAccessException();
        MissingFieldException();
        MissingMemberException();
        MissingMethodException();
        MulticastNotSupportedException();
        NotFiniteNumberException();
        NotImplementedException();
        NotSupportedException();
        NullReferenceException();
        ObjectDisposedException("")
        OperationCanceledException();
        OutOfMemoryException();
        OverflowException();
        PlatformNotSupportedException();
        RankException();
        StackOverflowException();
        TimeoutException();
        TimeZoneNotFoundException();
        TypeAccessException();
        TypeInitializationException("", Exception())
        TypeLoadException();
        TypeUnloadedException();
        UnauthorizedAccessException();
        UriFormatException();
    ]

let resultGenerator<'t, 'e when 'e :> exn> : Gen<Result<'t, 'e>> =
    gen { let! t = Arb.generate<'t>
          let! e = Arb.generate<'e>
          return! Gen.elements [ Success t; Failure e ] }

type ResultGenerators =
    static member Result() = Arb.fromGen resultGenerator
    static member exn() = Arb.fromGen exceptionGenerator


[<Arbitrary[|typeof<ResultGenerators>|]>]
module PropertyTests =
    
    module ``unit`` =

        [<Property>]
        let ``pure`` (v: int list) =
            unit v = Success v


    module ``map`` =

        [<Property>]
        // fmap id = id
        let ``obeys the identity law`` (r: Result<int list, exn>) =
            id <^> r = id r
            
        [<Property>]
        // fmap (g . h) = (fmap g) . (fmap h)
        let ``obeys the function composition law`` (g: int list -> int list, h: int list -> int list, r: Result<int list, exn>) =
            (<^>)(g >> h) r = ((<^>)(g) >> (<^>)(h)) r

    
    module ``apply`` =

        [<Property>]
        // pure id <*> v = v
        let ``obeys the identity law`` (v: Result<int list, exn>) =
            unit id <*> v = v

        [<Property>]
        // pure f <*> pure x = pure (f x)
        let ``obeys the homomorphism law`` (f: int list -> int list, x: int list) =
            unit f <*> unit x = unit (f x)
    
        [<Property>]
        // u <*> pure y = pure ($ y) <*> u
        let ``obeys the interchange law`` (u: Result<(int list -> int list), exn>, y: int list) =
            u <*> unit y = (unit (fun x -> x y) <*> u)
    
        [<Property>]
        // u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
        let ``obeys the composition law`` (u: Result<(int list -> int list), exn>, v: Result<(int list -> int list), exn>, w: Result<int list, exn>) =
            u <*> (v <*> w) = (unit(<<) <*> u <*> v <*> w)
    
    
    module ``flatMap`` =
    
        [<Property>]
        // return x >>= f = f x
        let ``obeys the left identity law`` (x: int list, f: int list -> Result<int list, exn>) =
            unit x >>= f = f x
    
        [<Property>]
        // m >>= return = m
        let ``obeys the right identity law`` (m: Result<int list, exn>) =
            m >>= unit = m
    
        [<Property>]
        // (m >>= f) >>= g = m >>= (\x -> f x >>= g)
        let ``obeys the associativity law`` (m: Result<int list, exn>, f: int list -> Result<int list, exn>, g: int list -> Result<int list, exn>) =
            (m >>= f) >>= g = (m >>= (fun x -> f x >>= g))

        
    module ``materialize`` =
    
        [<Property>]
        let ``success`` (f: int list -> int list, x: int list) =
            materialize f x = Success (f x)
    
        [<Property>]
        let ``failure`` (x: int list, e: exn) =
            let f _ : int list = raise e
            materialize f x = Result<int list, exn>.Failure e
    
    
    module ``dematerialize`` =
    
        [<Property>]
        let ``success`` (x: int list) =
            Success x |> dematerialize = x
        
        [<Property>]
        let ``failure`` (e: exn) =
            Prop.throws<exn, _> (lazy (Failure e |> dematerialize))
