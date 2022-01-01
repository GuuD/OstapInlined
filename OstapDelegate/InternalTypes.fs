module OstapDelegate.InternalTypes
open System
open System
open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

type Inl = InlineIfLambdaAttribute
type IError = abstract member GetErrorMessage: unit -> string

[<Struct;>] 
type ParsingError = { PositionStart: int; PositionEnd:int; Error: IError }
type Eq<'t> = delegate of 't *'t -> bool
type StartsWith<'t> = delegate of ReadOnlySpan<'t> * ReadOnlySpan<'t> -> bool



type Status =
    | Success = 0
    | Failure = 1
    | FatalError = -1

[<IsByRefLike; Struct; NoComparison; NoEquality>]
type UnsafeParserStream<'t> = 
    val mutable Errors : ResizeArray<ParsingError>
    val mutable Position: int
    val mutable Label: string
    val mutable Status: Status
    val Stream: ReadOnlySpan<'t>
    val Memory: ReadOnlyMemory<'t>

    
    member inline x.Advance step = 
        x.Position <- x.Position + step
        x.Status <- Status.Success
    member inline x.SignalMiss() = 
        x.Status <- Status.Failure
        
    member inline x.SignalSuccess() = 
        x.Status <- Status.Success
        
    member x.SignalFatalError(start, ``end``, error) =
        x.Status <- Status.FatalError
        if isNull x.Errors then x.Errors <- ResizeArray()
        x.Errors.Add {PositionStart = start; PositionEnd = ``end``; Error = error}
        
    

    new(memory: ReadOnlyMemory<'t>, pos) = {Memory = memory; Stream = memory.Span; Position = pos; Errors = Unchecked.defaultof<_>; Status = Status.Success; Label = ""}
    member inline x.TryGetCurrentToken(var: outref<'t>) =
        let p = x.Position
        if p < x.Stream.Length then
            var <- x.Stream.[p]
            true
        else false
        
[<Struct; NoComparison; NoEquality>]   
type MutTuple<'t1, 't2> =
    val mutable Item1: 't1
    val mutable Item2: 't2
    new(a, b) = {Item1 = a; Item2 = b}
    
[<Struct; NoComparison; NoEquality>]   
type ArgumentHolder<'t1, 't2> =
    val mutable F1: 't1
    val mutable F2: 't2
    member inline x.ApplyTo ([<Inl>] f) = f x.F1 x.F2


    
[<Struct; NoComparison; NoEquality>]   
type ArgumentHolder<'t1, 't2, 't3> =
    val mutable F1: ArgumentHolder<'t1, 't2>
    val mutable F2: 't3
    member inline x.ApplyTo ([<Inl>] f) = f x.F1.F1 x.F1.F2 x.F2

[<Struct; NoComparison; NoEquality>]   
type ArgumentHolder<'t1, 't2, 't3, 't4> =
    val mutable F1: ArgumentHolder<'t1, 't2, 't3>
    val mutable F2: 't4
    member inline x.ApplyTo ([<Inl>] f) = f x.F1.F1.F1 x.F1.F1.F2 x.F1.F2 x.F2

[<Struct; NoComparison; NoEquality>]   
type ArgumentHolder<'t1, 't2, 't3, 't4, 't5> =
    val mutable F1: ArgumentHolder<'t1, 't2, 't3, 't4>
    val mutable F2: 't5
    member inline x.ApplyTo ([<Inl>] f) = f x.F1.F1.F1.F1 x.F1.F1.F1.F2 x.F1.F1.F2 x.F1.F2 x.F2

type Parser<'tok, 'value> = delegate of byref<UnsafeParserStream<'tok>> * outref<'value> -> unit
type Pipeline<'tok, 'a, 'b> = Parser<'tok, ArgumentHolder<'a, 'b>>
type Pipeline<'tok, 'a, 'b, 'c> = Parser<'tok, ArgumentHolder<'a, 'b, 'c>>
type Pipeline<'tok, 'a, 'b, 'c, 'd> = Parser<'tok, ArgumentHolder<'a, 'b, 'c, 'd>>
type Pipeline<'tok, 'a, 'b, 'c, 'd, 'e> = Parser<'tok, ArgumentHolder<'a, 'b, 'c, 'd, 'e>>

type P<'tok, 'value> = unit -> Parser<'tok, 'value>

type Constructor<'t> = delegate of unit -> 't
type Folder<'res, 'elem> = delegate of byref<'res> * byref<'elem> -> unit
type Default3 = class end
type Default2 = class inherit Default3 end
type Default1 = class inherit Default2 end   
type PH =
    inherit Default1
    static member inline (?<-)([<Inl>] p1: Pipeline< ^t,  ^a,  ^b>, [<Inl>] p2: Parser< ^t,  ^c>, _: PH) =
        Pipeline< ^t,  ^a,  ^b,  ^c>
            (fun stream v ->
                let initialPosition = stream.Position
                p1.Invoke(&stream, &v.F1)
                if stream.Status = Status.Success then
                    p2.Invoke(&stream, &v.F2)
                    if stream.Status <> Status.Success then 
                        if stream.Position <> initialPosition then
                            stream.SignalFatalError(initialPosition, stream.Position, Unchecked.defaultof<_>)) 

    static member inline (?<-)([<Inl>] p1:  Pipeline< ^t,  ^a,  ^b,  ^c>, [<Inl>] p2: Parser< ^t,  ^d>, _: PH) =
        Pipeline< ^t,  ^a,  ^b,  ^c,  ^d>
            (fun stream v ->
                let initialPosition = stream.Position
                p1.Invoke(&stream, &v.F1)
                if stream.Status = Status.Success then
                    p2.Invoke(&stream, &v.F2)
                    if stream.Status <> Status.Success then 
                        if stream.Position <> initialPosition then
                            stream.SignalFatalError(initialPosition, stream.Position, Unchecked.defaultof<_>))
    static member inline (?<-)([<Inl>] p1:  Pipeline< ^t,  ^a,  ^b,  ^c, ^d>, [<Inl>] p2: Parser< ^t,  ^e>, _: PH) =
        Pipeline< ^t,  ^a,  ^b,  ^c,  ^d, ^e>
            (fun stream v ->
                let initialPosition = stream.Position
                p1.Invoke(&stream, &v.F1)
                if stream.Status = Status.Success then
                    p2.Invoke(&stream, &v.F2)
                    if stream.Status <> Status.Success then 
                        if stream.Position <> initialPosition then
                            stream.SignalFatalError(initialPosition, stream.Position, Unchecked.defaultof<_>))
    
type PH with
    static member inline (?<-)([<Inl>] p1: Parser< ^t, ^a>, [<Inl>] p2: Parser< ^t,  ^b>, _: Default1) =
        Pipeline< ^t,  ^a,  ^b>
            (fun stream v ->
                let initialPosition = stream.Position
                p1.Invoke(&stream, &v.F1)
                if stream.Status = Status.Success then
                    p2.Invoke(&stream, &v.F2)
                    if stream.Status <> Status.Success then 
                        if stream.Position <> initialPosition then
                            stream.SignalFatalError(initialPosition, stream.Position, Unchecked.defaultof<_>))
            
module Span =
    let inline fold ([<Inl>] folder) state (span: ReadOnlySpan<_>) =
        let mutable result = state
        for item in span do
            result <- folder result item
        result