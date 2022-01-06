﻿namespace OstapDelegate
module InternalTypes =
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

        member inline x.SkipWhile ([<Inl>] predicate: 't -> bool) =
            let pos = x.Position
            while x.Position < x.Stream.Length && predicate x.Stream.[x.Position] do
                x.Position <- x.Position + 1
            x.Position - pos
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
            if x.Position < x.Stream.Length then
                var <- x.Stream.[x.Position]
                true
            else false
            
        
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

    type Folder<'res, 'elem> = delegate of byref<'res> * byref<'elem> -> unit
    type Default3 = class end
    type Default2 = class inherit Default3 end
    type Default1 = class inherit Default2 end   
    type PH =
        inherit Default1
    //    static member inline (?<-)([<Inl>] p1: Pipeline< ^t,  ^a,  ^b>, [<Inl>] p2: P< ^t,  ^c>, _: PH) =
        static member inline (?<-)([<Inl>] p1: Pipeline< ^t,  ^a,  ^b>, [<Inl>] p2: Parser< ^t,  ^c>, _: PH) =

            fun () ->
            Pipeline< ^t,  ^a,  ^b,  ^c>
                (fun stream v ->
                    let initialPosition = stream.Position
                    p1.Invoke(&stream, &v.F1)
                    if stream.Status = Status.Success then
                        p2.Invoke(&stream, &v.F2)
                        if stream.Status <> Status.Success then 
                            if stream.Position <> initialPosition then
                                stream.SignalFatalError(initialPosition, stream.Position, Unchecked.defaultof<_>)) 
    //    static member inline (?<-)([<Inl>] p1:  Pipeline< ^t,  ^a,  ^b,  ^c>, [<Inl>] p2: P< ^t,  ^d>, _: PH) =
        static member inline (?<-)([<Inl>] p1:  Pipeline< ^t,  ^a,  ^b,  ^c>, [<Inl>] p2: Parser< ^t,  ^d>, _: PH) =

            fun () ->
            Pipeline< ^t,  ^a,  ^b,  ^c,  ^d>
                (fun stream v ->
                    let initialPosition = stream.Position
                    p1.Invoke(&stream, &v.F1)
                    if stream.Status = Status.Success then
                        p2.Invoke(&stream, &v.F2)
                        if stream.Status <> Status.Success then 
                            if stream.Position <> initialPosition then
                                stream.SignalFatalError(initialPosition, stream.Position, Unchecked.defaultof<_>))
    //    static member inline (?<-)([<Inl>] p1:  Pipeline< ^t,  ^a,  ^b,  ^c, ^d>, [<Inl>] p2: P< ^t,  ^e>, _: PH) =
        static member inline (?<-)([<Inl>] p1:  Pipeline< ^t,  ^a,  ^b,  ^c, ^d>, [<Inl>] p2: Parser< ^t,  ^e>, _: PH) =

            fun () ->
            Pipeline< ^t,  ^a,  ^b,  ^c,  ^d, ^e>
                (fun stream v ->
                    let initialPosition = stream.Position
                    p1.Invoke(&stream, &v.F1)
                    if stream.Status = Status.Success then
                        p2.Invoke(&stream, &v.F2)
                        if stream.Status <> Status.Success then 
                            if stream.Position <> initialPosition then
                                stream.SignalFatalError(initialPosition, stream.Position, Unchecked.defaultof<_>))
        static member inline (?<-)([<Inl>] p1: Parser< ^t, Unit>, [<Inl>] p2: Parser< ^t,  ^b>, _: PH) =
            fun () ->
            Parser< ^t, ^b>
                (fun stream v ->
                    let initialPosition = stream.Position
                    p1.Invoke(&stream) |> ignore
                    if stream.Status = Status.Success then
                        p2.Invoke(&stream, &v)
                        if stream.Status <> Status.Success then 
                            if stream.Position <> initialPosition then
                                stream.SignalFatalError(initialPosition, stream.Position, Unchecked.defaultof<_>))
        
    type PH with
    //    static member inline (?<-)([<Inl>] p1: Parser< ^t, ^a>, [<Inl>] p2: P< ^t,  ^b>, _: Default1) =
        static member inline (?<-)([<Inl>] p1: Parser< ^t, ^a>, [<Inl>] p2: Parser< ^t,  ^b>, _: Default1) =
            fun () ->
            Pipeline< ^t,  ^a,  ^b>
                (fun stream v ->
                    let initialPosition = stream.Position
                    p1.Invoke(&stream, &v.F1)
                    if stream.Status = Status.Success then
                        p2.Invoke(&stream, &v.F2)
                        if stream.Status <> Status.Success then 
                            if stream.Position <> initialPosition then
                                stream.SignalFatalError(initialPosition, stream.Position, Unchecked.defaultof<_>))

            
open InternalTypes

module Operators =
    open OptimizedClosures
    open InternalTypes
    open System.Runtime.CompilerServices
    open System.Runtime.InteropServices
    open System

    let inline (|>) (arg) ([<Inl>] func) = func arg
    let inline (!) ([<Inl>] p: P< ^t, ^a>) =
        fun () ->
            Parser< ^t, unit>
                (fun stream v ->
                    p().Invoke(&stream) |> ignore)
//    type FSharpFunc<'T, 'Res> with
//        static member inline CombineParsers([<Inl>] p1: FSharpFunc<Unit, Parser< ^t, Unit>>, [<Inl>] p2: FSharpFunc<Unit, Parser< ^t,  ^b>>)  =
//        static member inline CombineParsers([<Inl>] p1: FSharpFunc<Unit, Parser< ^t, Unit>>, [<Inl>] p2: FSharpFunc<Unit, Parser< ^t,  Unit>>)  =
//            fun () ->
//            Pipeline< ^t,  ^a,  ^b>
//                (fun stream v ->
//                    let initialPosition = stream.Position
//                    p1().Invoke(&stream, &v.F1)
//                    if stream.Status = Status.Success then
//                        p2().Invoke(&stream, &v.F2)
//                        if stream.Status <> Status.Success then 
//                            if stream.Position <> initialPosition then
//                                stream.SignalFatalError(initialPosition, stream.Position, Unchecked.defaultof<_>))
//        static member inline (|>) ([<Inl>] p1: FSharpFunc<Unit, Parser< ^t, ^b>>, f) =
//            f p1
            
module Span =
    open System.Runtime.CompilerServices
    open System.Runtime.InteropServices
    open System
    open InternalTypes
    let inline fold ([<Inl>] folder) state (span: ReadOnlySpan<_>) =
        let mutable result = state
        for item in span do
            result <- folder result item
        result
        
module VList = 
    open System
    open System.Collections.Generic

    open FSharp.NativeInterop
    open System.Runtime.CompilerServices
    open System.Runtime.InteropServices
    open System.Threading

    [<Struct>]
    type VList<'t> = 
        | Empty
        | List of Cons<'t> 
        member x.Cons(elem) = 
            match x with
            | Empty -> 
                List({Bucket = Bucket([|elem; Unchecked.defaultof<_>|], Empty, false); CurrentElementIndex = 0})
            | List({Bucket = b; CurrentElementIndex = i}) when not b.IsReadOnly && b.Capacity > i + 1 && Interlocked.CompareExchange(&b.LatestIndex, i + 1, i) = i ->
                Array.set b.Elems (i+1) elem
                List({Bucket = b; CurrentElementIndex = i + 1})
            | List({Bucket = b; CurrentElementIndex = i}) -> 
                let elems = Array.zeroCreate ((i + 1) * 2)
                elems.[0] <- elem
                List({Bucket = Bucket(elems, x, false); CurrentElementIndex = 0})
        member x.Count = 
            match x with
            | Empty -> 0
            | List({Bucket = b; CurrentElementIndex = i}) -> i + 1 + b.Previous.Count
    and Bucket<'t>(elems: 't[], previous: VList<'t>, isReadOnly) = 
        [<DefaultValue>] val mutable LatestIndex : int
        member x.IsReadOnly = isReadOnly 
        member x.Capacity = Array.length elems
        member x.Previous: VList<'t> = previous
        member x.Elems = elems

    and [<Struct; StructLayout(LayoutKind.Sequential, Pack = 4)>] Cons<'t> = {Bucket: Bucket<'t>; CurrentElementIndex: int}

    type [<Struct>] HeadTail<'t> = 
        | (::) of h: 't * t: VList<'t>

         
    let inline (|Unpack|) ({Bucket = b; CurrentElementIndex = i}) = 
        let tail = 
            if i = 0 then
                b.Previous
            else List({Bucket = b; CurrentElementIndex = i-1})
        let e = b.Elems.[i]
        Unpack(e::tail)


    let a = Empty.Cons(5L).Cons(12L).Cons(14L).Cons(14L).Cons(14L).Cons(14L).Cons(14L)

    let b  = 
        match a with
        | Empty -> 7L
        | List(Unpack(h::t)) -> h