
open System
open System.Runtime.CompilerServices

type IError = 
    abstract member GetErrorMessage: unit -> string

[<Struct;>] 
type ParsingError = { PositionStart: int; PositionEnd:int; Error: IError }
type Eq<'t> = delegate of 't *'t -> bool

type Status =
    | Success = 0
    | Failure = 1
    | FatalError = -1


module Parser = 
    [<IsByRefLike; Struct; NoComparison; NoEquality>]
    type UnsafeParserStream<'t> = 
        val mutable Errors : ResizeArray<ParsingError>
        val mutable Position: int
        val mutable Label: string
        val mutable Status: Status
        val Stream: ReadOnlySpan<'t>
        
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
            
        

        new(stream, pos) = {Stream = stream; Position = pos; Errors = Unchecked.defaultof<_>; Status = Status.Success; Label = ""}
        member inline x.TryGetCurrentToken(var: outref<'t>) =
            let p = x.Position
            if p < x.Stream.Length then
                var <- x.Stream.[p]
                true
            else false
            
        member inline x.CurrentStream = x.Stream.Slice(x.Position)
    
    [<Struct; NoComparison; NoEquality>]   
    type ArgumentHolder<'t1, 't2> =
        val mutable F1: 't1
        val mutable F2: 't2
        member x.ApplyTo f = f x.F1 x.F2

        
    [<Struct; NoComparison; NoEquality>]   
    type ArgumentHolder<'t1, 't2, 't3> =
        val mutable F1: ArgumentHolder<'t1, 't2>
        val mutable F2: 't3
        member x.ApplyTo f = f x.F1.F1 x.F1.F2 x.F2

        
    [<Struct; NoComparison; NoEquality>]   
    type ArgumentHolder<'t1, 't2, 't3, 't4> =
        val mutable F1: ArgumentHolder<'t1, 't2, 't3>
        val mutable F2: 't4
        member x.ApplyTo f = f x.F1.F1.F1 x.F1.F1.F2 x.F1.F2 x.F2

    
    [<Struct; NoComparison; NoEquality>]   
    type ArgumentHolder<'t1, 't2, 't3, 't4, 't5> =
        val mutable F1: ArgumentHolder<'t1, 't2, 't3, 't4>
        val mutable F2: 't5
        member x.ApplyTo f = f x.F1.F1.F1.F1 x.F1.F1.F1.F2 x.F1.F1.F2 x.F1.F2 x.F2

    type Parser<'tok, 'value> = delegate of byref<UnsafeParserStream<'tok>> * outref<'value> -> unit
    type Pipeline<'tok, 'a, 'b> = delegate of byref<UnsafeParserStream<'tok>> * outref<ArgumentHolder<'a, 'b>> -> unit
    type Pipeline<'tok, 'a, 'b, 'c> = delegate of byref<UnsafeParserStream<'tok>> * outref<ArgumentHolder<'a, 'b, 'c>> -> unit
    type Pipeline<'tok, 'a, 'b, 'c, 'd> = delegate of byref<UnsafeParserStream<'tok>> * outref<ArgumentHolder<'a, 'b, 'c, 'd>> -> unit
    type Pipeline<'tok, 'a, 'b, 'c, 'd, 'e> = delegate of byref<UnsafeParserStream<'tok>> * outref<ArgumentHolder<'a, 'b, 'c, 'd, 'e>> -> unit
    type P<'tok, 'value> = unit -> Parser<'tok, 'value>
    
    type Constructor<'t> = delegate of unit -> 't
    type Folder<'res, 'elem> = delegate of 'res * 'elem -> 'res
    type Inl = InlineIfLambdaAttribute
    
//    type IFunc<'t, 'res> =
//        abstract member Capture: 
    
    type ManyParser<'tok, 'value, 'finalValue>([<Inl>] parser: P<'tok, 'value>,  [<Inl>] folder: Folder<_,_>, [<Inl>] constructor: Constructor<_>, minCount, maxCount) =
        let error = {new IError with member x.GetErrorMessage() = $"Expected no less than {minCount} successfully parsed values. We found less, but input was consumed"}
        member x.ParseRepeatedly(stream: byref<UnsafeParserStream<'tok>>, currentCount: byref<int>, accumulatedValue: byref<'finalValue>) =
            if currentCount < maxCount then
                let mutable value = Unchecked.defaultof<_>
                (parser()).Invoke(&stream, &value)
                match stream with
                | {Status = Status.Success} ->
                    accumulatedValue <- folder.Invoke(accumulatedValue, value)
                    currentCount <- currentCount + 1
                    x.ParseRepeatedly(&stream, &currentCount, &accumulatedValue)
                | _ -> ()
        member x.Parse(stream: byref<UnsafeParserStream<'tok>>, v: outref<'finalValue>) =
            let initialPosition = stream.Position
            let mutable value = constructor.Invoke()
            let mutable count = 0
            x.ParseRepeatedly(&stream, &count, &value)
            if count >= minCount && stream.Status <> Status.FatalError then
                stream.SignalSuccess()
                v <- value
            else if count < minCount then
                if stream.Position <> initialPosition then
                    stream.SignalFatalError(initialPosition, stream.Position, error)
                else stream.SignalMiss()
            
            
                
            
    let inline matchAndReturn ([<Inl>]  eq: Eq<_>) valueToMatch valueToReturn : P<_,_> = 
        fun () ->
            Parser<_,_>
                (fun stream v ->
                    match stream.TryGetCurrentToken() with
                    | true, t when eq.Invoke(t, valueToMatch) ->
                        v <- valueToReturn
                        stream.Advance(1)
                    | _ -> stream.SignalMiss())
                
    let inline matchSequenceAndReturn ([<Inl>]  eq: Eq<_>) (seq: ReadOnlyMemory<_>) valueToReturn : P<_,_> =
        fun () ->
            Parser<_,_>
                (fun stream v ->
                    let length = seq.Length
                    let span = seq.Span
                    let mutable i = 0
                    let s = stream.CurrentStream
                    while i < length && eq.Invoke(s.[i], span.[i]) do
                        i <- i + 1
                    if i = length then
                        stream.Advance(length)
                        v <- valueToReturn
                    else stream.SignalMiss())
    let inline ret value =
        fun () -> Parser<_,_>(fun stream v -> stream.Advance(0); v <- value)

    let inline map ([<Inl>] f: 'a -> 'b) ([<Inl>] p: P<'t, 'a>) = 
        fun () ->
            Parser<_,_>
                (fun stream v ->
                    let mutable vo = Unchecked.defaultof<_>
                    (p()).Invoke(&stream, &vo)
                    if stream.Status = Status.Success then v <- f vo)
    
    let inline satisfyAndReturn ([<Inl>] f: 'a -> bool) valueToReturn = 
        fun () ->    
            (Parser<_,_>
                (fun stream v ->
                    match stream.TryGetCurrentToken() with
                    | true, t when f t ->
                        stream.Advance(1)
                        v <- valueToReturn
                    | _ -> stream.SignalMiss()))
            
            
    let inline satisfy ([<Inl>] f: 'a -> bool) = 
        fun () ->
            Parser<_,_>
                (fun stream v ->
                    match stream.TryGetCurrentToken() with
                    | true, t when f t ->
                        stream.Advance(1)
                        v <- t
                    | _ -> stream.SignalMiss())
    
               
    let inline oneOf (parsers: Parser<'t, 'a> list) =
        let parsers = Array.ofList parsers
        fun () ->
            Parser<_,_>(
                fun stream v ->
                    let mutable i = 0
                    let mutable shouldProceed = i < parsers.Length
                    while shouldProceed do
                        let current = parsers.[i]
                        current.Invoke(&stream, &v)
                        i <- i + 1
                        shouldProceed <- i < parsers.Length && stream.Status = Status.Failure)
    
    let inline many ([<Inl>] folder) ([<Inl>] ctr) minRepeats ([<Inl>] parser) =
        let manyParser = ManyParser(parser, Folder<_,_>(folder), Constructor<_>(ctr), minRepeats, Int32.MaxValue)
        fun () ->
            Parser<_,_>(fun stream v -> manyParser.Parse(&stream, &v))
    
    let inline map2 ([<Inl>] p1: P<'t, 'a>) ([<Inl>] p2: P<'t, 'b>) ([<Inl>] f) =
        fun () ->
            Parser<_,_>
                (fun stream v ->
                    let initialPosition = stream.Position
                    let mutable v1 = Unchecked.defaultof<_>
                    (p1()).Invoke(&stream, &v1)
                    if stream.Status = Status.Success then
                        let mutable v2 = Unchecked.defaultof<_>
                        (p2()).Invoke(&stream, &v2)
                        if stream.Status = Status.Success then
                            v <- f v1 v2
                        elif stream.Position <> initialPosition then
                            stream.SignalFatalError(initialPosition, stream.Position, Unchecked.defaultof<_>)
                        else stream.SignalMiss())
                
    
        
                
    let inline either ([<Inl>] p1: P<'t, 'a>) ([<Inl>] p2: P<'t, 'a>) =
        fun () ->
            Parser<_,_>
                (fun stream v ->
                    (p1()).Invoke(&stream, &v)
                    if stream.Status = Status.Failure then
                        (p2()).Invoke(&stream, &v))
   
    let inline (|>>) ([<Inl>] p: P<'t, 'a>) ([<Inl>] f) = map f p
    
    let inline (<|>) ([<Inl>] p1: P<'t, 'a>) ([<Inl>] p2: P<'t, 'a>) = either p1 p2
    
    let inline (<+) ([<Inl>] p1: P<'t, 'a>) ([<Inl>] p2: P<'t, 'b>) = map2 p1 p2 (fun a b -> a)
    let inline (+>) ([<Inl>] p1: P<'t, 'a>) ([<Inl>] p2: P<'t, 'b>) = map2 p1 p2 (fun a b -> b)
    




    type Mutator<'s, 'c when 's : struct> = delegate of byref<'s> -> byref<'c>
    
    type PH =
        class end 
        

    type PC =
        inherit PH
        static member inline (?<-)(_:PC, p1: P<'t,'a>, p2: P<'t,'b>) =
            fun () ->
                Pipeline<'t, 'a, 'b>
                    (fun stream v ->
                        let initialPosition = stream.Position
                        (p1()).Invoke(&stream, &v.F1)
                        if stream.Status = Status.Success then
                            (p2()).Invoke(&stream, &v.F2)
                            if stream.Status <> Status.Success then 
                                if stream.Position <> initialPosition then
                                    stream.SignalFatalError(initialPosition, stream.Position, Unchecked.defaultof<_>)) 

        static member inline (?<-)(_:PC, p1: unit -> Pipeline<'t, 'a, 'b>, p2: P<'t, 'c>) =
            fun () ->
                Pipeline<'t, 'a, 'b, 'c>
                    (fun stream v ->
                        let initialPosition = stream.Position
                        (p1()).Invoke(&stream, &v.F1)
                        if stream.Status = Status.Success then
                            (p2()).Invoke(&stream, &v.F2)
                            if stream.Status <> Status.Success then 
                                if stream.Position <> initialPosition then
                                    stream.SignalFatalError(initialPosition, stream.Position, Unchecked.defaultof<_>)) 

        
        static member inline (?<-)(_:PC, p1: unit -> Pipeline<'t, 'a, 'b, 'c>, p2: P<'t, 'd>) =
            fun () ->
                Pipeline<'t, 'a, 'b, 'c, 'd>
                    (fun stream v ->
                        let initialPosition = stream.Position
                        (p1()).Invoke(&stream, &v.F1)
                        if stream.Status = Status.Success then
                            (p2()).Invoke(&stream, &v.F2)
                            if stream.Status <> Status.Success then 
                                if stream.Position <> initialPosition then
                                    stream.SignalFatalError(initialPosition, stream.Position, Unchecked.defaultof<_>)) 
    
        static member inline (?<-)(_:PC, p1: unit -> Pipeline<'t, 'a, 'b, 'c, 'd>, p2: P<'t, 'e>) =
            fun () ->
                Pipeline<'t, 'a, 'b, 'c, 'd, 'e>
                    (fun stream v ->
                        let initialPosition = stream.Position
                        (p1()).Invoke(&stream, &v.F1)
                        if stream.Status = Status.Success then
                            (p2()).Invoke(&stream, &v.F2)
                            if stream.Status <> Status.Success then 
                                if stream.Position <> initialPosition then
                                    stream.SignalFatalError(initialPosition, stream.Position, Unchecked.defaultof<_>)) 
    
        static member inline (?<-)(_:PC, p1: unit -> Pipeline<'t, 'a, 'b>, f: 'a -> 'b -> 'c) =
            fun () ->
                Parser<'t, 'c>
                    (fun stream v ->
                        let mutable i = Unchecked.defaultof<_>
                        (p1()).Invoke(&stream, &i)
                        if stream.Status = Status.Success then
                            v <- i.ApplyTo f)
                    
        static member inline (?<-)(_:PC, p1: unit -> Pipeline<'t, 'a, 'b, 'c>, f: 'a -> 'b -> 'c -> 'd) =
            fun () ->
                Parser<'t, 'd>
                    (fun stream v ->
                        let mutable i = Unchecked.defaultof<_>
                        (p1()).Invoke(&stream, &i)
                        if stream.Status = Status.Success then
                            v <- i.ApplyTo f)
                    
        static member inline (?<-)(_:PC, p1: unit -> Pipeline<'t, 'a, 'b, 'c, 'd>, f: 'a -> 'b -> 'c -> 'd ->'e) =
            fun () ->
                Parser<'t, 'e>
                    (fun stream v ->
                        let mutable i = Unchecked.defaultof<_>
                        (p1()).Invoke(&stream, &i)
                        if stream.Status = Status.Success then
                            v <- i.ApplyTo f)
        static member inline (?<-)(_:PC, p1: unit -> Pipeline<'t, 'a, 'b, 'c, 'd, 'e>, f: 'a -> 'b -> 'c -> 'd ->'e -> 'f) =
            fun () ->
                Parser<'t, 'f>
                    (fun stream v ->
                        let mutable i = Unchecked.defaultof<_>
                        (p1()).Invoke(&stream, &i)
                        if stream.Status = Status.Success then
                            v <- i.ApplyTo f)
                    
    let inline (<*>) ([<Inl>] p1) ([<Inl>] p2) =
        (?<-) Unchecked.defaultof<PC> p1 p2

        

    let inline eq a b = (# "ceq" a b : bool #)
//    type 
//    
//    type ParserOp = ParserOp with
//        static member inline Arg1Setter< ^a, ^b when ^a: (member Arg1: ^b)> (s: byref< ^a>, a: outref< ^b>) =
//            a <- ((^a: (member Arg1: ^b) s))
//        static member inline (<+>) ([<Inl>] p1: P<'t, 'a>, [<Inl>] p2: P<'t, 'b>) f  = 
//            fun () ->
//                Parser<_,_>
//                    (fun stream v ->
//                        let initialPosition = stream.Position
//                        let mutable v1 = Unchecked.defaultof<_>
//                        (p1()).Invoke(&stream, &v1)
//                        if stream.Status = Status.Success then
//                            let mutable v2 = Unchecked.defaultof<_>
//                            (p2()).Invoke(&stream, &v2)
//                            if stream.Status = Status.Success then
//                                v <- f v1 v2
//                            elif stream.Position <> initialPosition then
//                                stream.SignalFatalError(initialPosition, stream.Position, Unchecked.defaultof<_>)
//                            else stream.SignalMiss())
//   

    let inline a1 ()  = matchAndReturn (Eq<_>eq) 'l' 1 ()   
    let inline a2 () = matchAndReturn  (Eq<_>eq) 'o' 2 ()
    let inline a3 () = matchAndReturn  (Eq<_>eq) 'h' 3 ()
    
    let firstLetter = a1 <|> a2 <|> a3
    
    
    
    let inline char (c: Char) () = satisfy (fun t -> t = c) ()
    
    let loh = char 'l' <+ char 'o' <+ char 'h' |>> (fun c -> [c; c; c; c;c])
    
    let loh2 = char 'k' <*> char 'l' <*> char 'o' 
    
    let b = map2 (matchAndReturn  (Eq<_>(=)) 'l' 10) (matchAndReturn  (Eq<_>(=)) 'o' 5) (fun a b -> a * b)
    let c = map (fun c -> c.ToString()) (map2 a1 a2 (*))
    
    let d = map2 a1 a2 (*) |>> (fun c -> float c / 500.)

    let run (str: string) =
        let mutable  unsafeParserStream = UnsafeParserStream(str.AsSpan(), 0)
        let mutable r = Unchecked.defaultof<_> 
        loh().Invoke(&unsafeParserStream, &r)
        printfn $"{unsafeParserStream.Status}"
        printfn $"{unsafeParserStream.Position}"
        printfn $"{r}"


open Parser

run "lohlohlohoban"

// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"
