
open System
open OstapDelegate.InternalTypes






module Parser =
    type ManyParser<'tok, 'value, 'finalValue>([<Inl>] parser: Parser<'tok, 'value>,  [<Inl>] folder: 'finalValue-> 'value -> 'finalValue, [<Inl>] constructor: Constructor<_>, minCount, maxCount) =
        let error = {new IError with member x.GetErrorMessage() = $"Expected no less than {minCount} successfully parsed values. We found less, but input was consumed"}
        member x.ParseRepeatedly(stream: byref<UnsafeParserStream<'tok>>, currentCount: byref<int>, accumulatedValue: byref<'finalValue>) =
            if currentCount < maxCount then
                let mutable value = Unchecked.defaultof<_>
                (parser).Invoke(&stream, &value)
                match stream with
                | {Status = Status.Success} ->
                    accumulatedValue <- folder accumulatedValue value
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
            
            
                
            
    let inline matchAndReturnWithEq ([<Inl>]  eq: Eq<_>) valueToMatch valueToReturn : P<_,_> = 
        fun () ->
            Parser<_,_>
                (fun stream v ->
                    match stream.TryGetCurrentToken() with
                    | true, t when eq.Invoke(t, valueToMatch) ->
                        v <- valueToReturn
                        stream.Advance(1)
                    | _ -> stream.SignalMiss())
                
    let inline matchSequenceAndReturnWithEq  ([<Inl>] eq: EqSeq<_>) (seq: ReadOnlyMemory<_>) valueToReturn : P<_,_> =
        fun () ->
            Parser<_,_>
                (fun stream v ->
                    let length = seq.Length
                    let span = seq.Span
                    if stream.Stream.Length - stream.Position >= length then
                        let s = stream.Stream.Slice(stream.Position, length)
                        if eq.Invoke(s, span) then
                            stream.Advance(length)
                            v <- valueToReturn
                        else stream.SignalMiss()
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
    
    let inline many ([<Inl>] folder) ([<Inl>] ctr: unit -> 'r) minCount ([<Inl>] p: P<_,_>) =
        //let manyParser = ManyParser(parser(), folder, Constructor<_>(ctr), minRepeats, Int32.MaxValue)
        fun () ->
            Parser<_,_>
                (fun stream v ->
                    let initialPosition = stream.Position
                    let mutable count = 0
                    v <- ctr()
                    let mutable parserResult = Unchecked.defaultof<_>
                    while stream.Status = Status.Success do
                        p().Invoke(&stream, &parserResult)
                        if stream.Status = Status.Success then
                            v <- folder v parserResult
                            count <- count + 1
                    if count >= minCount && stream.Status <> Status.FatalError then
                        stream.SignalSuccess()
                    else if count < minCount then
                        if stream.Position <> initialPosition then
                            stream.SignalFatalError(initialPosition, stream.Position, {new IError with member x.GetErrorMessage() = $"Expected no less than {minCount} successfully parsed values. We found less, but input was consumed"})
                        else stream.SignalMiss()
                )
//        let p = Parser<_,_>(fun stream v -> manyParser.Parse(&stream, &v))
//        fun () -> p
    
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
    let inline (|->) ([<Inl>] p: unit -> Parser< ^t, ^a> when ^a : (member ApplyTo: (^b -> ^c) -> ^d)) ([<Inl>] f: ^b -> ^c) =
        fun () ->
            Parser<_,_>
                (fun stream v ->
                    let mutable vo = Unchecked.defaultof<_>
                    (p()).Invoke(&stream, &vo)
                    if stream.Status = Status.Success then v <- (^a : (member ApplyTo: (^b -> ^c) -> ^d)(vo, f)))

            
    let inline (<+>) ([<Inl>] p1) ([<Inl>] p2) =
        fun () -> (?<-) (p1()) (p2()) Unchecked.defaultof<PH>
        
    let inline (||>)  ([<Inl>] p1: P<'t, 'a>) ([<Inl>] f) =
        f p1
        
    let inline (<?>) ([<Inl>] p: P<'t, 'a>) dv =
        fun () ->
            Parser<_,_>
                (fun stream v ->
                    (p()).Invoke(&stream, &v)
                    if stream.Status = Status.Failure then
                        stream.SignalSuccess()
                        v <- dv)

    let inline matchAndReturn vtm vtr = matchAndReturnWithEq (Eq<_>(=)) vtm vtr
    let inline matchStringAndReturn stm vtr = matchSequenceAndReturnWithEq (EqSeq(fun a b -> MemoryExtensions.Equals(a, b, StringComparison.Ordinal))) stm vtr
    let inline char (c: Char) () = satisfy (fun t -> t = c) ()
    
    let inline integer () =
        many (fun acc e -> acc * 10L + int64 e) (fun () -> 0L) 1 (satisfy (fun c -> c >= '0' && c <= '9') |>> fun c -> int64 c - int64 '0') ()

    let inline fraction () =
        (satisfy (fun c -> c >= '0' && c <= '9') |>> fun c -> int64 c - int64 '0'
        ||> many (fun struct(exp, v) e -> struct(exp + 1, v * 10L + int64 e)) (fun () -> struct(0, 0L)) 1
        |>> fun struct(exp, v) -> (float v) / Math.Pow(10.0, float exp)) ()
    
    let inline signedInteger () =
        (matchAndReturn '-' -1 <?> 1
        <+> integer
        |-> fun s i -> int64 s * i)()
    

    let run (str: string) =
        let mutable  unsafeParserStream = UnsafeParserStream(str.AsSpan(), 0)
        let mutable r = Unchecked.defaultof<_> 
        integer().Invoke(&unsafeParserStream, &r)
        printfn $"{unsafeParserStream.Status}"
        printfn $"{unsafeParserStream.Position}"
        printfn $"{r}"


open Parser

run "42478212452g"

// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"
