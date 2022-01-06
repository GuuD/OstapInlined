
open System
open System.Diagnostics
open System.IO
open System.Runtime.CompilerServices
open Microsoft.FSharp.Core
open OstapDelegate.InternalTypes
open System.Runtime.InteropServices





module Parser =        
    let inline matchAndReturnWithEq ([<Inl>]  eq: Eq<_>) valueToMatch valueToReturn : P<_,_> = 
        fun () ->
            Parser<_,_>
                (fun stream v ->
                    match stream.TryGetCurrentToken() with
                    | true, t when eq.Invoke(t, valueToMatch) ->
                        v <- valueToReturn
                        stream.Advance(1)
                    | _ -> stream.SignalMiss())
                
    let inline matchSequenceAndReturnWithEq  ([<Inl>] eq: StartsWith<_>) (seq: ReadOnlyMemory<_>) valueToReturn : P<_,_> =
        fun () ->
            Parser<_,_>
                (fun stream v ->
                    let length = seq.Length
                    let span = seq.Span
                    if eq.Invoke(stream.Stream.Slice stream.Position, span) then
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
                
    let inline satisfyMap ([<Inl>] pred: 'a -> bool) ([<Inl>] f) = 
        fun () ->
            Parser<_,_>
                (fun stream v ->
                    match stream.TryGetCurrentToken() with
                    | true, t when pred t ->
                        stream.Advance(1)
                        v <- f t
                    | _ -> stream.SignalMiss())

    
    let inline many ([<Inl>] folder) ([<Inl>] ctr: unit -> 'r) min max ([<Inl>] p: P<_,_>) =
        fun () ->
            Parser<_,_>
                (fun stream v ->
                    let initialPosition = stream.Position
                    let mutable count = 0
                    v <- ctr()
                    let mutable parserResult = Unchecked.defaultof<_>
                    while stream.Status = Status.Success && count < max do
                        p().Invoke(&stream, &parserResult)
                        if stream.Status = Status.Success then
                            v <- folder v parserResult
                            count <- count + 1
                    if count >= min && stream.Status <> Status.FatalError then
                        stream.SignalSuccess()
                    else if count < min then
                        if stream.Position <> initialPosition then
                            stream.SignalFatalError(initialPosition, stream.Position, {new IError with member x.GetErrorMessage() = $"Expected no less than {min} successfully parsed values. We found less, but input was consumed"})
                        else stream.SignalMiss()
                )
    let inline manyCount ([<Inl>] folder) ([<Inl>] ctr: unit -> 'r) min max ([<Inl>] p: P<_,_>) =
        fun () ->
            Parser<_,_>
                (fun stream (v: outref<ArgumentHolder<_,_>>) ->
                    let initialPosition = stream.Position
//                    let mutable count = 0
//                    let mutable res = ctr()
                    v.F1 <- ctr()
                    let mutable parserResult = Unchecked.defaultof<_>
                    while stream.Status = Status.Success && v.F2 < max do
                        p().Invoke(&stream, &parserResult)
                        if stream.Status = Status.Success then
                            v.F1 <- folder v.F1 parserResult
                            v.F2 <- v.F2 + 1
                    if v.F2 >= min && stream.Status <> Status.FatalError then
                        stream.SignalSuccess()
                    else if v.F2 < min then
                        if stream.Position <> initialPosition then
                            stream.SignalFatalError(initialPosition, stream.Position, {new IError with member x.GetErrorMessage() = $"Expected no less than {min} successfully parsed values. We found less, but input was consumed"})
                        else stream.SignalMiss()
                )        
    let inline separated ([<Inl>] folder) ([<Inl>] ctr: unit -> 'r) min max ([<Inl>] sp: P<_,_>) ([<Inl>] p: P<_,_>) =
        fun () ->
            Parser<_,_>
                (fun stream v ->
                    let initialPosition = stream.Position
                    let mutable count = 0
                    v <- ctr()
                    let mutable parserResult = Unchecked.defaultof<_>
                    let mutable sr = Unchecked.defaultof<_>
                    while stream.Status = Status.Success && count < max do
                        p().Invoke(&stream, &parserResult)
                        if stream.Status = Status.Success then
                            v <- folder v parserResult
                            count <- count + 1
                            sp().Invoke(&stream, &sr)
                    if count >= min && stream.Status <> Status.FatalError then
                        stream.SignalSuccess()
                    else if count < min then
                        if stream.Position <> initialPosition then
                            stream.SignalFatalError(initialPosition, stream.Position, {new IError with member x.GetErrorMessage() = $"Expected no less than {min} successfully parsed values. We found less, but input was consumed"})
                        else stream.SignalMiss()
                )
                
    let inline manySkip min max ([<Inl>] p: P<_,_>) =
        fun () ->
            Parser<_,_>
                (fun stream v ->
                    let initialPosition = stream.Position
                    let mutable count = 0
                    let mutable parserResult = Unchecked.defaultof<_>
                    while stream.Status = Status.Success && count < max do
                        p().Invoke(&stream, &parserResult)
                        if stream.Status = Status.Success then
                            count <- count + 1
                    if count >= min && stream.Status <> Status.FatalError then
                        stream.SignalSuccess()
                        v <- ()
                    else if count < min then
                        if stream.Position <> initialPosition then
                            stream.SignalFatalError(initialPosition, stream.Position, {new IError with member x.GetErrorMessage() = $"Expected no less than {min} successfully parsed values. We found less, but input was consumed"})
                        else stream.SignalMiss()
                )
                
    let inline satisfyManySlice ([<Inl>] pred: 'a -> bool) =
        fun () ->
            Parser<_,_>
                (fun stream v ->
                    let skipped = stream.SkipWhile pred
                    if skipped > 0 then
                        v <- stream.Memory.Slice(stream.Position - skipped, skipped)
                    else stream.SignalMiss())
       
    let inline either ([<Inl>] p1: P<'t, 'a>) ([<Inl>] p2: P<'t, 'a>) =
        fun () ->
            Parser<_,_>
                (fun stream v ->
                    p1().Invoke(&stream, &v)
                    if stream.Status = Status.Failure then
                        p2().Invoke(&stream, &v))
   
    let inline (|>>) ([<Inl>] p: P<'t, 'a>) ([<Inl>] f) = map f p
    
    let inline (<|>) ([<Inl>] p1: P<'t, 'a>) ([<Inl>] p2: P<'t, 'a>) = either p1 p2
    
    let inline (<+) ([<Inl>] p1: P<'t, 'a>) ([<Inl>] p2: P<'t, 'b>) =
        fun () ->
            Parser<_,_>
                (fun stream v ->
                    let initialPosition = stream.Position
                    p1().Invoke(&stream, &v)
                    if stream.Status = Status.Success then
                        p2().Invoke(&stream) |> ignore
                        if stream.Status <> Status.Success then
                            if stream.Position <> initialPosition then
                                stream.SignalFatalError(initialPosition, stream.Position, Unchecked.defaultof<_>)
                            else stream.SignalMiss())
    let inline (+>) ([<Inl>] p1: P<'t, 'a>) ([<Inl>] p2: P<'t, 'b>) =
        fun () ->
            Parser<_,_>
                (fun stream v ->
                    let initialPosition = stream.Position
                    p1().Invoke(&stream) |> ignore
                    if stream.Status = Status.Success then
                        p2().Invoke(&stream, &v)
                        if stream.Status <> Status.Success then
                            if stream.Position <> initialPosition then
                                stream.SignalFatalError(initialPosition, stream.Position, Unchecked.defaultof<_>)
                            else stream.SignalMiss())
    let inline (|->) ([<Inl>] p: unit -> Parser< ^t, ^a> when ^a : (member ApplyTo: (^b -> ^c) -> ^d)) ([<Inl>] f: ^b -> ^c) =
        fun () ->
            Parser<_,_>
                (fun stream v ->
                    let mutable vo = Unchecked.defaultof<_>
                    (p()).Invoke(&stream, &vo)
                    if stream.Status = Status.Success then v <- (^a : (member ApplyTo: (^b -> ^c) -> ^d)(vo, f)))

            
    let inline (<+>) ([<Inl>] p1) ([<Inl>] p2: unit -> ^b) =
        fun () ->
        ((?<-) (p1()) (p2()) Unchecked.defaultof<PH>)()
        
    let inline (<?>) ([<Inl>] p: P<'t, 'a>) dv =
        fun () ->
            Parser<_,_>
                (fun stream v ->
                    (p()).Invoke(&stream, &v)
                    if stream.Status = Status.Failure then
                        stream.SignalSuccess()
                        v <- dv)
          
    let inline (<->) ([<Inl>] p: P<'t, 'a>) ([<Inl>] ps: P<'t, 'b>) =
        fun () ->
            Parser<_,_>
                (fun stream v ->
                let initialPosition = stream.Position
                p().Invoke(&stream, &v)
                if stream.Status = Status.Success then
                    ps().Invoke(&stream) |> ignore
                    if stream.Status <> Status.Success then 
                        if stream.Position <> initialPosition then
                            stream.SignalFatalError(initialPosition, stream.Position, Unchecked.defaultof<_>))
                
//    let inline peek ([<Inl>] p: P<'t, 'a>)  =
//        fun () ->
//            Parser<_,_>
//                (fun stream v ->
//                let initialPosition = stream.Position
//                let mutable ignore = Unchecked.defaultof<_> 
//                p().Invoke(&stream, &v)
//                if stream.Status = Status.Success then
//                    ps().Invoke(&stream, &ignore)
//                    if stream.Status <> Status.Success then 
//                        if stream.Position <> initialPosition then
//                            stream.SignalFatalError(initialPosition, stream.Position, Unchecked.defaultof<_>))
    let inline parse ([<Inl>] p: P<'t, 'a>) = p ()
    [<Literal>]
    let M = Int32.MaxValue
    let inline zeroOrMoreToList ([<Inl>] p: P<'t, 'a>) =
        many (fun ra e -> e::ra) (fun () -> []) 0 M p
    
    let inline oneOrMoreToList ([<Inl>] p: P<'t, 'a>) =
        many (fun ra e -> e::ra) (fun () -> []) 1 M p    
    
    let inline zeroOrMoreToResizeArray ([<Inl>] p: P<'t, 'a>) =
        many (fun (ra: ResizeArray<_>) e -> ra.Add e; ra) (ResizeArray<_>) 0 M p
    let inline oneOrMoreToResizeArray ([<Inl>] p: P<'t, 'a>) =
        many (fun (ra: ResizeArray<_>) e -> ra.Add e; ra) (ResizeArray<_>) 1 M p
        
        
    let inline zeroOrMoreSeparatedToResizeArray ([<Inl>] sp: P<_, _>) ([<Inl>] p: P<'t, 'a>) =
        separated (fun (ra: ResizeArray<_>) e -> ra.Add e; ra) (ResizeArray<_>) 0 M sp p
        
    let inline oneOrMoreSeparatedToResizeArray ([<Inl>] sp: P<_, _>) ([<Inl>] p: P<'t, 'a>) =
        separated (fun (ra: ResizeArray<_>) e -> ra.Add e; ra) (ResizeArray<_>) 1 M sp p
    
    
    let inline charAndReturn vtm vtr () = matchAndReturnWithEq (Eq<_>(=)) vtm vtr ()
    let inline stringAndReturn (stm: string) vtr () =
        matchSequenceAndReturnWithEq (StartsWith(fun a b -> a.StartsWith(b))) (stm.AsMemory()) vtr ()
    let inline ch (c: Char) () = charAndReturn c () |> parse
    open OstapDelegate.Operators
    let inline dot () = ch '.' |> parse
    
    let inline quote () = ch '"' |> parse
    
    let inline ``[`` () = ch '[' |> parse
    let inline ``]`` () = ch ']' |> parse
    
    let inline ``{`` () = ch '{' |> parse
    let inline ``}`` () = ch '}' |> parse
    let inline comma () = ch ',' |> parse
    let inline colon () = ch ':' |> parse
    let inline anySpace () = satisfy (fun c -> c = ' ' || c = '\r' || c = '\n' || c = '\t') |> parse
    let inline spaces () = manySkip 0 M anySpace |> parse
    let inline digit () =
        satisfyMap (fun c -> c >= '0' && c <= '9') (fun c -> int64 c - int64 '0') |> parse
    let inline integer () =
        many (fun acc e -> acc * 10L + e) (fun () -> 0L) 1 M digit |> parse

    let inline fractional () =
        dot
        <+> manyCount (fun acc e -> acc * 10L + int64 e) (fun () -> 0L) 1 M digit
        |-> fun v count -> (float v) / Math.Pow(10.0, float count)
        <?> 0.0
        |> parse
        
    let inline number () =
        integer
        <+> fractional
        => fun i f -> float i + f
        |> parse
    
    let inline signedInteger () =
        charAndReturn '-' -1L <?> 1L
        <+> integer
        |-> (*)
        |> parse
        
    let inline signedNumber () =
        charAndReturn '-' -1.0 <?> 1.0
        <+> number
        |-> (*)
        |> parse
        
    let inline simpleChar () =
        satisfy (fun c -> c <> '"' && c <> '\\') ()
        
    let inline simpleString () =
        satisfyManySlice (fun c -> c <> '"' && c <> '\\') ()
    let inline hexChar () =
        (satisfyMap (fun c -> c >= '0' && c <= '9') (fun c -> int c - int '0')
        <|> satisfyMap (fun c -> c >= 'A' && c <= 'F') (fun c -> int c + 10 - int 'A')
        <|> satisfyMap (fun c -> c >= 'a' && c <= 'f') (fun c -> int c + 10 - int 'a')) ()

    let inline unicodeChar () =
        (stringAndReturn "\\u" ()
        <+> many (fun acc v -> ( acc<<<4 ) + v) (fun () -> 0) 4 4 hexChar
        |>> char) ()
        

    
    let inline charInString () =
        (simpleChar
        <|> stringAndReturn "\\\"" '\"'
        <|> stringAndReturn "\\\\" '\\'
        <|> stringAndReturn "\\/" '/'
        <|> stringAndReturn "\\b" '\b'
        <|> stringAndReturn "\\f" '\f'
        <|> stringAndReturn "\\n" '\n'
        <|> stringAndReturn "\\r" '\r'
        <|> stringAndReturn "\\t" '\t'
        <|> unicodeChar)()

    let stringContent =
        let ra = ResizeArray<char>()
        let dictionary = System.Collections.Generic.Dictionary<_,_>()
        let p = 
            ((many (fun (acc : ResizeArray<char>) v -> acc.Add v; acc) (fun () -> ra.Clear(); ra) 0 M charInString)
            |>> (fun list -> 
                    if list.Count > 0 then 
                        let span = CollectionsMarshal.AsSpan list
                        span.ToString().AsMemory()
//                        let hashCode = String.GetHashCode(Span.op_Implicit span)
//                        let res =
//                            match dictionary.TryGetValue hashCode with
//                            | true, (v) -> v
//                            | _ ->
//                                let string = span.ToString().AsMemory()
//                                dictionary.[hashCode] <- string
//                                string
//                        res
                    else ReadOnlyMemory.Empty))()
        fun () -> p
    let inline cachedString () =
        (quote +> (simpleString <+ quote) <|> (stringContent <+ quote)) ()
    
    type JObject =
        | JString of ReadOnlyMemory<char>
        | JNumber of float
        | JBool of bool
        | JArray of JObject ResizeArray 
        | JObject of ResizeArray<struct(ReadOnlyMemory<char> * JObject)>
        | JNull
    let zero = JNumber 0.0
    let one = JNumber 1.0
    let inline jNumber () = 
        (signedNumber |>> function | 0.0 -> zero | 1.0 -> one | n -> JNumber n) ()
        
        
    let inline jString () =
        (cachedString |>> JString) ()
        
    let inline jNull () = stringAndReturn "null" JNull ()
    
    let trueVal = JBool true
    let falseVal = JBool false
    let inline jTrue () = (stringAndReturn "true" trueVal) ()
    let inline jFalse () = (stringAndReturn "false" falseVal) ()
    
    
    
    let inline jArray ([<Inl>] jObject) =
        ``[`` +> spaces +> zeroOrMoreSeparatedToResizeArray (comma <+ spaces) jObject <+ spaces <+ ``]`` <+ spaces
        |>> JArray
        
    let inline kvPair ([<Inl>] jObject) =
        ``{``
        +> spaces
        +> oneOrMoreSeparatedToResizeArray (comma <+ spaces) (cachedString <-> spaces <-> colon <-> spaces <+> jObject <-> spaces |-> fun k v -> struct(k, v))
        <+ spaces
        <+ ``}``
        <+ spaces
        |>> JObject
        
    let inline jsonParser () =
        
        let mutable parser = Unchecked.defaultof<_>
        parser <-
            (jNull
            <|> jNumber
            <|> jString
            <|> jArray (fun () -> parser)
            <|> kvPair (fun () -> parser)
            <|> jTrue
            <|> jFalse)()
        parser

    let run (str: string) =
        let mutable  unsafeParserStream = UnsafeParserStream(str.AsMemory(), 0)
        let mutable r = Unchecked.defaultof<_>
        let sw = Stopwatch.StartNew()
        let p = jsonParser()
        printfn "created in %i ms" sw.ElapsedMilliseconds
        p.Invoke(&unsafeParserStream, &r)
        printfn "parsed in %i ms" sw.ElapsedMilliseconds
        printfn $"{unsafeParserStream.Status}"
        printfn $"{unsafeParserStream.Position}"



open Parser

//run (File.ReadAllText "C:\\Developing\\Ostap4\\10000_example4.json") //"""{ "test": "5"} """
//run (File.ReadAllText "C:\\Developing\\Ostap4\\twitter.json") //"""{ "test": "5"} """
//run """{ "test": 5} """
run (File.ReadAllText "C:\\Developing\\Ostap4\\citylots.json") //"""{ "test": "5"} """
//run (File.ReadAllText "C:\\Developing\\Ostap4\\test2.json") //"""{ "test": "5"} """

// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"
