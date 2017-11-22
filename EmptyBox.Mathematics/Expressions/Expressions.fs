#nowarn "86" //Operator warnings
namespace EmptyBox.Mathematics.Expressions

open System
open System.Collections.Generic
open System.Linq
open System.Collections
open System.Text
open System.Numerics
open System.Runtime.CompilerServices
open EmptyBox.ScriptRuntime.Extensions
open EmptyBox.Mathematics

/// <summary>
/// Перечисление, содержащее ошибки расчётов.
/// </summary>
type public ComputationErrors =
    | DivisionByZero = 0u
    | DevirativeByNotVariable = 1u
    | VectorTypeMismatch = 2u
    | MissingParameters = 3u
    | SetMismatch = 4u
    | ArgumentIsNotFromNaturalNumberWithZero = 5u
    | InternalError = 1000u
    
/// <summary>
/// Базовый тип, представляющий собой математическую функцию, результат которой является скаляром.
/// </summary>
type [<CustomEquality>] [<CustomComparison>] public ScalarFunction =
    /// <summary>
    /// Константа, не использующуюся при расчётах.
    /// </summary>
    | None
    /// <summary>
    /// Тип, экземпляр которого содержит в себе выражение и ошибку, полученную при его расчёте.
    /// </summary>
    | ComputationError of Function : ScalarFunction * Error : ComputationErrors
    /// <summary>
    /// Константа, являющаяся представлением мнимой еденицы.
    /// </summary>
    | ImaginaryOne
    /// <summary>
    /// Константа, являющаяся представлением числа Pi.
    /// </summary>
    | Pi
    /// <summary>
    /// Константа, являющаяся представлением числа E.
    /// </summary>
    | Exponent
    | GoldenNumber
    | SilverNumber
    | EulerConstant
    | PlasticNumber
    /// <summary>
    /// Константа, являющаяся представлением беззнаковой бесконечности.
    /// </summary>
    | Infinity
    /// <summary>
    /// Тип, описывающий задаваемую вещественную константу.
    /// </summary>
    | Constant of Value : double
    /// <summary>
    /// Тип, описывающий переменную.
    /// </summary>
    | Variable of Name : string
    /// <summary>
    /// Тип, описывающий переменную, ограниченную заданным условием.
    /// </summary>
    | DefinedVariable of Name : string * Definition : BinaryFunction
    //Trigonometry
    | Arccos of X : ScalarFunction
    | Arcctg of X : ScalarFunction
    | Arcsin of X : ScalarFunction
    | Arctg  of X : ScalarFunction
    | Cos of X : ScalarFunction
    | Cosec of X : ScalarFunction
    | Cot of X : ScalarFunction
    | Ctg of X : ScalarFunction
    | Sin of X : ScalarFunction
    | Sec of X : ScalarFunction
    | Tg of X : ScalarFunction
    /// <summary>
    /// Тип, описывающий функцию, вычисляющую целую часть от заданного скаляра.
    /// </summary>
    | IntegerPart of X : ScalarFunction
    | Floor of X : ScalarFunction
    | Ceiling of X : ScalarFunction
    | Negate of X : ScalarFunction
    | Confirmation of X : ScalarFunction
    | Module of X : ScalarFunction
    | Lg of X : ScalarFunction
    | Ln of X : ScalarFunction
    | Sign of X : ScalarFunction
    //Binary operations
    | Limit of X : ScalarFunction * Y : BinaryFunction
    | Integral of X : ScalarFunction * Y : ScalarFunction
    | DefinedIntegral of X : ScalarFunction * Y : ScalarFunction * A : ScalarFunction * B : ScalarFunction
    | CurvilinearIntegral of X : ScalarFunction * Y : ScalarFunction * L : BinaryFunction
    | SurfaceIntegral of X : ScalarFunction * Y : ScalarFunction * C : BinaryFunction
    | Devirative of X : ScalarFunction * Y : ScalarFunction
    | Division of X : ScalarFunction * Y : ScalarFunction
    | Power of X : ScalarFunction * Y : ScalarFunction
    | Remainder of X : ScalarFunction * Y : ScalarFunction
    | Subtraction of X : ScalarFunction * Y : ScalarFunction
    | VectorPower of X : VectorFunction * Y : ScalarFunction
    | Log of X : ScalarFunction * Y : ScalarFunction
    //Multicarne
    | Addition of X : IEnumerable<ScalarFunction>
    | Multiply of X : IEnumerable<ScalarFunction>
    //Other
    | Determinant of X : MatrixFunction
    | Length of X : VectorFunction
    | VectorElementsCount of X : VectorFunction
    | VectorMultiply of X : VectorFunction * Y : VectorFunction
    | VectorElementAt of X : VectorFunction * Index : ScalarFunction
    | MatrixRowsCount of X : MatrixFunction
    | MatrixColumnsCount of X : MatrixFunction
    | MatrixElementAt of X : MatrixFunction * RowIndex : ScalarFunction * ColumnIndex : ScalarFunction
    | Maximum of Functions : IEnumerable<ScalarFunction>

    override this.Equals(obj) =
        match obj with
        | :? ScalarFunction as x ->
            match BinaryFunction.ScalarEqual(this, x).Calculate() with
            | True -> true
            | _ -> false
        | _ -> false

    interface IComparable with
        member this.CompareTo(obj : obj) : int =
            match obj with
            | :? ScalarFunction as arg0 ->
                match BinaryFunction.op_True(BinaryFunction.ScalarGreaterThan(this, arg0)) with
                | true -> 1
                | false ->
                    match BinaryFunction.op_True(BinaryFunction.ScalarLessThan(this, arg0)) with
                    | true -> -1
                    | false ->
                         match BinaryFunction.op_True(BinaryFunction.ScalarEqual(this, arg0)) with
                         | true -> 0
                         | false -> -2
            | _ -> raise(ArgumentException())
     
    //interface IFormattable with
    //    member this.ToString(format : string, provider : IFormatProvider) : string = this.ToString(format, provider)

    static member op_Implicit(f0 : Complex) : ScalarFunction = Constant(f0.Real) + ImaginaryOne * Constant(f0.Imaginary)
    static member op_Implicit(f0 : Double) : ScalarFunction = Constant(f0)
    static member op_Implicit(f0 : String) : ScalarFunction = Variable(f0)
    static member op_Implicit(f0 : ScalarFunction) : Nullable<double> =
        match f0 with
        | Constant(value) -> Nullable(value)
        | _ -> Nullable()

    static member inline (~-) (x : ScalarFunction)                     : ScalarFunction = Negate(x)
    static member inline (~+) (x : ScalarFunction)                     : ScalarFunction = Confirmation(x)
    static member (*) (x : ScalarFunction, y : ScalarFunction) : ScalarFunction = Multiply([x; y])
    static member (+) (x : ScalarFunction, y : ScalarFunction) : ScalarFunction = Addition([x; y])
    static member inline (-) (x : ScalarFunction, y : ScalarFunction) : ScalarFunction = Subtraction(x, y)
    static member inline (/) (x : ScalarFunction, y : ScalarFunction) : ScalarFunction = Division(x, y)
    static member inline (%) (x : ScalarFunction, y : ScalarFunction) : ScalarFunction = Remainder(x, y)
    static member inline ( ** ) (x : ScalarFunction, y : ScalarFunction) : ScalarFunction = Power(x, y)
    static member inline (|=|) (x : ScalarFunction, y : ScalarFunction) : bool           = BinaryFunction.op_Implicit(BinaryFunction.ScalarStructEqual(x, y)).Value
    static member inline (|<>|) (x : ScalarFunction, y : ScalarFunction) : bool           = BinaryFunction.op_Implicit(BinaryFunction.ScalarStructNotEqual(x, y)).Value
    static member inline (<>) (x : ScalarFunction, y : ScalarFunction) : BinaryFunction = BinaryFunction.ScalarNotEqual(x, y)
    static member inline (=) (x : ScalarFunction, y : ScalarFunction) : BinaryFunction = BinaryFunction.ScalarEqual(x, y)
    static member inline (>) (x : ScalarFunction, y : ScalarFunction) : BinaryFunction = BinaryFunction.ScalarGreaterThan(x, y)
    static member inline (<) (x : ScalarFunction, y : ScalarFunction) : BinaryFunction = BinaryFunction.ScalarLessThan(x, y)
    static member inline (>=) (x : ScalarFunction, y : ScalarFunction) : BinaryFunction = BinaryFunction.ScalarGreaterThanOrEqual(x, y)
    static member inline (<=) (x : ScalarFunction, y : ScalarFunction) : BinaryFunction = BinaryFunction.ScalarLessThanOrEqual(x, y)

    static member Pow(x, y) =
        Power(x, y)
    static member Zero
        with get() = Constant(0.)
    static member One
        with get() = Constant(1.)

    static member private _RecursiveHash(basehash : int, list : list<ScalarFunction>) : int =
        match list with
        | head :: tail -> ScalarFunction._RecursiveHash(basehash ^^^ head.GetHashCode(), tail)
        | [] -> basehash

    override this.GetHashCode() =
        match this with
        | None -> 0
        | ImaginaryOne -> -1
        | Pi -> Math.PI.GetHashCode()
        | Exponent -> Math.E.GetHashCode()
        | Constant(value) -> value.GetHashCode()
        | Variable(name) | DefinedVariable(Name = name) -> name.GetHashCode()
        | Power(fun0, fun1) -> 4524 ^^^ fun0.GetHashCode() ^^^ fun1.GetHashCode()
        | Subtraction(fun0, fun1) -> 1348 ^^^ fun0.GetHashCode() ^^^ fun1.GetHashCode()
        | Division(fun0, fun1) -> 9825 ^^^ fun0.GetHashCode() ^^^ fun1.GetHashCode()
        | Log(fun0, fun1) -> 7981 ^^^ fun0.GetHashCode() ^^^ fun1.GetHashCode()
        | Ln(fun0) -> 7981 ^^^ Math.E.GetHashCode() ^^^ fun0.GetHashCode()
        | Lg(fun0) -> 7981 ^^^ (10.).GetHashCode() ^^^ fun0.GetHashCode()
        | Addition(list0) -> ScalarFunction._RecursiveHash(2596, List.ofSeq(list0))
        | Multiply(list0) -> ScalarFunction._RecursiveHash(6574, List.ofSeq(list0))
        | _ -> -2

    member this.Priority
        with get() =
            match this with
            | Constant _ | Variable _ | DefinedVariable _ | Pi | Exponent | ImaginaryOne -> 255uy
            | Addition _ | Subtraction _ -> 1uy
            | Multiply _ | Division _  | Remainder _ -> 2uy
            | Power _ | Log _ | Ln _ | Lg _ | Negate _ -> 3uy
            | Sin _ | Cos _ | Tg _ | Ctg _ | Arcsin _ | Arccos _ | Arctg _ | Arcctg _ | Sec _ | Cosec _ | Cot _ -> 4uy
            | Module _ | IntegerPart _ | Floor _ | Ceiling _ | Devirative _ -> 254uy
            | _ -> 0uy

    member this.Identificator
        with get() : string[] =
            match this with
            | Addition _ | Confirmation _ -> [|"+"|]
            | Multiply _ | VectorMultiply _ -> [|"*"|]
            | Subtraction _ | Negate _ -> [|"-"|]
            | Division _ -> [|"/"|]
            | Power _ -> [|"^"; "√"; "∛"; "∜"|]
            | Sin _ -> [|"sin"|]
            | Cos _ -> [|"cos"|]
            | Tg _ -> [|"tg"|]
            | Ctg _ -> [|"ctg"|]
            | Arcsin _ -> [|"arcsin"|]
            | Arccos _ -> [|"arccos"|]
            | Arctg _ -> [|"arctg"|]
            | Arcctg _ -> [|"arcctg"|]
            | Sec _ -> [|"sec"|]
            | Cosec _ -> [|"cosec"|]
            | Cot _ -> [|"cot"|]
            | Module _ -> [|"|"|]
            | Length _ -> [|"|"|]
            | IntegerPart _ -> [|"["; "]"|]
            | Floor _ -> [|"⎣"; "⎦"|]
            | Ceiling _ -> [|"⎡"; "⎤"|]
            | Limit _ -> [|"→"|]
            | Devirative _ -> [|"'";"∂"|]
            | Integral _ | DefinedIntegral _ -> [|"∫"; "∬"; "∭"|]
            | CurvilinearIntegral _ -> [|"∱"; "∲"; "∳"|]
            | SurfaceIntegral _ -> [|"∮"; "∯"; "∰"|]
            | Remainder _ -> [|"%"|]
            | Determinant _ -> [|"∆"|]
            | Log _ -> [|"log"|]
            | Ln _ -> [|"ln"|]
            | Lg _ -> [|"lg"|]
            | ImaginaryOne -> [|"i"|]
            | Exponent -> [|"e"|]
            | Pi -> [|"π"|]
            | GoldenNumber -> [|"Φ"|]
            | SilverNumber -> [|"δₛ"|]
            | EulerConstant -> [|"γ"|]
            | PlasticNumber -> [|"ρ"|]
            | Infinity -> [|"∞"|]
            | Variable(name) -> [|name|]
            | Constant(value) -> [|value.ToString()|]
            | None -> [|"‽"|]
            | _  -> [|"?"|]

    member this.Contains(func : ScalarFunction) : bool =
        match this with
        | x when x = func -> true
        | Addition(functions) | Multiply(functions) -> functions.Any(fun x -> x.Contains(func))
        | Negate(x) | Module(x) | Sin(x) | Cos(x) | Tg(x) | Ctg(x) | Ln(x) | Lg(x) | Arcsin(x) | Arccos(x) | Arctg(x) | Arcctg(x) | Sec(x) | Cosec(x) | Cot(x) -> x.Contains(func)
        | Subtraction(x, y) | Division(x, y) | Power(x, y) | Devirative(x, y) | Log(x, y) | Remainder(x, y) -> x.Contains(func) || y.Contains(func)
        | _ -> false

    static member private _1(x : ScalarFunction, list : list<ScalarFunction>, func : ScalarFunction * ScalarFunction -> ScalarFunction) =
        match x with
        | ScalarFunction.None ->
            match list with
            | [] -> []
            | head :: tail -> ScalarFunction._1(head, tail, func)
        | x ->
            let x = x.Calculate()
            match list with
            | head :: tail ->
                let head = head.Calculate()
                match func(x, head) with
                | ScalarFunction.None ->
                    let next = ScalarFunction._1(x, tail, func)
                    match next with
                    | next_head :: next_tail when next_head = x -> next_head :: ScalarFunction._1(head, next_tail, func)
                    | x -> ScalarFunction._1(head, x, func)
                | x -> ScalarFunction._1(x, tail, func)
            | [] -> x :: []

    static member private _CheckAddition(val0 : ScalarFunction, val1 : ScalarFunction) : ScalarFunction =
        let check = val0 + val1
        match check.Calculate() with
        | x when x |<>| check -> x
        | _ -> ScalarFunction.None

    static member private _CheckMultiply(val0 : ScalarFunction, val1 : ScalarFunction) : ScalarFunction =
        let check = val0 * val1
        match check.Calculate() with
        | x when x |<>| check -> x
        | _ -> ScalarFunction.None

    member this.Calculate() = this.Calculate(True)
    member this.Calculate(definition : BinaryFunction) =
        match this with
        | Constant(value) ->
            match value with
            | Math.E -> Exponent
            | Math.PI -> Pi
            | x -> this
        | Variable(name) ->
            let x = ScalarFind(this, definition).Calculate()
            match x with
            | ScalarAny(x, True) -> x
            | ScalarEqual(x, y) -> y
            | x -> DefinedVariable(name, x)
        | Addition(functions) ->
            let functions = functions.OrderByDescending(fun x -> x.Priority)
            match functions.Count() with
            | 0 -> Constant(0.)
            | 1 -> functions.ElementAt(0).Calculate(definition)
            | 2 ->
                let x = functions.ElementAt(0).Calculate(definition)
                let y = functions.ElementAt(1).Calculate(definition)
                match (x, y) with
                | (Constant(a), x) | (x, Constant(a)) when a = 0. -> x
                | (Constant(a), Constant(b)) -> Constant(a + b)
                | (x, y) when x = y -> Constant(2.) * x
                | (Negate(x), y) | (y, Negate(x)) ->
                    match y with
                    | Negate(y) -> (-(x + y)).Calculate()
                    | Subtraction(y, z) -> (y - (x + z)).Calculate()
                    | _ -> (y - x).Calculate()
                | (Addition(list0), x) | (x, Addition(list0)) ->
                    match x with
                    | Addition(list1) -> Addition(list0.Concat(list1)).Calculate()
                    | Subtraction(x, y) -> (Addition(list0.Concat([|x|])) - y).Calculate()
                    | _ -> Addition(list0.Concat([x])).Calculate()
                | (Subtraction(sfun2, sfun3), ufun1) | (ufun1, Subtraction(sfun2, sfun3)) ->
                    match ufun1 with
                    | Subtraction (sfun4, sfun5) -> ((sfun2 + sfun4) - (sfun3 + sfun5)).Calculate()
                    | _ -> ((ufun1 + sfun2).Calculate() - sfun3)
                | (Multiply(list1), ufun1) | (ufun1, Multiply(list1)) ->
                    match ufun1 with
                    | Multiply(list2) ->
                        let intersect = list1.Intersect(list2)
                        match intersect.Count() with
                        | 0 -> Multiply(list1) + Multiply(list2) //Без оптимизации, ибо выражение возвращается тоже самое, а сами функции были оптимизированы выше
                        | _ ->
                            let except0 = list1.Except(intersect)
                            let except1 = list2.Except(intersect)
                            let func =
                                match except0.Count() with
                                | 0 -> Constant(0.)
                                | 1 -> except0.ElementAt(0)
                                | _ -> Multiply(except0)
                            let func =
                                match except1.Count() with
                                | 0 -> func
                                | 1 -> func + except1.ElementAt(0)
                                | _ -> func + Multiply(except1)
                            match intersect.Count() with
                            | 1 -> (intersect.ElementAt(0) * func).Calculate()
                            | _ -> Multiply(intersect.Append(func)).Calculate()
                    | _ ->
                        match list1.Contains(ufun1) with
                        | true ->
                            let list3 = list1.SetExcept([|ufun1|])
                            let multiplier =
                                match list3.Count() with
                                | 0 -> raise (Exception("Something wrong in Addition_FunFun->Multiply->_ with list2"))
                                | 1 -> (list3.ElementAt(0) + Constant(1.)).Calculate()
                                | _ -> Multiply(list3) + Constant(1.)
                            (multiplier * ufun1).Calculate()
                        | false -> Multiply(list1) + ufun1  //Без оптимизации, ибо выражение возвращается тоже самое, а сами функции были оптимизированы выше
                | (Division(x0, y0), z) | (z, Division(x0, y0)) ->
                    match z with
                    | Division(x1, y1) ->
                        match y0 = y1 with
                        | true -> ((x0 + x1) / y0).Calculate()
                        | false -> x0 / y0 + x1 / y1  //Без оптимизации, ибо выражение возвращается тоже самое, а сами функции были оптимизированы выше
                    | _ -> x0 / y0 + z
                | (Power(Sin(x), Constant(a)), Power(Cos(y), Constant(b))) | (Power(Cos(y), Constant(b)), Power(Sin(x), Constant(a))) when x = y && a = 2. && b = 2. -> Constant(1.)
                | _ -> x + y
            | _ -> Addition(ScalarFunction._1(None, List.ofSeq(functions), ScalarFunction._CheckAddition))
        | Subtraction(x, y) ->
            let x = x.Calculate(definition)
            let y = y.Calculate(definition)
            match (x, y) with
            | (x, y) when x = y -> Constant(0.)
            | (Constant(a), Constant(b)) -> Constant(a - b)
            | (Constant(a), y) when a = 0. -> (-y).Calculate()
            | (x, Constant(a)) when a = -0. -> x
            | (Negate(x), Negate(y)) -> (y - x).Calculate()
            | (Negate(x), y) -> (-(x + y)).Calculate()
            | (x, Negate(y)) -> (x + y).Calculate()
            | (Division(x0, y0), Division(x1, y1)) ->
                match y0 = y1 with
                | true -> ((x0 - x1) / y0).Calculate()
                | false -> x0 / y0 - x1 / y1
            | (Subtraction(x0, y0), Subtraction(x1, y1)) -> ((x0 + y1) - (x1 + y0)).Calculate()
            | (Subtraction(x, y), z) -> (x - (y + z)).Calculate()
            | (x, Subtraction(y, z)) -> ((x + z) - y).Calculate()
            | (Addition(list0), y) ->
                let list0 = List.ofSeq(list0)
                let rec f(x : list<ScalarFunction>, a : ScalarFunction) : ScalarFunction =
                    match x with
                    | [] -> -a
                    | x :: z ->
                        let check = x - y
                        match check.Calculate() with
                        | x when x |<>| check -> 
                            match z with
                            | [] -> x
                            | z :: [] -> (z + x).Calculate()
                            | z -> (Addition(z) + x).Calculate()
                        | _ -> (x + f(z, a)).Calculate()
                f(list0, y)
            | (x, Addition(list0)) ->
                let list0 = List.ofSeq(list0)
                let rec f(x : list<ScalarFunction>, a : ScalarFunction) : ScalarFunction =
                    match x with
                    | [] -> a
                    | x :: y ->
                        let check = a - x
                        match check.Calculate() with
                        | x when x |<>| check -> 
                            match y with
                            | [] -> x
                            | y :: [] -> (x - y).Calculate()
                            | y -> (x - Addition(y)).Calculate()
                        | _ ->
                            let check1 = a - Addition(y).Calculate()
                            let check2 = f(y, a)
                            match check2.Calculate() with
                            | z when z |<>| check1 -> (z - x).Calculate()
                            | _ -> a - Addition(x :: y).Calculate()
                f(list0, x)
            | _ -> x - y
        | Multiply(functions) ->
            let functions = functions.OrderByDescending(fun x -> x.Priority)
            match functions.Count() with
            | 0 -> Constant(0.)
            | 1 -> functions.ElementAt(0).Calculate(definition)
            | 2 ->
                let fun0 = functions.ElementAt(0).Calculate(definition)
                let fun1 = functions.ElementAt(1).Calculate(definition)
                match (fun0, fun1) with
                | _ when fun0 = Constant(0.) || fun1 = Constant(0.) -> Constant(0.)
                | _ when fun0 = Constant(1.) -> fun1
                | _ when fun1 = Constant(1.) -> fun0
                | _ when fun0 = Constant(-1.) -> (-fun1).Calculate()
                | _ when fun1 = Constant(-1.) -> (-fun0).Calculate()
                | (Constant(value0), Constant(value1)) -> Constant(value0 * value1)
                | (Negate(x), Negate(y)) -> (x*y).Calculate()
                | _ when fun0 = fun1 -> Power(fun0, Constant(2.))
                | (Multiply(list0), ufun1) | (ufun1, Multiply(list0)) ->
                    match ufun1 with
                    | Multiply(list1) -> Multiply(list0.Concat(list1)).Calculate()
                    | Division(left, rigth) -> (Multiply(list0.Concat([|left|])) / rigth).Calculate()
                    | _ -> Multiply(list0.Concat([|ufun1|])).Calculate()
                | (Power(left0, rigth0), ufun1) | (ufun1, Power(left0, rigth0)) ->
                    match ufun1 with
                    | _ when left0 = ufun1 -> (left0 ** (rigth0 + Constant(1.))).Calculate()
                    | Power(left1, rigth1) ->
                        match left1 with
                        | _ when rigth0 = rigth1 -> ((left0 * left1) ** left1).Calculate()
                        | Multiply(functions1) ->
                            let func =
                                match left0 with
                                | Multiply(functions0) -> functions0
                                | x -> [|x|].AsEnumerable()
                            let intersect = func.Intersect(functions1)
                            match intersect.Count() with
                            | 0 -> left0 ** rigth0 * left1 ** rigth1 //Ничего не изменилось, всё оптимизированно
                            | _ ->
                                let result =
                                    match intersect.Count() with
                                    | 1 -> intersect.ElementAt(0) ** (rigth0 + rigth1)
                                    | _ -> Multiply(intersect) ** (rigth0 + rigth1)
                                let functions2 = func.Except(intersect)
                                let functions3 = functions1.Except(intersect)
                                let result =
                                    match functions2.Count() with
                                    | 0 -> result
                                    | 1 -> result * functions2.ElementAt(0) ** rigth0
                                    | _ -> result * Multiply(functions2) ** rigth0
                                match functions3.Count() with
                                | 0 -> result.Calculate()
                                | 1 -> (result * functions3.ElementAt(0) ** rigth1).Calculate()
                                | _ -> (result * Multiply(functions3) ** rigth1).Calculate()
                        | _ -> fun0 * fun1
                    | _ -> fun0 * fun1
                | (Division(left0, rigth0), ufun1) | (ufun1, Division(left0, rigth0)) ->
                    match ufun1 with
                    | Division(left1, rigth1) -> ((left0 * rigth1) / (left1 * rigth0)).Calculate()
                    | _ -> ((left0 * ufun1) / rigth0).Calculate()
                | (Tg(x), Ctg(y)) | (Ctg(x), Tg(y)) when x = y -> Constant(1.)
                | _ -> fun0 * fun1
            | _ ->
                let f3 =
                    match functions.Count() with
                    | 3 ->
                        let fun0 = functions.ElementAt(0).Calculate(definition)
                        let fun1 = functions.ElementAt(1).Calculate(definition)
                        let fun2 = functions.ElementAt(2).Calculate(definition)
                        match (fun0, fun1, fun2) with
                        | (x, Cos(y), Sin(z)) | (Cos(y), x, Sin(z)) | (Cos(y), Sin(z), x) | (Sin(z), Cos(y), x) | (x, Sin(z), Cos(y)) | (Sin(z), x, Cos(y)) when x = Constant(2.) && y = z -> Sin(Constant(2.) * y)
                        | _ -> None
                    | _ -> None
                match f3 with
                | None -> Multiply(ScalarFunction._1(None, List.ofSeq(functions), ScalarFunction._CheckMultiply))
                | _ -> f3
        | Division(fun0, fun1) ->
            let fun0 = fun0.Calculate(definition)
            let fun1 = fun1.Calculate(definition)
            match (fun0, fun1) with
            | (fun0, fun1) when fun0 = fun1 -> Constant(1.)
            | (Constant(a), y) when a = 0. -> Constant(0.)
            | (_, Constant(b)) when b = 0. -> ComputationError(this, ComputationErrors.DivisionByZero)
            | (x, Constant(b)) when b = 1. -> x
            | (Constant(a), Constant(b)) -> Constant(a / b)
            | (Power(x0, y0), Power(x1, y1)) ->
                match (ScalarEqual(x0, x1).Calculate(), ScalarEqual(y0, y1).Calculate()) with
                | (True, _) -> (x0 ** (y0 - y1)).Calculate()
                | (_, True) -> ((x0 / x1) ** y0).Calculate()
                | (_, _) ->
                    match ScalarStructNotEqual(x0 / x1, (x0 / x1).Calculate()) with
                    | True ->
                        let max = Maximum([y0; y1])
                        ((x0 / x1) ** max * x0 ** (max - y0) / x1 ** (max - y1)).Calculate()
                    | _ -> x0 ** y0 / x1 ** y1
            | (Power(x0, y0), y) ->
                match ScalarStructNotEqual(x0 / y, (x0 / y).Calculate()) with
                | True -> ((x0 / y) * x0 ** (y0 - Constant(1.))).Calculate()
                | _ -> x0 ** y0 / y
            | (x, Power(x1, y1)) ->
                match ScalarStructNotEqual(x / x1, (x / x1).Calculate()) with
                | True -> ((x / x1) * x1 ** (y1 - Constant(1.))).Calculate()
                | _ -> x / x1 ** y1
            | (Addition(list0), fun1) ->
                let mutable free_part = List<ScalarFunction>()
                let mutable other_part = List<ScalarFunction>()
                for func in list0 do
                    let check = func / fun1
                    match check.Calculate() with
                    | x when x |<>| check -> free_part.Add(x)
                    | _ -> other_part.Add(func)
                match (free_part.Count, other_part.Count) with
                | (0, _) -> Addition(other_part) / fun1
                | (1, _) -> free_part.ElementAt(0) + Addition(other_part) / fun1
                | (_, 1) -> Addition(free_part) + other_part.ElementAt(0) / fun1
                | (_, 0) -> Addition(free_part)
                | (_, _) -> Addition(free_part) + Addition(other_part) / fun1
            | (Subtraction(fun00, fun01), fun1) ->
                let check0 = fun00 / fun1
                let check1 = fun01 / fun1
                match (check0.Calculate(), check1.Calculate()) with
                | (fun10, fun11) when fun10 = check0 && fun11 = check1 -> (fun00 - fun01) / fun1
                | (fun10, fun11) -> (fun10 - fun11).Calculate()
            | (Multiply(list0), fun1) ->
                match fun1 with
                | Multiply(list1) ->
                    let mutable list0 = List(list0)
                    let mutable list1 = List(list1)
                    let mutable i0 = 0
                    while i0 < list0.Count do
                        let mutable accum = list0.ElementAt(i0)
                        let mutable i1 = 0
                        let mutable changed = false
                        while i1 < list1.Count do
                            let check = accum / list1.ElementAt(i1)
                            match check.Calculate() with
                            | x when x |<>| check ->
                                list1.Remove(list1.ElementAt(i1)) |> ignore
                                accum <- x
                                changed <- true
                            | _ -> i1 <- i1 + 1
                        match changed with
                        | true ->
                            list0.Remove(list0.ElementAt(i0)) |> ignore
                            list0.Add(accum)
                        | false ->
                            i0 <- i0 + 1
                    match (list0.Count, list1.Count) with
                    | (0, 1) -> (list1.ElementAt(0) ** Constant(-1.)).Calculate()
                    | (0, _) -> (Multiply(list1) ** Constant(-1.)).Calculate()
                    | (1, 0) -> list0.ElementAt(0)
                    | (1, 1) -> (list0.ElementAt(0) / list1.ElementAt(0)).Calculate()
                    | (1, _) -> (list0.ElementAt(0) / Multiply(list1)).Calculate()
                    | (_, 1) -> (Multiply(list0) / list1.ElementAt(0)).Calculate()
                    | (_, 0) -> Multiply(list0).Calculate()
                    | (_, _) -> (Multiply(list0) / Multiply(list1)).Calculate()
                | Power(fun10, fun11) -> Multiply(list0.Concat([|fun10 ** -fun11|])).Calculate()
                | x ->
                    let mutable list1 = []
                    let mutable en = false
                    for y in list0 do
                        let check = y / x
                        match check.Calculate() with
                        | z when z |<>| check && not(en) ->
                            en <- true
                            list1 <- list1 @ z :: []
                        | _ -> list1 <- list1 @ y :: []
                    match en with
                    | true -> Multiply(list1).Calculate()
                    | false -> Multiply(list1) / x
            | (x, Multiply(list0)) ->
                match x with
                | x ->
                    let mutable list1 = []
                    let mutable val0 = ScalarFunction.None
                    let mutable en = false
                    for y in list0 do
                        let check = x / y
                        match check.Calculate() with
                        | z when z |<>| check && not(en) ->
                            en <- true
                            val0 <- z 
                        | _ -> list1 <- list1 @ y :: []
                    match en with
                    | true -> (val0 / Multiply(list1)).Calculate()
                    | false -> x / Multiply(list1)
            | (Division(x, y), Division(z, t)) -> ((x*t) / (y*z)).Calculate()
            | (x, Division(y, z)) -> ((x * z) / y).Calculate()
            | (Division(x, y), z) -> (x / (y * z)).Calculate()
            | _ -> fun0 / fun1
        | Negate(func) ->
            let func = func.Calculate(definition)
            match func with
            | Infinity -> -Infinity
            | Negate(func) -> func
            | Constant(value) -> Constant(-value)
            | Subtraction(fun0, fun1) -> fun1 - fun0
            | x -> Negate(x)
        | Power(x, y) ->
            let x = x.Calculate(definition)
            let y = y.Calculate(definition)
            match (x, y) with
            | (Constant(a), Constant(b)) ->
                match (a, b) with
                | (0., 0.) | (1., _) | (_, 0.) -> Constant(1.)
                | (0., _) -> Constant(0.)
                | (a, 1.) -> Constant(a)
                | (a, -1.) -> Constant(1. / a)
                | (a, b) when a < 0. && abs(b) < 1. && b % 2. = 0. -> (Constant(-a ** b) * ImaginaryOne ** Constant(b * 2.)).Calculate()
                | (a, b) -> Constant(a ** b)
            | (ImaginaryOne _, Constant(b)) when abs(b) >= 2. -> (Constant(-1.) * ImaginaryOne ** Constant((double(sign(b)) * (abs(b) - 1.)))).Calculate()
            | (_, Constant(b)) when b = 0. -> Constant(1.)
            | (x, Constant(b)) when b = 1. -> x
            | (Exponent, Ln(y)) -> y
            | (Constant(a), Lg(y)) when a = 10. -> y
            | (x, Log(y, z)) when x = y -> z
            | (Power(x, y), z) -> (x ** (y * z)).Calculate()
            | _ -> x ** y
        | Ln(func) ->
            let func = func.Calculate(definition)
            match func with
            | Constant(value) -> Constant(Math.Log(value))
            | Exponent -> Constant(1.)
            | Power(left, rigth) -> rigth * Ln(left) //А это может оптимизироваться??
            | _ -> Ln(func)
        | Lg(func) ->
            let func = func.Calculate(definition)
            match func with
            | Constant(value) -> Constant(Math.Log10(value))
            | Power(left, rigth) -> rigth * Lg(left) //А это может оптимизироваться??
            | _ -> Lg(func)
        | Sin(func) ->
            let func = func.Calculate(definition)
            match func with
            | Pi -> Constant(0.)
            | Multiply(list0) when list0.Contains(Pi) && BinaryFunction.op_True(Multiply(list0.SetExcept([Pi])) |+| WholeNumbers) -> Constant(0.)
            | Constant(value) -> Constant(Math.Sin(value))
            | Arcsin(func) -> func
            | _ -> Sin(func)
        | Cos(func) ->
            let func = func.Calculate(definition)
            match func with
            | Pi -> Constant(-1.)
            | Constant(value) -> Constant(Math.Cos(value))
            | Arccos(func) -> func
            | _ -> Cos(func)
        | Devirative(func, var) ->
            let func = func.Calculate(definition)
            match var with
            | Variable _ ->
                match func with
                | Constant _ | DefinedVariable _ | VectorElementsCount _ -> Constant(0.)
                | Variable _ ->
                    match func = var with
                    | true -> Constant(1.)
                    | false -> Constant(0.)
                | Sin(x) -> (Devirative(x, var) * -Cos(x)).Calculate()
                | Cos(x) -> (Devirative(x, var) * Sin(x)).Calculate()
                | Power(x, y) -> (y * Devirative(x, var) * (x ** (y - Constant(1.))) + Devirative(y, var) * (x ** y) * Ln(x)).Calculate()
                | Addition(list0) -> Addition(list0.Select(fun x -> Devirative(x, var))).Calculate()
                | _ -> this
            | _ -> ComputationError(this, ComputationErrors.DevirativeByNotVariable)
        | Confirmation(x) ->
            let x = x.Calculate(definition)
            match x with
            | Infinity -> +Infinity
            | x -> x
        | Module(x) ->
            let x = x.Calculate(definition)
            match x with
            | Constant(a) -> Constant(abs(a))
            | x when BinaryFunction.op_True(ScalarFunction.(>=)(x, Constant(0.))) -> x
            | x when BinaryFunction.op_True(ScalarFunction.(<)(x, Constant(0.))) -> (-x).Calculate()
            | x -> Module(x)
        | Remainder(x, y) ->
            let x = x.Calculate(definition)
            let y = y.Calculate(definition)
            match (x, y) with
            | (x, y) when x = y -> Constant(0.)
            | (x, y) when x < y -> x
            | (Constant(a), Constant(b)) -> Constant(a % b)
            | (x, y) -> Remainder(x, y)
        | MatrixRowsCount(x) ->
            let x = x.Calculate(definition)
            match x with
            | Matrix(matrix0) -> Constant(double(matrix0.RowsCount))
            | x -> MatrixRowsCount(x)
        | MatrixColumnsCount(x) ->
            let x = x.Calculate(definition)
            match x with
            | Matrix(matrix0) -> Constant(double(matrix0.ColumnsCount))
            | x -> MatrixColumnsCount(x)
        | VectorElementsCount(x) ->
            let x = x.Calculate(definition)
            match x with
            | Vector(list0) | ColumnVector(list0) -> Constant(double(list0.Count()))
            | x -> VectorElementsCount(x)
        | VectorMultiply(x, y) ->
            let x = x.Calculate(definition)
            let y = y.Calculate(definition)
            match (x, y) with
            | (Vector(list0), Vector(list1)) -> list0.Zip(list1, fun x y -> x * y).Aggregate(fun x y -> x + y).Calculate()
            | (ColumnVector(list0), ColumnVector(list1)) -> list0.Zip(list1, fun x y -> x * y).Aggregate(fun x y -> x + y).Calculate()
            | (Vector(list0), ColumnVector(list1)) | (ColumnVector(list0), Vector(list1)) -> list0.Zip(list1, fun x y -> x * y).Aggregate(fun x y -> x + y).Calculate()
            | (x, y) -> VectorMultiply(x, y)
        | MatrixElementAt(x, y, z) ->
            let x = x.Calculate(definition)
            let y = y.Calculate(definition)
            let z = z.Calculate(definition)
            match (SetScalarIsIncluded(y, NaturalNumbersWithZero) &&& SetScalarIsIncluded(z, NaturalNumbersWithZero)).Calculate() with
            | True ->
                match x with
                | Matrix(matrix0) ->
                    match (y, z) with
                    | (Constant(a), Constant(b)) -> matrix0.[int(a), int(b)]
                    | (y, z) -> MatrixElementAt(x, y, z)
                | x -> MatrixElementAt(x, y, z)
            | False -> ComputationError(this, ComputationErrors.ArgumentIsNotFromNaturalNumberWithZero)
            | _ -> MatrixElementAt(x, y, z)
        | VectorElementAt(x, y) ->
            let x = x.Calculate(definition)
            let y = y.Calculate(definition)
            match SetScalarIsIncluded(y, NaturalNumbersWithZero).Calculate() with
            | True ->
                match x with
                | Vector(list0) | ColumnVector(list0) ->
                    match y with
                    | Constant(a) -> list0.ElementAt(int(a))
                    | y -> VectorElementAt(x, y)
                | x -> VectorElementAt(x, y)
            | False -> ComputationError(this, ComputationErrors.ArgumentIsNotFromNaturalNumberWithZero)
            | _ -> VectorElementAt(x, y)
        | Maximum(functions) ->
            match functions.Count() with
            | 0 -> Constant(0.)
            | 1 -> functions.ElementAt(0).Calculate(definition)
            | 2 ->
                let x = functions.ElementAt(0).Calculate(definition)
                let y = functions.ElementAt(1).Calculate(definition)
                match (ScalarGreaterThan(x, y) ||| ScalarGreaterThanOrEqual(x, y) ||| ScalarEqual(x, y)).Calculate() with
                | True -> x
                | _ ->
                    match ScalarLessThan(x, y) ||| ScalarLessThanOrEqual(x, y) with
                    | True -> y
                    | _ -> Maximum([x; y])
        | _ -> this

    override this.ToString() =
        let mutable result = StringBuilder()
        match this with
        | Constant _ | Variable _ | None _ | ImaginaryOne | Pi | Exponent -> result <- result.Append(this.Identificator.[0])
        | Addition(functions) | Multiply(functions) ->
            for i0 = 0 to functions.Count() - 1 do
                if functions.ElementAt(i0).Priority <= this.Priority then result <- result.Append('(')
                result <- result.Append(functions.ElementAt(i0))
                if functions.ElementAt(i0).Priority <= this.Priority then result <- result.Append(')')
                if i0 + 1 <> functions.Count() then result <- result.Append(this.Identificator.[0])
        | Subtraction(x, y) | Division(x, y) | Remainder(x, y) ->
            if x.Priority <= this.Priority then result <- result.Append('(')
            result <- result.Append(x)
            if x.Priority <= this.Priority then result <- result.Append(')')
            result <- result.Append(this.Identificator.[0])
            if y.Priority <= this.Priority then result <- result.Append('(')
            result <- result.Append(y)
            if y.Priority <= this.Priority then result <- result.Append(')')
        | Power(x, y) ->
            match y with
            | Constant(a) when a = 0.5 || a = 0.333333333333333 || a = 0.25 ->
                match a with
                | 0.5 -> result <- result.Append(this.Identificator.[1])
                | 0.333333333333333 -> result <- result.Append(this.Identificator.[2])
                | 0.25 -> result <- result.Append(this.Identificator.[3])
                | _ -> ()
                if x.Priority < this.Priority then result <- result.Append('(')
                result <- result.Append(x)
                if x.Priority < this.Priority then result <- result.Append(')')
            | _ ->
                if x.Priority < this.Priority then result <- result.Append('(')
                result <- result.Append(x)
                if x.Priority < this.Priority then result <- result.Append(')')
                result <- result.Append(this.Identificator.[0])
                if y.Priority < this.Priority then result <- result.Append('(')
                result <- result.Append(y)
                if y.Priority < this.Priority then result <- result.Append(')')
        | Negate(x) | Sin(x) | Cos(x) | Tg(x) | Ctg(x) | Ln(x) | Lg(x) | Arcsin(x) | Arccos(x) | Arctg(x) | Arcctg(x) | Sec(x) | Cosec(x) | Cot(x) ->
            result <- result.Append(this.Identificator.[0])
            if x.Priority < this.Priority then result <- result.Append('(')
            result <- result.Append(x)
            if x.Priority < this.Priority then result <- result.Append(')')
        | Module(x) | IntegerPart(x) | Floor(x) | Ceiling(x) ->
            result <- result.Append(this.Identificator.[0])
            result <- result.Append(x)
            result <- result.Append(this.Identificator.[0])
        | Devirative(x, y) ->
            if x.Priority < this.Priority then result <- result.Append('(')
            result <- result.Append(x)
            if x.Priority < this.Priority then result <- result.Append(')')
            result <- result.Append(this.Identificator.[0])
            result <- result.Append(y)
        | Log(x, y) ->
            result <- result.Append(this.Identificator.[0])
            result <- result.Append('(')
            result <- result.Append(x)
            result <- result.Append(',')
            result <- result.Append(y)
            result <- result.Append(')')
        | ComputationError(func, error) ->
            result <- result.Append("В выражении ")
            result <- result.Append(func)
            result <- result.Append(" произошла ошибка ")
            result <- result.Append(error)
        | _ ->  result <- result.Append("_ERROR_")
        result.ToString()

and [<CustomEquality>] [<NoComparison>] public VectorFunction =
    | Vector                 of X : IEnumerable<ScalarFunction>
    | ColumnVector           of X : IEnumerable<ScalarFunction>
    | Addition               of X : IEnumerable<VectorFunction>
    | BitwiseMultiply of X : VectorFunction * Y : VectorFunction
    | BitwiseDivision of X : VectorFunction * Y : VectorFunction
    | Subtraction            of X : VectorFunction * Y : VectorFunction
    | Concat                 of X : VectorFunction * Y : VectorFunction
    | ScalarMultiply         of X : VectorFunction * Y : ScalarFunction
    | ScalarDivision         of X : VectorFunction * Y : ScalarFunction
    | Complement             of X : VectorFunction * Y : ScalarFunction
    | MatrixLeftMultiply     of X : VectorFunction * Y : MatrixFunction
    | MatrixRightMultiply    of X : MatrixFunction * Y : VectorFunction
    | MatrixRowAt of X : MatrixFunction * Y : ScalarFunction
    | MatrixColumnAt of X : MatrixFunction * Y : ScalarFunction
    | Negate                 of X : VectorFunction
    | Transposition          of X : VectorFunction
    | Normalize              of X : VectorFunction
    | Sequence               of What : ScalarFunction * From : ScalarFunction * To : ScalarFunction * How : ScalarFunction * With : ScalarFunction
    | ComputationError       of Function : VectorFunction * Error : ComputationErrors
    | SetElementAt of Vector : VectorFunction * ElementIndex : ScalarFunction * Value : ScalarFunction
    | None

    override this.Equals(obj) =
        match obj with
        | :? VectorFunction as x ->
            match BinaryFunction.VectorEqual(this, x).Calculate() with
            | True -> true
            | _ -> false
        | _ -> false
    
    static member inline (~-)   (x : VectorFunction) : VectorFunction = Negate(x)
    static member inline (~~)   (x : VectorFunction) : VectorFunction = Transposition(x)
    static member inline (+)    (x : VectorFunction, y : VectorFunction) : VectorFunction = Addition([x; y])
    static member inline (-)    (x : VectorFunction, y : VectorFunction) : VectorFunction = Subtraction(x, y)
    static member inline (*)    (x : VectorFunction, y : VectorFunction) : MatrixFunction = MatrixFunction.VectorMultiply(x, y)
    static member inline (|*)   (x : VectorFunction, y : VectorFunction) : ScalarFunction = ScalarFunction.VectorMultiply(x, y)
    static member inline (*)    (x : VectorFunction, y : ScalarFunction) : VectorFunction = VectorFunction.ScalarMultiply(x, y)
    static member inline (*)    (x : ScalarFunction, y : VectorFunction) : VectorFunction = VectorFunction.ScalarMultiply(y, x)
    static member inline (*)    (x : VectorFunction, y : MatrixFunction) : VectorFunction = MatrixLeftMultiply(x, y)
    static member inline (*)    (x : MatrixFunction, y : VectorFunction) : VectorFunction = MatrixRightMultiply(x, y)
    static member inline (.*)    (x : VectorFunction, y : VectorFunction) : VectorFunction = BitwiseMultiply(x, y)
    static member inline ( ** ) (x : VectorFunction, y : ScalarFunction) : ScalarFunction = ScalarFunction.VectorPower(x, y)
    static member inline (/)    (x : VectorFunction, y : ScalarFunction) : VectorFunction = ScalarDivision(x, y)
    static member inline (./)    (x : VectorFunction, y : VectorFunction) : VectorFunction = BitwiseDivision(x, y)
    static member inline (@)    (x : VectorFunction, y : VectorFunction) : VectorFunction = Concat(x, y)
    static member inline (|=|)  (x : VectorFunction, y : VectorFunction) : bool = BinaryFunction.op_True(BinaryFunction.VectorStructEqual(x, y))
    static member inline (|<>|) (x : VectorFunction, y : VectorFunction) : bool = BinaryFunction.op_False(BinaryFunction.VectorStructNotEqual(x, y))
    static member inline (<>)   (x : VectorFunction, y : VectorFunction) : BinaryFunction = BinaryFunction.VectorNotEqual(x, y)
    static member inline (=)    (x : VectorFunction, y : VectorFunction) : BinaryFunction = BinaryFunction.VectorEqual(x, y)
    static member inline (>)    (x : VectorFunction, y : VectorFunction) : BinaryFunction = BinaryFunction.VectorGreaterThan(x, y)
    static member inline (<)    (x : VectorFunction, y : VectorFunction) : BinaryFunction = BinaryFunction.VectorLessThan(x, y)
    static member inline (>=)   (x : VectorFunction, y : VectorFunction) : BinaryFunction = BinaryFunction.VectorGreaterThanOrEqual(x, y)
    static member inline (<=)   (x : VectorFunction, y : VectorFunction) : BinaryFunction = BinaryFunction.VectorLessThanOrEqual(x, y)
    static member inline op_OnesComplement (x : VectorFunction) : VectorFunction = Transposition(x)

    member this.Item
        with get(x : ScalarFunction) : ScalarFunction = VectorElementAt(this, x)

    static member private _OptimizeList(elements : list<ScalarFunction>, definition : BinaryFunction) : list<ScalarFunction> =
        match elements with
        | head :: tail -> head.Calculate(definition) :: VectorFunction._OptimizeList(tail, definition)
        | [] -> []

    static member private _OptimizeFunction(list0 : list<ScalarFunction>, list1 : list<ScalarFunction>, func : ScalarFunction * ScalarFunction -> ScalarFunction) : list<ScalarFunction> =
        match (list0, list1) with
        | (head0 :: tail0, head1 :: tail1) -> func(head0, head1) :: VectorFunction._OptimizeFunction(tail0, tail1, func)
        | (head0 :: tail0, []) -> func(head0, Constant(0.)) :: VectorFunction._OptimizeFunction(tail0, [], func)
        | ([], head1 :: tail1) -> func(Constant(0.), head1) :: VectorFunction._OptimizeFunction([], tail1, func)
        | ([], []) -> []
    
    member this.Calculate() = this.Calculate(True)
    member this.Calculate(definition : BinaryFunction) : VectorFunction =
        match this with
        | Vector(elements) -> Vector(VectorFunction._OptimizeList(List.ofSeq(elements), definition))
        | ColumnVector(elements) -> ColumnVector(VectorFunction._OptimizeList(List.ofSeq(elements), definition))
        | Transposition(func) ->
            let func = func.Calculate(definition)
            match func with
            | Vector(elements) -> ColumnVector(elements)
            | ColumnVector(elements) -> Vector(elements)
            | x -> Transposition(x)
        | SetElementAt(x, y, z) ->
            let x = x.Calculate(definition)
            let y = y.Calculate(definition)
            let z = z.Calculate(definition)
            match y with
            | y when BinaryFunction.op_True(y |+| NaturalNumbersWithZero) && y < VectorElementsCount(x) ->
                match (x, y) with
                | (Vector(list0), Constant(a)) ->
                    let list0 = List(list0)
                    list0.[int(a)] <- z
                    Vector(list0)
                | (ColumnVector(list0), Constant(a)) ->
                    let list0 = List(list0)
                    list0.[int(a)] <- z
                    ColumnVector(list0)
                | (x, y) -> SetElementAt(x, y, z)
            | _ -> ComputationError(this, ComputationErrors.ArgumentIsNotFromNaturalNumberWithZero)
        | Addition(functions) ->
            match functions.Count() with
            | 0 -> None//I DON'T KNOW WHAT WE MUST RETURN
            | 1 -> functions.ElementAt(0).Calculate(definition)
            | 2 ->
                let fun0 = functions.ElementAt(0).Calculate(definition)
                let fun1 = functions.ElementAt(1).Calculate(definition)
                match (fun0, fun1) with
                | (Vector(elements0), Vector(elements1)) -> Vector(VectorFunction._OptimizeFunction(List.ofSeq(elements0), List.ofSeq(elements1), ScalarFunction.(+)))
                | (ColumnVector(elements0), ColumnVector(elements1)) -> ColumnVector(VectorFunction._OptimizeFunction(List.ofSeq(elements0), List.ofSeq(elements1), ScalarFunction.(+)))
                | _ -> fun0 + fun1
            | _ -> this
        | Subtraction(fun0, fun1) ->
            let fun0 = fun0.Calculate(definition)
            let fun1 = fun1.Calculate(definition)
            match (fun0, fun1) with
            | (Vector(elements0), Vector(elements1)) -> Vector(VectorFunction._OptimizeFunction(List.ofSeq(elements0), List.ofSeq(elements1), ScalarFunction.(-)))
            | (ColumnVector(elements0), ColumnVector(elements1)) -> ColumnVector(VectorFunction._OptimizeFunction(List.ofSeq(elements0), List.ofSeq(elements1), ScalarFunction.(-)))
            | _ -> fun0 - fun1
        | ScalarMultiply(x, y) ->
            let x = x.Calculate(definition)
            let y = y.Calculate(definition)
            match x with
            | Vector(list0) -> Vector(list0.Select(fun x -> (x * y).Calculate()))
            | ColumnVector(list0) -> ColumnVector(list0.Select(fun x -> (x * y).Calculate()))
            | Sequence(_what, _from, _to, _how, _with) -> Sequence((_what * y).Calculate(), _from, _to, _how, _with)
            | _ -> this
        | ScalarDivision(x, y) ->
            let x = x.Calculate(definition)
            let y = y.Calculate(definition)
            match x with
            | Vector(list0) -> Vector(list0.Select(fun x -> (x / y).Calculate()))
            | ColumnVector(list0) -> ColumnVector(list0.Select(fun x -> (x / y).Calculate()))
            | Sequence(_what, _from, _to, _how, _with) -> Sequence((_what / y).Calculate(), _from, _to, _how, _with)
            | _ -> this
        | MatrixRowAt(x, y) ->
            let x = x.Calculate(definition)
            let y = y.Calculate(definition)
            match SetScalarIsIncluded(y, NaturalNumbersWithZero).Calculate() with
            | True ->
                match x with
                | Matrix(matrix0) ->
                    match y with
                    | Constant(a) -> Vector(matrix0.RowAt(int(a)))
                    | y -> MatrixRowAt(x, y)
                | x -> MatrixRowAt(x, y)
            | False -> ComputationError(this, ComputationErrors.ArgumentIsNotFromNaturalNumberWithZero)
            | _ -> MatrixRowAt(x, y)
        | MatrixColumnAt(x, y) ->
            let x = x.Calculate(definition)
            let y = y.Calculate(definition)
            match SetScalarIsIncluded(y, NaturalNumbersWithZero).Calculate() with
            | True ->
                match x with
                | Matrix(matrix0) ->
                    match y with
                    | Constant(a) -> ColumnVector(matrix0.ColumnAt(int(a)))
                    | y -> MatrixColumnAt(x, y)
                | x -> MatrixColumnAt(x, y)
            | False -> ComputationError(this, ComputationErrors.ArgumentIsNotFromNaturalNumberWithZero)
            | _ -> MatrixColumnAt(x, y)
        | MatrixRightMultiply(x, y) ->
            let x = x.Calculate(definition)
            let y = y.Calculate(definition)
            match MatrixRowsCount(x) = VectorElementsCount(y) with
            | true ->
                match (x, y) with
                | (Matrix(matrix0), ColumnVector(list1)) ->
                    let mutable list2 = []
                    for i0 in 0 .. list1.Count() - 1 do list2 <- list2 @ (Vector(matrix0.RowAt(i0)) |* y).Calculate() :: []
                    ColumnVector(list2)
                | _ -> this
            | false -> ComputationError(this, ComputationErrors.VectorTypeMismatch)
        | Sequence(_what, _from, _to, _how, _with) ->
            match _with with
            | Variable _ ->
                let mutable iterator = _from.Calculate(definition)
                let mutable list = []
                let _to = _to.Calculate(definition)
                let _what = _what.Calculate(definition)
                let _how = _how.Calculate(definition)
                while (iterator <= _to) do
                    list <- list @ _what.Calculate(ScalarEqual(_with, iterator)) :: []
                    iterator <- _how.Calculate(ScalarEqual(_with, iterator))
                Vector(list)
            | _ -> ComputationError(this, ComputationErrors.VectorTypeMismatch)
        | x -> x

    member this.Copy() : VectorFunction =
        match this with
        | Vector(vector0) -> Vector(vector0.AsEnumerable())
        | ColumnVector(vector0) -> ColumnVector(vector0.AsEnumerable())
        | x -> x

    override this.ToString() =
        let mutable result = StringBuilder()
        match this with
        | Vector(elements) ->
            result <- result.Append('{')
            for i0 = 0 to elements.Count() - 1 do
                result <- result.Append(elements.ElementAt(i0))
                if i0 + 1 <> elements.Count() then result <- result.Append(';')
            result <- result.Append('}')
        | ColumnVector(elements) ->
            result <- result.Append("~{")
            for i0 = 0 to elements.Count() - 1 do
                result <- result.Append(elements.ElementAt(i0))
                if i0 + 1 <> elements.Count() then result <- result.Append(';')
            result <- result.Append('}')
        | None _ -> result <- result.Append("_NONE_")
        | ComputationError(func, error) ->
            result <- result.Append("В выражении ")
            result <- result.Append(func)
            result <- result.Append(" произошла ошибка ")
            result <- result.Append(error)
        | _ -> result <- result.Append("_ERROR_")
        result.ToString()

and [<CustomEquality>] [<NoComparison>] public MatrixFunction =
    | Matrix of IMatrix<ScalarFunction>
    | Addition of X : IEnumerable<MatrixFunction>
    | Subtraction of X : MatrixFunction * Y : MatrixFunction
    | ScalarMultiply of X : MatrixFunction * Y : ScalarFunction
    | ScalarDivision of X : MatrixFunction * Y : ScalarFunction
    | BitwiseMultiply of X : MatrixFunction * Y : MatrixFunction
    | BitwiseDivision of X : MatrixFunction * Y : MatrixFunction
    | Power of X : MatrixFunction * Y : ScalarFunction
    | Multiply of X : MatrixFunction * Y : MatrixFunction
    | VectorMultiply of X : VectorFunction * Y : VectorFunction
    | Transposition of X : MatrixFunction
    | Negate of X : MatrixFunction
    | SetRowAt of Matrix : MatrixFunction * RowIndex : ScalarFunction * Value : VectorFunction
    | SetColumnAt of Matrix : MatrixFunction * ColumnIndex : ScalarFunction * Value : VectorFunction
    | SetElementAt of Matrix : MatrixFunction * RowIndex : ScalarFunction * ColumnIndex : ScalarFunction * Value : ScalarFunction
    | ComputationError of Function : MatrixFunction * Error : ComputationErrors
    | None

    override this.Equals(obj) =
        match obj with
        | :? MatrixFunction as x ->
            match BinaryFunction.MatrixEqual(this, x).Calculate() with
            | True -> true
            | _ -> false
        | _ -> false
    
    static member (+) (x : MatrixFunction, y : MatrixFunction) : MatrixFunction = Addition([x; y])
    static member (-) (x : MatrixFunction, y : MatrixFunction) : MatrixFunction = Subtraction(x, y)
    static member (*) (x : MatrixFunction, y : ScalarFunction) : MatrixFunction = ScalarMultiply(x, y)
    static member (*) (x : ScalarFunction, y : MatrixFunction) : MatrixFunction = ScalarMultiply(y, x)
    static member (*) (x : MatrixFunction, y : MatrixFunction) : MatrixFunction = Multiply(x, y)
    static member (.*) (x : MatrixFunction, y : MatrixFunction) : MatrixFunction = BitwiseMultiply(x, y)
    static member (./) (x : MatrixFunction, y : MatrixFunction) : MatrixFunction = Multiply(x, y)
    static member ( ** ) (x : MatrixFunction, y : ScalarFunction) : MatrixFunction = Power(x, y)
    static member op_OnesComplement (x : MatrixFunction) : MatrixFunction = Transposition(x)
    static member (~~) (x : MatrixFunction) : MatrixFunction = Transposition(x)
    static member (~-) (x : MatrixFunction) : MatrixFunction = Negate(x)

    static member Pow (arg0 : MatrixFunction, arg1 : ScalarFunction) : MatrixFunction = Power(arg0, arg1)

    member this.Item
        with get(x : ScalarFunction, y : ScalarFunction) : ScalarFunction = MatrixElementAt(this, x, y)

    member this.Calculate() = this.Calculate(True)
    member public this.Calculate(definition : BinaryFunction) : MatrixFunction =
        match this with
        | Matrix(matrix0) -> Matrix(matrix0.Select(fun x -> x.Calculate(definition)).ToMatrix(matrix0.RowsCount, matrix0.ColumnsCount))
        | Addition(list0) ->
            match list0.Count() with
            | 0 -> None
            | 1 -> list0.ElementAt(0).Calculate(definition)
            | 2 ->
                let x = list0.ElementAt(0).Calculate(definition)
                let y = list0.ElementAt(1).Calculate(definition)
                match MatrixRowsCount(x) = MatrixRowsCount(y) && MatrixColumnsCount(x) = MatrixColumnsCount(y) with
                | true->
                    match (x, y) with
                    | (Matrix(matrix0), Matrix(matrix1)) -> Matrix(matrix0.Zip(matrix1, fun a b -> a + b).ToMatrix(matrix0.RowsCount, matrix0.ColumnsCount))
                    | (x, y) -> x + y
                | false -> ComputationError(this, ComputationErrors.VectorTypeMismatch)
            | _ -> this
        | SetRowAt(x, y, z) ->
            let x = x.Calculate(definition)
            let y = y.Calculate(definition)
            let z = z.Calculate(definition)
            match SetScalarIsIncluded(y, NaturalNumbersWithZero).Calculate() with
            | True ->
                match (x, y, z) with
                | (Matrix(matrix0), Constant(a), Vector(list0)) ->
                    let matrix0 = matrix0.ToMatrix(matrix0.RowsCount, matrix0.ColumnsCount)
                    matrix0.RowAt(int(a)) <- list0
                    Matrix(matrix0)
                | (x, y, z) -> SetRowAt(x, y, z)
            | False -> ComputationError(this, ComputationErrors.ArgumentIsNotFromNaturalNumberWithZero)
            | _ -> SetRowAt(x, y, z)
        | SetColumnAt(x, y, z) ->
            let x = x.Calculate(definition)
            let y = y.Calculate(definition)
            let z = z.Calculate(definition)
            match SetScalarIsIncluded(y, NaturalNumbersWithZero).Calculate() with
            | True ->
                match (x, y, z) with
                | (Matrix(matrix0), Constant(a), ColumnVector(list0)) ->
                    let matrix0 = matrix0.ToMatrix(matrix0.RowsCount, matrix0.ColumnsCount)
                    matrix0.ColumnAt(int(a)) <- list0
                    Matrix(matrix0)
                | (x, y, z) -> SetColumnAt(x, y, z)
            | False -> ComputationError(this, ComputationErrors.ArgumentIsNotFromNaturalNumberWithZero)
            | _ -> SetColumnAt(x, y, z)
        | SetElementAt(m, i, j, v) ->
            let m = m.Calculate(definition)
            let i = i.Calculate(definition)
            let j = j.Calculate(definition)
            let v = v.Calculate(definition)
            match (SetScalarIsIncluded(i, NaturalNumbersWithZero) &&& SetScalarIsIncluded(j, NaturalNumbersWithZero)).Calculate() with
            | True ->
                match (m, i, j) with
                | (Matrix(matrix0), Constant(a), Constant(b)) ->
                    let matrix0 = matrix0.ToMatrix(matrix0.RowsCount, matrix0.ColumnsCount)
                    matrix0.[int(a), int(b)] <- v
                    Matrix(matrix0)
                | (m, i, j) -> SetElementAt(m, i, j, v)
            | False -> ComputationError(this, ComputationErrors.ArgumentIsNotFromNaturalNumberWithZero)
            | _ -> SetElementAt(m, i, j, v)
        | x -> x

    member this.Copy() : MatrixFunction =
        match this with
        | Matrix(matrix0) -> Matrix(matrix0.ToMatrix(matrix0.RowsCount, matrix0.ColumnsCount))
        | x -> x

    override this.ToString() =
        let mutable result = StringBuilder()
        match this with
        | Matrix(matrix0) ->
            for i0 in 0 .. matrix0.RowsCount .. matrix0.RowsCount do
                for i1 in 0 .. matrix0.ColumnsCount .. matrix0.ColumnsCount do
                    result <- result.Append(matrix0.[i0, i1])
                result <- result.Append('\n')
        | x -> result <- result.Append("?")
        result.ToString()

and [<CustomEquality>] [<NoComparison>] public SetFunction =
    | None
    | ComputationError of X : SetFunction * Error : ComputationErrors
    | Empty
    | NaturalNumbers
    | NaturalNumbersWithZero
    | WholeNumbers
    | RationalNumbers
    | RealNumbers
    | ComplexNumbers
    | ScalarSet of X : IEnumerable<ScalarFunction>
    | VectorSet of X : IEnumerable<VectorFunction>
    | MatrixSet of X : IEnumerable<MatrixFunction>
    | BinarySet of X : IEnumerable<BinaryFunction>
    | DefinedSet of X : BinaryFunction
    | Intersection of X : IEnumerable<SetFunction>
    | Union of X : IEnumerable<SetFunction>
    | Difference of X : SetFunction * Y : SetFunction

    override this.Equals(obj) =
        match obj with
        | :? SetFunction as x ->
            match BinaryFunction.SetEqual(this, x).Calculate() with
            | True -> true
            | _ -> false
        | _ -> false

    static member (&&&) (x : SetFunction, y : SetFunction) : SetFunction = Intersection([x; y])
    static member (|||) (x : SetFunction, y : SetFunction) : SetFunction = Union([x; y])
    static member (-) (x : SetFunction, y : SetFunction) : SetFunction = Difference(x, y)
    static member (*) (x : SetFunction, y : SetFunction) : SetFunction = None
    static member (>) (x : SetFunction, y : SetFunction) : BinaryFunction = BinaryFunction.SetGreaterThan(x, y)
    static member (>=) (x : SetFunction, y : SetFunction) : BinaryFunction = BinaryFunction.SetGreaterThanOrEqual(x, y)
    static member (<) (x : SetFunction, y : SetFunction) : BinaryFunction = BinaryFunction.SetLessThan(x, y)
    static member (<=) (x : SetFunction, y : SetFunction) : BinaryFunction = BinaryFunction.SetLessThanOrEqual(x, y)
    static member (=) (x : SetFunction, y : SetFunction) : BinaryFunction = BinaryFunction.SetEqual(x, y)
    static member (<>) (x : SetFunction, y : SetFunction) : BinaryFunction = BinaryFunction.SetNotEqual(x, y)
    static member (|=|) (x : SetFunction, y : SetFunction) : bool = BinaryFunction.op_True(BinaryFunction.SetStructEqual(x, y))
    static member (|<>|) (x : SetFunction, y : SetFunction) : bool = BinaryFunction.op_False(BinaryFunction.SetStructNotEqual(x, y))
    static member (|+|) (x : ScalarFunction, y : SetFunction) : BinaryFunction = BinaryFunction.SetScalarIsIncluded(x, y)
    static member (|+|) (x : SetFunction, y : ScalarFunction) : BinaryFunction = BinaryFunction.SetScalarIsIncluded(y, x)
    static member (|-|) (x : ScalarFunction, y : SetFunction) : BinaryFunction = BinaryFunction.SetScalarIsNotIncluded(x, y)
    static member (|-|) (x : SetFunction, y : ScalarFunction) : BinaryFunction = BinaryFunction.SetScalarIsNotIncluded(y, x)
    static member (|+|) (x : VectorFunction, y : SetFunction) : BinaryFunction = BinaryFunction.SetVectorIsIncluded(x, y)
    static member (|+|) (x : SetFunction, y : VectorFunction) : BinaryFunction = BinaryFunction.SetVectorIsIncluded(y, x)
    static member (|-|) (x : VectorFunction, y : SetFunction) : BinaryFunction = BinaryFunction.SetVectorIsNotIncluded(x, y)
    static member (|-|) (x : SetFunction, y : VectorFunction) : BinaryFunction = BinaryFunction.SetVectorIsNotIncluded(y, x)
    static member (|+|) (x : MatrixFunction, y : SetFunction) : BinaryFunction = BinaryFunction.SetMatrixIsIncluded(x, y)
    static member (|+|) (x : SetFunction, y : MatrixFunction) : BinaryFunction = BinaryFunction.SetMatrixIsIncluded(y, x)
    static member (|-|) (x : MatrixFunction, y : SetFunction) : BinaryFunction = BinaryFunction.SetMatrixIsNotIncluded(x, y)
    static member (|-|) (x : SetFunction, y : MatrixFunction) : BinaryFunction = BinaryFunction.SetMatrixIsNotIncluded(y, x)
    static member (|+|) (x : BinaryFunction, y : SetFunction) : BinaryFunction = BinaryFunction.SetBinaryIsIncluded(x, y)
    static member (|+|) (x : SetFunction, y : BinaryFunction) : BinaryFunction = BinaryFunction.SetBinaryIsIncluded(y, x)
    static member (|-|) (x : BinaryFunction, y : SetFunction) : BinaryFunction = BinaryFunction.SetBinaryIsNotIncluded(x, y)
    static member (|-|) (x : SetFunction, y : BinaryFunction) : BinaryFunction = BinaryFunction.SetBinaryIsNotIncluded(y, x)
    static member (|+|) (x : SetFunction, y : SetFunction) : BinaryFunction = BinaryFunction.SetSetIsIncluded(x, y)
    static member (|-|) (x : SetFunction, y : SetFunction) : BinaryFunction = BinaryFunction.SetSetIsNotIncluded(x, y)

    member this.Calculate() = this.Calculate(True)
    member this.Calculate(?definition : BinaryFunction) : SetFunction =
        let definition = defaultArg definition True
        match this with
        | Union(list0) ->
            match list0.Count() with
            | 0 -> Empty
            | 1 -> list0.ElementAt(0).Calculate(definition)
            | 2 ->
                let x = list0.ElementAt(0).Calculate(definition)
                let y = list0.ElementAt(1).Calculate(definition)
                match (x, y) with
                | (x, y) when x = y -> x
                | (Empty, x) | (x, Empty) -> x
                | (ComplexNumbers, x) | (x, ComplexNumbers) ->
                    match x with
                    | RealNumbers | RationalNumbers | WholeNumbers | NaturalNumbersWithZero | NaturalNumbers -> ComplexNumbers
                    | x -> ComplexNumbers ||| x
                | (RealNumbers, x) | (x, RealNumbers) ->
                    match x with
                    | RationalNumbers | WholeNumbers | NaturalNumbersWithZero | NaturalNumbers -> RealNumbers
                    | x -> RealNumbers ||| x
                | (RationalNumbers, x) | (x, RationalNumbers) ->
                    match x with
                    | WholeNumbers | NaturalNumbersWithZero | NaturalNumbers -> RationalNumbers
                    | x -> RationalNumbers ||| x
                | (WholeNumbers, x) | (x, WholeNumbers) ->
                    match x with
                    | NaturalNumbersWithZero | NaturalNumbers -> WholeNumbers
                    | x -> WholeNumbers ||| x
                | (NaturalNumbersWithZero, x) | (x, NaturalNumbersWithZero) ->
                    match x with
                    | NaturalNumbers -> NaturalNumbersWithZero
                    | x -> NaturalNumbersWithZero ||| x
                | (x, y) -> x ||| y
            | _ -> this
        | _ -> this

    member this.Identificator
        with get() : string[] =
            match this with
            | Empty -> [|"∅"|]
            | NaturalNumbers -> [|"ℕ"|]
            | NaturalNumbersWithZero -> [|"ℕ ∪ {0}"|]
            | WholeNumbers -> [|"ℤ"|]
            | RationalNumbers -> [|"ℚ"|]
            | RealNumbers -> [|"ℝ"|]
            | ComplexNumbers -> [|"ℂ"|]
            | Union _ -> [|"∪"|]
            | Intersection _ -> [|"⋂"|]
            | Difference _ -> [|"\\"|]
            | _ -> [|"?"|]
    
    override this.ToString() =
        let mutable result = StringBuilder()
        match this with
        | Empty | NaturalNumbers | NaturalNumbersWithZero | WholeNumbers | RationalNumbers | RealNumbers | ComplexNumbers ->
            result <- result.Append(this.Identificator.[0])
        | x -> result <- result.Append("?")
        result.ToString()

and [<CustomEquality>] [<NoComparison>] public BinaryFunction = 
    | And of X : IEnumerable<BinaryFunction>
    | Or of X : IEnumerable<BinaryFunction>
    | XOr of X : IEnumerable<BinaryFunction>
    | Not of X : BinaryFunction
    | Equal of X : BinaryFunction * Y : BinaryFunction
    | NotEqual of X : BinaryFunction * Y : BinaryFunction
    | Implication of X : BinaryFunction * Y : BinaryFunction
    | Identity of X : BinaryFunction * Y : BinaryFunction
    | ScalarGreaterThan of X : ScalarFunction * Y : ScalarFunction
    | ScalarLessThan of X : ScalarFunction * Y : ScalarFunction
    | ScalarGreaterThanOrEqual of X : ScalarFunction * Y : ScalarFunction
    | ScalarLessThanOrEqual of X : ScalarFunction * Y : ScalarFunction
    | ScalarEqual of X : ScalarFunction * Y : ScalarFunction
    | ScalarNotEqual of X : ScalarFunction * Y : ScalarFunction
    | ScalarStructEqual of X : ScalarFunction * Y : ScalarFunction
    | ScalarStructNotEqual of X : ScalarFunction * Y : ScalarFunction
    | ScalarAny of Function : ScalarFunction * Condition : BinaryFunction
    | ScalarExist of Function : ScalarFunction * Condition : BinaryFunction
    | ScalarFind of Function : ScalarFunction * Condition : BinaryFunction
    | VectorGreaterThan of X : VectorFunction * Y : VectorFunction
    | VectorLessThan of X : VectorFunction * Y : VectorFunction
    | VectorGreaterThanOrEqual of X : VectorFunction * Y : VectorFunction
    | VectorLessThanOrEqual of X : VectorFunction * Y : VectorFunction
    | VectorEqual of X : VectorFunction * Y : VectorFunction
    | VectorNotEqual of X : VectorFunction * Y : VectorFunction
    | VectorStructEqual of X : VectorFunction * Y : VectorFunction
    | VectorStructNotEqual of X : VectorFunction * Y : VectorFunction
    | VectorAny of Function : VectorFunction * Condition : BinaryFunction
    | VectorExist of Function : VectorFunction * Condition : BinaryFunction
    | VectorFind of Function : VectorFunction * Condition : BinaryFunction
    | MatrixGreaterThan of X : MatrixFunction * Y : MatrixFunction
    | MatrixLessThan of X : MatrixFunction * Y : MatrixFunction
    | MatrixGreaterThanOrEqual of X : MatrixFunction * Y : MatrixFunction
    | MatrixLessThanOrEqual of X : MatrixFunction * Y : MatrixFunction
    | MatrixEqual of X : MatrixFunction * Y : MatrixFunction
    | MatrixNotEqual of X : MatrixFunction * Y : MatrixFunction
    | MatrixStructEqual of X : MatrixFunction * Y : MatrixFunction
    | MatrixStructNotEqual of X : MatrixFunction * Y : MatrixFunction
    | MatrixAny of Function : MatrixFunction * Condition : BinaryFunction
    | MatrixExist of Function : MatrixFunction * Condition : BinaryFunction
    | MatrixFind of Function : MatrixFunction * Condition : BinaryFunction
    | SetGreaterThan of X : SetFunction * Y : SetFunction
    | SetLessThan of X : SetFunction * Y : SetFunction
    | SetGreaterThanOrEqual of X : SetFunction * Y : SetFunction
    | SetLessThanOrEqual of X : SetFunction * Y : SetFunction
    | SetEqual of X : SetFunction * Y : SetFunction
    | SetNotEqual of X : SetFunction * Y : SetFunction
    | SetStructEqual of X : SetFunction * Y : SetFunction
    | SetStructNotEqual of X : SetFunction * Y : SetFunction
    | SetAny of Function : SetFunction * Condition : BinaryFunction
    | SetExist of Function : SetFunction * Condition : BinaryFunction
    | SetFind of X : SetFunction * Y : BinaryFunction
    | SetScalarIsIncluded of X : ScalarFunction * Y : SetFunction
    | SetVectorIsIncluded of X : VectorFunction * Y : SetFunction
    | SetMatrixIsIncluded of X : MatrixFunction * Y : SetFunction
    | SetBinaryIsIncluded of X : BinaryFunction * Y : SetFunction
    | SetSetIsIncluded of X : SetFunction * Y : SetFunction
    | SetScalarIsNotIncluded of X : ScalarFunction * Y : SetFunction
    | SetVectorIsNotIncluded of X : VectorFunction * Y : SetFunction
    | SetMatrixIsNotIncluded of X : MatrixFunction * Y : SetFunction
    | SetBinaryIsNotIncluded of X : BinaryFunction * Y : SetFunction
    | SetSetIsNotIncluded of X : SetFunction * Y : SetFunction
    | VectorIsColumn of Vector : VectorFunction
    | VectorIsRow of Vector : VectorFunction
    | True
    | False
    | ComputationError of Function : BinaryFunction * Error : ComputationErrors
    | None

    override this.Equals(obj) =
        match obj with
        | :? BinaryFunction as x ->
            BinaryFunction.op_True(Equal(this, x).Calculate())
        | _ ->
            false

    static member op_Implicit(value : bool) : BinaryFunction =
        match value with
        | true -> True
        | false -> False

    static member op_Implicit(func : BinaryFunction) : Nullable<bool> =
        let func = func.Calculate()
        match func with
        | True -> Nullable(true)
        | False -> Nullable(false)
        | _ -> Nullable()

    static member op_True(x : BinaryFunction) : bool =
        let x = x.Calculate()
        match x with
        | True -> true
        | _ -> false

    static member op_False(x : BinaryFunction) : bool =
        let x = x.Calculate()
        match x with
        | False -> true
        | _ -> false

    static member (~~~) (x : BinaryFunction) : BinaryFunction = Not(x)
    static member (&&&) (x : BinaryFunction, y : BinaryFunction) : BinaryFunction = And([x; y])
    static member (|||) (x : BinaryFunction, y : BinaryFunction) : BinaryFunction = Or([x; y])
    static member (=>) (x : BinaryFunction, y : BinaryFunction) : BinaryFunction = Implication(x, y)
    static member (<=>) (x : BinaryFunction, y : BinaryFunction) : BinaryFunction = Identity(x, y)
    static member (=) (x : BinaryFunction, y : BinaryFunction) : BinaryFunction = Equal(x, y)
    static member (<>) (x : BinaryFunction, y : BinaryFunction) : BinaryFunction = NotEqual(x, y)
    static member op_Identity (x : BinaryFunction, y : BinaryFunction) : BinaryFunction = Identity(x, y)
    static member op_Implication (x : BinaryFunction, y : BinaryFunction) : BinaryFunction = Implication(x, y)

    member this.Contains(func : ScalarFunction) : bool =
        let x = this.Calculate()
        match x with
        | And(list0) | Or(list0) | XOr(list0) ->
            list0.Aggregate(false, fun x y -> x || y.Contains(func))
        | Not(x) ->
            x.Contains(func)
        | ScalarAny(x, y) | ScalarExist(x, y) | ScalarFind(x, y) ->
            x.Contains(func) || y.Contains(func)
        | ScalarGreaterThan(x, y) | ScalarGreaterThanOrEqual(x, y) | ScalarLessThan(x, y) | ScalarLessThanOrEqual(x, y) | ScalarEqual(x, y) | ScalarNotEqual(x, y) ->
            x.Contains(func) || y.Contains(func)
        | x -> false

    member this.Calculate() = this.Calculate(True)
    member this.Calculate(definition : BinaryFunction) : BinaryFunction =
        match this with
        | Equal(x, y) ->
            let x = x.Calculate(definition)
            let y = y.Calculate(definition)
            match (x, y) with
            | (True, True) | (False, False) ->
                True
            | (False, True) | (True, False) ->
                False
            | (Equal(x0, y0), Equal(x1, y1)) ->
                (Equal(x0, x1) &&& Equal(y0, y1)).Calculate()
            | (ScalarEqual(x0, y0), ScalarEqual(x1, y1)) ->
                ((ScalarEqual(x0, x1) &&& ScalarEqual(y0, y1)) ||| (ScalarEqual(x0, y1) &&& ScalarEqual(x1, y0))).Calculate()
            | (x, y) -> Equal(x, y)
        | NotEqual(x, y) ->
            (~~~Equal(x, y)).Calculate(definition)
        | ScalarGreaterThan(x, y) ->
            let x = x.Calculate(definition)
            let y = y.Calculate(definition)
            match (x, y) with
            | (Constant(a), Constant(b)) -> BinaryFunction.op_Implicit(a > b)
            | (Constant(a), Pi) -> BinaryFunction.op_Implicit(a > Math.PI)
            | (Pi, Constant(a)) -> BinaryFunction.op_Implicit(Math.PI > a)
            | (Constant(a), Exponent) -> BinaryFunction.op_Implicit(a > Math.E)
            | (Exponent, Constant(a)) -> BinaryFunction.op_Implicit(Math.E > a)
            | _ -> ScalarGreaterThan(x, y)
        | ScalarGreaterThanOrEqual(x, y) ->
            let x = x.Calculate(definition)
            let y = y.Calculate(definition)
            match (x, y) with
            | (Constant(a), Constant(b)) -> BinaryFunction.op_Implicit(a >= b)
            | (ScalarFunction.Module _, Constant(b)) when b = 0. -> True
            | (Constant(a), Pi) -> BinaryFunction.op_Implicit(a >= Math.PI)
            | (Pi, Constant(a)) -> BinaryFunction.op_Implicit(Math.PI >= a)
            | (Constant(a), Exponent) -> BinaryFunction.op_Implicit(a >= Math.E)
            | (Exponent, Constant(a)) -> BinaryFunction.op_Implicit(Math.E >= a)
            | _ -> ScalarGreaterThanOrEqual(x, y)
        | ScalarLessThan(x, y) ->
            let x = x.Calculate(definition)
            let y = y.Calculate(definition)
            match (x, y) with
            | (Constant(a), Constant(b)) -> BinaryFunction.op_Implicit(a < b)
            | (Constant(a), Pi) -> BinaryFunction.op_Implicit(a < Math.PI)
            | (Pi, Constant(a)) -> BinaryFunction.op_Implicit(Math.PI < a)
            | (Constant(a), Exponent) -> BinaryFunction.op_Implicit(a < Math.E)
            | (Exponent, Constant(a)) -> BinaryFunction.op_Implicit(Math.E < a)
            | _ -> ScalarLessThan(x, y)
        | ScalarLessThanOrEqual(x, y) ->
            let x = x.Calculate(definition)
            let y = y.Calculate(definition)
            match (x, y) with
            | (Constant(a), Constant(b)) -> BinaryFunction.op_Implicit(a <= b)
            | (Constant(a), ScalarFunction.Module _) when a = 0. -> True
            | (Constant(a), Pi) -> BinaryFunction.op_Implicit(a <= Math.PI)
            | (Pi, Constant(a)) -> BinaryFunction.op_Implicit(Math.PI <= a)
            | (Constant(a), Exponent) -> BinaryFunction.op_Implicit(a <= Math.E)
            | (Exponent, Constant(a)) -> BinaryFunction.op_Implicit(Math.E <= a)
            | _ -> ScalarLessThanOrEqual(x, y)
        | ScalarEqual(x, y) ->
            let x = x.Calculate(definition)
            let y = y.Calculate(definition)
            match (x, y) with
            | (ScalarFunction.Constant(a), ScalarFunction.Constant(b)) ->
                BinaryFunction.op_Implicit((a = b))
            | (ScalarFunction.Variable(name0), ScalarFunction.Variable(name1)) ->
                match name0 = name1 with
                | true ->
                    True
                | false ->
                    ScalarEqual(x, y)
            | (ScalarFunction.Addition(list0), ScalarFunction.Addition(list1)) | (ScalarFunction.Multiply(list0), ScalarFunction.Multiply(list1)) ->
                match And(list0.Select(fun x -> Or(list1.Select(fun y -> ScalarEqual(x, y))))).Calculate() with
                | True -> True
                | False -> False
                | _ -> ScalarEqual(x, y)
            | (ScalarFunction.Subtraction(fun00, fun01), ScalarFunction.Subtraction(fun10, fun11)) | (ScalarFunction.Division(fun00, fun01), ScalarFunction.Division(fun10, fun11)) | (ScalarFunction.Power(fun00, fun01), ScalarFunction.Power(fun10, fun11)) | (ScalarFunction.Log(fun00, fun01), ScalarFunction.Log(fun10, fun11)) ->
                (ScalarEqual(fun00, fun10) &&& ScalarEqual(fun01, fun11)).Calculate()
            | (ScalarFunction.Ln(fun0), ScalarFunction.Ln(fun1)) | (ScalarFunction.Lg(fun0), ScalarFunction.Lg(fun1)) | (ScalarFunction.Sin(fun0), ScalarFunction.Sin(fun1)) | (ScalarFunction.Cos(fun0), ScalarFunction.Cos(fun1)) | (ScalarFunction.Negate(fun0), ScalarFunction.Negate(fun1)) ->
                ScalarEqual(fun0, fun1).Calculate()
            | (ScalarFunction.Ln(fun0), ScalarFunction.Log(fun10, fun11)) | (ScalarFunction.Log(fun10, fun11), ScalarFunction.Lg(fun0)) ->
                (ScalarEqual(fun10, Exponent) &&& ScalarEqual(fun0, fun11)).Calculate()
            | (ScalarFunction.Lg(fun0), ScalarFunction.Log(fun10, fun11)) | (ScalarFunction.Log(fun10, fun11), ScalarFunction.Lg(fun0)) ->
                (ScalarEqual(fun10, Constant(10.)) &&& ScalarEqual(fun0, fun11)).Calculate()
            | (ScalarFunction.Pi, ScalarFunction.Pi) ->
                True
            | (ScalarFunction.Pi, ScalarFunction.Pi) | (ScalarFunction.Exponent, ScalarFunction.Exponent) | (ScalarFunction.ImaginaryOne, ScalarFunction.ImaginaryOne) | (ScalarFunction.None, ScalarFunction.None) ->
                True
            | (IntegerPart(x), y) | (y, IntegerPart(x)) when x = y ->
                (x |+| WholeNumbers).Calculate()
            | (Module(x), y) | (y, Module(x)) when x = y ->
                ScalarGreaterThanOrEqual(x, Constant(0.)).Calculate()
            | _ ->
                ScalarEqual(x, y)
        | ScalarStructEqual(x, y) ->
            match (x, y) with
            | (ScalarFunction.Constant(a), ScalarFunction.Constant(b)) ->
                BinaryFunction.op_Implicit((a = b))
            | (ScalarFunction.Variable(name0), ScalarFunction.Variable(name1)) ->
                BinaryFunction.op_Implicit((name0 = name1))
            | (ScalarFunction.Addition(list0), ScalarFunction.Addition(list1)) | (ScalarFunction.Multiply(list0), ScalarFunction.Multiply(list1)) ->
                BinaryFunction.op_Implicit(list0.SetEqual(list1))
            | (ScalarFunction.Subtraction(fun00, fun01), ScalarFunction.Subtraction(fun10, fun11)) | (ScalarFunction.Division(fun00, fun01), ScalarFunction.Division(fun10, fun11)) | (ScalarFunction.Power(fun00, fun01), ScalarFunction.Power(fun10, fun11)) | (ScalarFunction.Log(fun00, fun01), ScalarFunction.Log(fun10, fun11)) ->
                BinaryFunction.op_Implicit(fun00 = fun10 && fun01 = fun11)
            | (ScalarFunction.Ln(fun0), ScalarFunction.Ln(fun1)) | (ScalarFunction.Lg(fun0), ScalarFunction.Lg(fun1)) | (ScalarFunction.Sin(fun0), ScalarFunction.Sin(fun1)) | (ScalarFunction.Cos(fun0), ScalarFunction.Cos(fun1)) | (ScalarFunction.Negate(fun0), ScalarFunction.Negate(fun1)) ->
                BinaryFunction.op_Implicit((fun0 = fun1))
            | (ScalarFunction.Ln(fun0), ScalarFunction.Log(fun10, fun11)) | (ScalarFunction.Log(fun10, fun11), ScalarFunction.Lg(fun0)) ->
                BinaryFunction.op_Implicit(fun10 = Exponent && fun0 = fun11)
            | (ScalarFunction.Lg(fun0), ScalarFunction.Log(fun10, fun11)) | (ScalarFunction.Log(fun10, fun11), ScalarFunction.Lg(fun0)) ->
                BinaryFunction.op_Implicit(fun10 = Pi && fun0 = fun11)
            | (ScalarFunction.Pi, ScalarFunction.Pi) | (ScalarFunction.Exponent, ScalarFunction.Exponent) | (ScalarFunction.ImaginaryOne, ScalarFunction.ImaginaryOne) | (ScalarFunction.None, ScalarFunction.None) ->
                True
            | _ -> False
        | ScalarNotEqual(x, y) -> (~~~ScalarEqual(x, y)).Calculate(definition)
        | ScalarStructNotEqual(x, y) -> (~~~ScalarStructEqual(x, y)).Calculate(definition)
        | VectorEqual(x, y) ->
            let x = x.Calculate(definition)
            let y = y.Calculate(definition)
            match (x, y) with
            | (Vector(list0), Vector(list1)) ->
                match list0.SequenceEqual(list1) with
                | true -> True
                | false -> VectorEqual(x, y)
            | (ColumnVector(list0), ColumnVector(list1)) ->
                match list0.SequenceEqual(list1) with
                | true -> True
                | false -> VectorEqual(x, y)
            | (x, y) -> VectorEqual(x, y)
        | Not(x) ->
            let x = x.Calculate(definition)
            match x with
            | True -> False
            | False -> True
            | _ -> ~~~x
        | And(functions) ->
            match functions.Count() with
            | 0 -> True
            | 1 -> functions.ElementAt(0).Calculate(definition)
            | 2 ->
                let x = functions.ElementAt(0).Calculate(definition)
                let y = functions.ElementAt(1).Calculate(definition)
                match (x, y) with
                | (False, _) | (_, False) -> False
                | (True, True) -> True
                | (x, True) | (True, x) -> x
                | (ScalarAny(x0, y0), ScalarAny(x1, y1)) ->
                    match (x0, y0, x1, y1) with
                    | (x0, y0, x1, y1) when x0 = x1 -> ScalarAny(x0, y0 &&& y1).Calculate()
                    | (x0, y0, x1, y1) -> ScalarAny(x0, y0) &&& ScalarAny(x1, y1)
                | (ScalarAny(x0, y0), ScalarEqual(x1, y1)) | (ScalarEqual(x1, y1), ScalarAny(x0, y0)) ->
                    match (x0, y0, x1, y1) with
                    | (x0, y0, x1, y1) when x0 = x1 ->
                        match (y0, y1) with
                        | (True, x) -> ScalarEqual(x0, x).Calculate()
                        | (y0, y1) -> (ScalarAny(x0, y0) &&& ScalarEqual(x1, y1)).Calculate()
                    | (x0, y0, x1, y1) -> (ScalarAny(x0, y0) &&& ScalarEqual(x1, y1)).Calculate()
                | (ScalarLessThan(x0, y0), ScalarGreaterThan(x1, y1)) | (ScalarGreaterThan(x1, y1), ScalarLessThan(x0, y0)) ->
                    match (x0, y0, x1, y1) with
                    | (x0, y0, x1, y1) when x0 = x1 && y0 = y1 -> ScalarNotEqual(x0, y0).Calculate()
                    | (x0, y0, x1, y1) -> ScalarLessThan(x0, y0) &&& ScalarGreaterThan(x1, y1)
                | (ScalarGreaterThan(x0, y0), ScalarGreaterThanOrEqual(x1, y1)) | (ScalarGreaterThanOrEqual(x1, y1), ScalarGreaterThan(x0, y0)) ->
                    match (x0, y0, x1, y1) with
                    | (x0, y0, x1, y1) when x0 = x1 ->
                        match (y0, y1) with
                        | (y0, y1) when y0 > y1 || y0 = y1 || y0 >= y1 -> ScalarGreaterThan(x0, y0).Calculate()
                        | (y0, y1) when y0 <= y1 -> ScalarGreaterThan(x0, y1).Calculate()
                        | (y0, y1) when y0 < y1 -> ScalarGreaterThanOrEqual(x0, y1).Calculate()
                        | (y0, y1) -> ScalarGreaterThan(x0, y0) &&& ScalarGreaterThanOrEqual(x1, y1)
                    | (x0, y0, x1, y1) -> ScalarGreaterThan(x0, y0) &&& ScalarGreaterThanOrEqual(x1, y1)
                | (ScalarLessThan(x0, y0), ScalarLessThanOrEqual(x1, y1)) | (ScalarLessThanOrEqual(x1, y1), ScalarLessThan(x0, y0)) ->
                    match (x0, y0, x1, y1) with
                    | (x0, y0, x1, y1) when x0 = x1 ->
                        match (y0, y1) with
                        | (y0, y1) when y0 < y1 || y0 = y1 || y0 <= y1 -> ScalarGreaterThan(x0, y0).Calculate()
                        | (y0, y1) when y0 >= y1 -> ScalarGreaterThan(x0, y1).Calculate()
                        | (y0, y1) when y0 > y1 -> ScalarGreaterThanOrEqual(x0, y1).Calculate()
                        | (y0, y1) -> ScalarLessThan(x0, y0) &&& ScalarLessThanOrEqual(x1, y1)
                    | (x0, y0, x1, y1) -> ScalarLessThan(x0, y0) &&& ScalarLessThanOrEqual(x1, y1)
                | _ -> x &&& y
            | _ -> this
        | Or(functions) ->
            match functions.Count() with
            | 0 -> True
            | 1 -> functions.ElementAt(0).Calculate(definition)
            | 2 ->
                let x = functions.ElementAt(0).Calculate(definition)
                let y = functions.ElementAt(1).Calculate(definition)
                match (x, y) with
                | (True, _) | (_, True) -> True
                | (False, False) -> False
                | (x, False) | (False, x) -> x
                | _ -> x ||| y
            | _ -> this
        | VectorFind(x, y) ->
            let x = x.Calculate(definition)
            let y = y.Calculate(definition)
            match y with
            | VectorEqual(a, b) ->
                match (x, a, b) with
                | (Vector(list0), Vector(list1), Vector(list2)) | (ColumnVector(list0), ColumnVector(list1), ColumnVector(list2)) ->
                    let rec func(x, y, z, definition) =
                        match (x, y, z) with
                        | (x :: x_tail, y :: y_tail, z :: z_tail) -> ScalarFind(x, ScalarEqual(y, z)).Calculate(func(x_tail, y_tail, z_tail, definition))
                        | ([], [], []) -> definition
                        | _ -> ComputationError(this, ComputationErrors.InternalError)
                    func(List.ofSeq(list0), List.ofSeq(list1), List.ofSeq(list2), True)
                | _ -> VectorFind(x, y)
            | y -> VectorFind(x, y)
        | ScalarFind(x, fun1) ->
            let x =
                match x with
                | Variable _ -> x
                | x -> x.Calculate(definition)
            let fun1 = fun1.Calculate(definition)
            match x with
            | ScalarFunction.Variable(name0) ->
                match fun1 with
                | ScalarFind(y, z) ->
                    match x = y with
                    | true -> ScalarFind(x, z)
                    | false -> ScalarAny(x, True)
                | And(list0) ->
                    match list0.Count() with
                    | 0 | 1 -> ComputationError(this, ComputationErrors.InternalError)
                    | _ -> And(list0.Select(fun z -> ScalarFind(x, z))).Calculate()
                | ScalarEqual(fun10, fun11) ->
                    match (fun10, fun11) with
                    | (fun10, fun11) | (fun11, fun10) when not(fun10.Contains(x)) && not(fun11.Contains(x)) -> ScalarAny(x, True)
                    | (fun10, fun11) | (fun11, fun10) when fun10.Contains(x) && not(fun11.Contains(x)) ->
                        match fun10 with
                        | ScalarFunction.Variable(name1) ->
                            match name0 = name1 with
                            | true -> ScalarEqual(fun10, fun11)
                            | false -> ScalarAny(fun10, True)
                        | ScalarFunction.Power(fun100, fun101) ->
                            match (fun100, fun101) with
                            | (fun100, fun101) when fun100.Contains(x) && not(fun101.Contains(x)) -> ScalarFind(x, ScalarEqual(fun100, fun11 ** (Constant(1.) / fun101))).Calculate()
                            | _ -> this
                        | ScalarFunction.Multiply(list0) ->
                            let mutable y = fun10
                            let mutable z = fun11
                            for a in list0 do
                                match a.Contains(x) with
                                | false ->
                                    y <- y / a
                                    z <- z / a
                                | true -> ()
                            y <- y.Calculate()
                            z <- z.Calculate()
                            ScalarFind(x, ScalarEqual(y, z)).Calculate()
                        | ScalarFunction.Addition(list0) ->
                            let mutable y = fun10
                            let mutable z = fun11
                            for a in list0 do
                                match a.Contains(x) with
                                | false ->
                                    y <- y - a
                                    z <- z - a
                                | true -> ()
                            y <- y.Calculate()
                            z <- z.Calculate()
                            ScalarFind(x, ScalarEqual(y, z)).Calculate()
                        | ScalarFunction.Negate(x) -> ScalarFind(x, ScalarEqual(x, -fun11)).Calculate()
                        | ScalarFunction.Subtraction(y, z) ->
                            match (y, z) with
                            | (y, z) when x.Contains(x) && not(y.Contains(x)) -> ScalarFind(x, ScalarEqual(y, fun11 + z)).Calculate()
                            | (y, z) when not(x.Contains(x)) && y.Contains(x) -> ScalarFind(x, ScalarEqual(z, y - fun11)).Calculate()
                            | _ -> this
                        | y -> ScalarFind(x, ScalarEqual(y, fun11))
                    | (y, z) -> ScalarFind(x, ScalarEqual(y, z))
                | True -> ScalarAny(x, True)
                | False -> ScalarAny(x, False)
                | _ -> ScalarFind(x, fun1)
            | _ -> this
        | SetScalarIsIncluded(x, y) ->
            let x = x.Calculate(definition)
            let y = y.Calculate(definition)
            match y with
            | Empty -> False
            | NaturalNumbers ->
                match x with
                | Module(x) -> (ScalarNotEqual(x, Constant(0.)) &&& ScalarEqual(IntegerPart(x), x)).Calculate()
                | Constant(value) ->
                    match value with
                    | x when x > 0. && x = truncate(x) -> True
                    | x -> False
                | x -> False
            | NaturalNumbersWithZero ->
                match x with
                | x when BinaryFunction.op_True(x |+| NaturalNumbers) -> True
                | Constant(value) ->
                    match value with
                    | x when x = 0. -> True
                    | x -> False
                | x -> False
            | WholeNumbers ->
                match x with
                | x when BinaryFunction.op_True(Module(x) |+| NaturalNumbersWithZero) -> True
                | IntegerPart _ -> True
                | _ -> SetScalarIsIncluded(x, y)
            | RationalNumbers ->
                match x with
                | x when BinaryFunction.op_True(Module(x) |+| WholeNumbers) -> True
                | _ -> False
            | RealNumbers ->
                match x with
                | Pi -> True
                | Exponent -> True
                | SilverNumber -> True
                | GoldenNumber -> True
                | EulerConstant -> True
                | x when BinaryFunction.op_True(Module(x) |+| RationalNumbers) -> True
                | _ -> this
            | _ -> this
        | VectorIsColumn(x) ->
            let x = x.Calculate(definition)
            match x with
            | Vector _ -> False
            | ColumnVector _ -> True
            | MatrixRowAt _ -> False
            | MatrixColumnAt _ -> True
            | x -> VectorIsColumn(x)
        | VectorIsRow(x) ->
            let x = x.Calculate(definition)
            match x with
            | Vector _ -> True
            | ColumnVector _ -> False
            | MatrixRowAt _ -> True
            | MatrixColumnAt _ -> False
            | x -> VectorIsRow(x)
        | ScalarAny(x, y) ->
            let x = x.Calculate(definition)
            let y = y.Calculate(definition)
            match y with
            | False -> ScalarAny(x, False)
            | True -> ScalarAny(x, True)
            | y -> ScalarAny(x, y)
        | _ -> this

    member this.Identificator
        with get() : string[] =
            match this with
            | And _ -> [|"∧"; "&"|]
            | Or _ -> [|"∨"; "|"|]
            | XOr _ -> [|"⊕"; "⊻"; "⨧"|]
            | Not _ -> [|"¬"|]
            | Implication _ -> [|"⇒"|]
            | Identity _ -> [|"⇔"|]
            | ScalarEqual _ | VectorEqual _ -> [|"="|]
            | ScalarNotEqual _ | VectorNotEqual _ -> [|"≠"|]
            | ScalarGreaterThan _ -> [|">"|]
            | ScalarGreaterThanOrEqual _ -> [|"⩾"; "≥"|]
            | ScalarLessThan _ -> [|"<"|]
            | ScalarLessThanOrEqual _ -> [|"⩽"; "≤"|]
            | ScalarAny _ -> [|"∀"|]
            | ScalarExist _ -> [|"∃"|]
            | ScalarFind _ -> [|":"|]
            | SetScalarIsIncluded _ -> [|"∈"|]
            | SetScalarIsNotIncluded _ -> [|"∉"|]
            | True -> [|"true"; "1"|]
            | False -> [|"false"; "0"|]
            | _ -> [|"?"|]

    override this.ToString() : string =
        let mutable result = StringBuilder()
        match this with
        | And(functions) | Or(functions) | XOr(functions) ->
            for i0 = 0 to functions.Count() - 1 do
                //if functions.ElementAt(i0).Priority <= this.Priority then result <- result.Append('(')
                result <- result.Append(functions.ElementAt(i0))
                //if functions.ElementAt(i0).Priority <= this.Priority then result <- result.Append(')')
                if i0 + 1 <> functions.Count() then result <- result.Append(this.Identificator.[0])
        | ScalarEqual(x, y) | ScalarNotEqual(x, y) | ScalarGreaterThan(x, y) | ScalarGreaterThanOrEqual(x, y) | ScalarLessThan(x, y) | ScalarLessThanOrEqual(x, y) ->
            result <- result.Append(x)
            result <- result.Append(" ")
            result <- result.Append(this.Identificator.[0])
            result <- result.Append(" ")
            result <- result.Append(y)
        | ScalarAny(x, y) | ScalarExist(x, y) | ScalarFind(x, y) ->
            result <- result.Append(x)
            result <- result.Append(" ")
            result <- result.Append(this.Identificator.[0])
            result <- result.Append(" ")
            result <- result.Append(y)
        | Implication(x, y) | Identity(x, y) ->
            result <- result.Append(x)
            result <- result.Append(" ")
            result <- result.Append(this.Identificator.[0])
            result <- result.Append(" ")
            result <- result.Append(y)
        | SetScalarIsIncluded(x, y) | SetScalarIsNotIncluded(x, y) ->
            result <- result.Append(x)
            result <- result.Append(" ")
            result <- result.Append(this.Identificator.[0])
            result <- result.Append(" ")
            result <- result.Append(y)
        | Not(x) ->
            result <- result.Append(this.Identificator.[0])
            result <- result.Append(" ")
            result <- result.Append(x)
        | VectorEqual(x, y) | VectorNotEqual (x, y) ->
            result <- result.Append(x)
            result <- result.Append(" ")
            result <- result.Append(this.Identificator.[0])
            result <- result.Append(" ")
            result <- result.Append(y)
        | True | False -> result <- result.Append(this.Identificator.[0])
        | _ -> result <- result.Append("?")
        result.ToString()