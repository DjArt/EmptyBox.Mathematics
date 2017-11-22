namespace EmptyBox.Mathematics

open System
open System.Collections
open System.Collections.Generic
open System.Linq
open System.Runtime.CompilerServices

type public IReadOnlyMatrix<'T> =
    inherit IEnumerable<'T>
    abstract member Item : row : int * column : int -> 'T with get
    abstract member RowsCount : int with get
    abstract member ColumnsCount : int with get
    abstract member Rows : IEnumerable<IEnumerable<'T>> with get
    abstract member Columns : IEnumerable<IEnumerable<'T>> with get

type public IMatrix<'T> =
    inherit IReadOnlyMatrix<'T>
    abstract member Item : row : int * column : int -> 'T with get, set
    abstract member Resize : rows : int * columns : int -> unit
    abstract member RowAt : index : int -> IEnumerable<'T> with get, set
    abstract member ColumnAt : index : int -> IEnumerable<'T> with get, set

type public Matrix<'T>(rows : int, columns : int) =
    let mutable _RowsCount = rows
    let mutable _ColumnsCount = columns
    let mutable _Store : 'T[] = Array.zeroCreate(rows * columns)

    new (source : IEnumerable<IEnumerable<'T>>) as this =
        let rows = source.Count()
        let columns = source.Max(fun x -> x.Count())
        Matrix<'T>(rows, columns)
        then
            for i0 in 0 .. source.Count() - 1 do
                for i1 in 0 .. source.ElementAt(i0).Count() - 1 do
                    this.[i0, i1] <- source.ElementAt(i0).ElementAt(i1)

    interface IReadOnlyMatrix<'T> with
        member this.Item
            with get(row : int, column : int) = this.[row, column]
        member this.RowsCount = this.RowsCount
        member this.ColumnsCount = this.ColumnsCount
        member this.Rows = this.Rows
        member this.Columns = this.Columns
        member this.GetEnumerator() : IEnumerator<'T> = _Store.AsEnumerable<'T>().GetEnumerator()
        member this.GetEnumerator() : IEnumerator = _Store.GetEnumerator()

    interface IMatrix<'T> with
        member this.Item
            with get(row : int, column : int) = this.[row, column]
            and set(row : int, column : int) value = this.[row, column] <- value
        member this.Resize(rows : int, columns : int) = this.Resize(rows, columns)
        member this.RowAt
            with get(index : int) = this.RowAt(index)
            and set(index : int) value = this.RowAt(index) <- value
        member this.ColumnAt
            with get(index : int) = this.ColumnAt(index)
            and set(index : int) value = this.ColumnAt(index) <- value

    member public this.RowsCount with get() = _RowsCount
    member public this.ColumnsCount with get() = _ColumnsCount

    member public this.Item
        with get(row : int, column : int) = _Store.[row * _ColumnsCount + column]
        and set(row : int, column : int) (value : 'T) = _Store.[row * _ColumnsCount + column] <- value

    /// <summary>
    /// Возвращает или задаёт строку матрицы.
    /// Задаваемая последовательность должна иметь размер, равный количеству столбцов матрицы.
    /// </summary>
    /// <param name="index"></param>
    member public this.RowAt
        with get(index : int) =
            match index < _RowsCount with
            | true -> _Store.[index * _ColumnsCount .. (index + 1) * _ColumnsCount - 1] :> IEnumerable<'T>
            | false -> raise(ArgumentOutOfRangeException())
        and set(index : int) (value : IEnumerable<'T>) =
            match value.Count() = _ColumnsCount && index < _RowsCount with
            | true -> for i0 in 0 .. _ColumnsCount - 1 do _Store.[index * _ColumnsCount + i0] <- value.ElementAt(i0)
            | false -> raise(ArgumentOutOfRangeException())

    /// <summary>
    /// Возвращает или задаёт стобец матрицы.
    /// Задаваемая последовательность должна иметь размер, равный количеству строк матрицы.
    /// </summary>
    /// <param name="index">Индекс стобца</param>
    member public this.ColumnAt
        with get(index : int) =
            match index < _ColumnsCount with
            | true -> [for i0 in 0 .. _RowsCount - 1 -> _Store.[i0 * _ColumnsCount + index]] :> IEnumerable<'T>
            | false -> raise(ArgumentOutOfRangeException())
        and set(index : int) (value : IEnumerable<'T>) =
            match value.Count() = _RowsCount && index < _ColumnsCount with
            | true -> for i0 in 0 .. _RowsCount - 1 do _Store.[i0 * _ColumnsCount + index] <- value.ElementAt(i0)
            | false -> raise(ArgumentOutOfRangeException())

    /// <summary>
    /// Возвращает множество всех строк матрицы.
    /// </summary>
    member public this.Rows
        with get() =
            [for i0 in 0 .. _RowsCount - 1 -> _Store.[i0 * _ColumnsCount .. (i0 + 1) * _ColumnsCount - 1] :> IEnumerable<'T>] :> IEnumerable<IEnumerable<'T>>
  
    /// <summary>
    /// Возвращает множество всех столбцов матрицы.
    /// </summary>
    member public this.Columns
        with get() =
            [for i0 in 0 .. _ColumnsCount - 1 -> [for i1 in 0 .. _RowsCount - 1 -> _Store.[i1 * _ColumnsCount + i0]] :> IEnumerable<'T>] :> IEnumerable<IEnumerable<'T>>

    /// <summary>
    /// Изменяет размеры матрицы до указанных значений.
    /// Элементы после изменения размеров остаются на своих местах, если строки и столбцы, в которых они находились, не были удалены.
    /// </summary>
    /// <param name="rows">Количество строк</param>
    /// <param name="columns">Количество столбцов</param>
    member public this.Resize(rows : int, columns : int) : unit =
        let old_Store = Array.copy(_Store)
        let old_ColumnsCount = _ColumnsCount
        let copy_RowsCount =
            match rows > _RowsCount with
            | true -> _RowsCount
            | false -> rows
        let copy_ColumnsCount =
            match columns > _ColumnsCount with
            | true -> _ColumnsCount
            | false -> columns
        _Store <- Array.zeroCreate(rows * columns)
        _RowsCount <- rows
        _ColumnsCount <- columns
        for i0 in 0 .. copy_RowsCount - 1 do for i1 in 0 .. copy_ColumnsCount - 1 do this.[i0, i1] <- old_Store.[i0 * old_ColumnsCount + i1]

[<Extension>]
type public MatrixExtensions =
    /// <summary>
    /// Преобразует последовательность элементов в матрицу на основе их индекса.
    /// </summary>
    /// <param name="this" Последовательность элементов />
    /// <param name="rows" Количество строк в матрице />
    /// <param name="columns" Количество столбцов в матрице />
    /// <param name="indexer" Функция преобразования индекса элемента последовательности в координаты ячейки массива />
    [<Extension>]
    static member public ToMatrix(this : IEnumerable<'T>, rows : int, columns : int, indexer : Func<int, struct (int * int)>) =
        let m = Matrix<'T>(rows, columns)
        for i0 in 0 .. this.Count() - 1 do
            let x, y = indexer.Invoke(i0).ToTuple()
            m.[x, y] <- this.ElementAt(i0)
        m
    
    /// <summary>
    /// Преобразует последовательность элементов в матрицу на основе их индекса.
    /// </summary>
    /// <param name="this" Последовательность элементов />
    /// <param name="rows" Количество строк в матрице />
    /// <param name="columns" Количество столбцов в матрице />
    [<Extension>]
    static member public ToMatrix(this : IEnumerable<'T>, rows : int, columns : int) = this.ToMatrix(rows, columns, fun x -> struct (x / columns, x % columns))