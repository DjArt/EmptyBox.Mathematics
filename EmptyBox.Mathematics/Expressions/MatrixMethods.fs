namespace EmptyBox.Mathematics.Expressions

open System
open System.Linq
open EmptyBox.ScriptRuntime.Extensions
open EmptyBox.Mathematics

module public MatrixMethods =
    let RotationMethod(matrix : MatrixFunction, vector : VectorFunction, result : VectorFunction) : BinaryFunction =
        match (matrix, vector, result) with
        | (Matrix _, ColumnVector _, ColumnVector _) ->
            let mutable r = result.Copy()
            let mutable m = matrix.Copy()
            for i0 in Constant(0.) .. MatrixRowsCount(m) .. MatrixRowsCount(m) do
                for i1 in i0+Constant(1.) .. MatrixRowsCount(m) .. MatrixRowsCount(m) do
                    let div = (m.[i0, i0] ** Constant(2.) + m.[i1, i0] ** Constant(2.)) ** Constant(0.5)
                    let c = m.[i0, i0] / div
                    let s = m.[i1, i0] / div
                    let row0 = MatrixRowAt(m, i0) * c + MatrixRowAt(m, i1) * s
                    let row1 = MatrixRowAt(m, i1) * c - MatrixRowAt(m, i0) * s
                    m <- SetRowAt(m, i0, row0)
                    m <- SetRowAt(m, i1, row1)
                    let elem0 = r.[i0] * c + r.[i1] * s
                    let elem1 = r.[i1] * c - r.[i0] * s
                    r <- VectorFunction.SetElementAt(r, i0, elem0)
                    r <- VectorFunction.SetElementAt(r, i1, elem1)
            VectorFind(vector, VectorEqual((m * vector), r))
        | _ -> BinaryFunction.None

