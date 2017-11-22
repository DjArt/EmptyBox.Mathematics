using EmptyBox.Mathematics.Expressions;
using static EmptyBox.Mathematics.Expressions.VectorFunction;
using static EmptyBox.Mathematics.Expressions.MatrixFunction;
using static EmptyBox.Mathematics.Expressions.BinaryFunction;
using static EmptyBox.Mathematics.Expressions.ScalarFunction;
using static EmptyBox.Mathematics.Expressions.SetFunction;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace EmptyBox.Mathematics.Tests
{
    [TestClass]
    public class MatrixMethodsTest
    {
        [TestMethod]
        public void RotateMethod()
        {
            MatrixFunction m = NewMatrix(new Matrix<ScalarFunction>(
                new[] {
                    new ScalarFunction[] { "a", "b"},
                    new ScalarFunction[] { "c", "d"}
                }));
            VectorFunction v = NewColumnVector(new ScalarFunction[] { "x", "y" });
            VectorFunction r = NewColumnVector(new ScalarFunction[] { "i", "j" });
            BinaryFunction b = MatrixMethods.RotationMethod(m, v, r);
        }
    }
}
