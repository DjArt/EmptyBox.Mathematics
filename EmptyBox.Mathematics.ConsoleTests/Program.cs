using EmptyBox.Mathematics.Expressions;
using static EmptyBox.Mathematics.Expressions.BinaryFunction;
using static EmptyBox.Mathematics.Expressions.VectorFunction;
using static EmptyBox.Mathematics.Expressions.SetFunction;
using static EmptyBox.Mathematics.Expressions.ScalarFunction;
using static EmptyBox.Mathematics.Expressions.MatrixFunction;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace EmptyBox.Mathematics.ConsoleTests
{
    class Program
    {
        static void Main(string[] args)
        {
            //
            ScalarFunction x = "x";
            ScalarFunction y = "y";
            ScalarFunction a = "a";
            ScalarFunction b = "b";
            ScalarFunction c = "c";
            ScalarFunction d = "d";
            ScalarFunction i = "i";
            ScalarFunction j = "j";
            MatrixFunction m = NewMatrix(new Matrix<ScalarFunction>(
                new[] {
                    new ScalarFunction[] { a, b },
                    new ScalarFunction[] { c, d }
                }));
            VectorFunction v = NewColumnVector(new ScalarFunction[] { x, y });
            VectorFunction r = NewColumnVector(new ScalarFunction[] { i, j });
            BinaryFunction k = MatrixMethods.RotationMethod(m, v, r);
            //k = k.Calculate();
            Console.WriteLine(((a * i + c * j) / (a * a + c * c)).Calculate());
            var sqrt = op_Exponentiation(op_Exponentiation(a, 2) + op_Exponentiation(c, 2), 0.5);
            var d_y = ((a * j - c * i) / (a * d - c * b)) * (a * b + c * d);
            var gg = ((x * (a * a + c * c)) + y) / sqrt == (a * i + c * j) / sqrt;
            var gg2 = NewScalarFind(x, gg).Calculate();
            var gg3 = gg2.Calculate(y == d_y);
            Console.ReadKey();
        }
    }
}
