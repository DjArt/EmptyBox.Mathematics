using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using EmptyBox.Mathematics.Expressions;
using static EmptyBox.Mathematics.Expressions.ScalarFunction;

namespace EmptyBox.Mathematics.Tests
{
    [TestClass]
    public class UnitTest1
    {
        public static Random Random = new Random();

        [TestMethod]
        public void Heat()
        {
            ScalarFunction x = "x";
            ScalarFunction y = "y";
            ScalarFunction z = x + y;
            double? e = z;
            double? c = z.Calculate("x" == NewConstant(9) & "y" == NewConstant(10));
        }

        [TestMethod]
        public void CheckConstants()
        {
            double a = Random.NextDouble();
            double b = Random.NextDouble();
            ScalarFunction x = "x";
            ScalarFunction y = "y";
            double? result = (x + y).Calculate("x" == NewConstant(a) & "y" == NewConstant(b));
            Assert.AreEqual(a + b, result);
            result = (x - y).Calculate("x" == NewConstant(a) & "y" == NewConstant(b));
            Assert.AreEqual(a - b, result);
            result = (x * y).Calculate("x" == NewConstant(a) & "y" == NewConstant(b));
            Assert.AreEqual(a * b, result);
        }

        [TestMethod]
        public void CheckBinaryAddition()
        {
            ScalarFunction x = "x";
            ScalarFunction y = "y";
            Assert.AreEqual(x + y, x + y);
            Assert.AreEqual(y + x, y + x);
            Assert.AreEqual(x + x, 2 * x);
            Assert.AreEqual(y + y, 2 * y);
            Assert.AreEqual(0 + x, x);
            Assert.AreEqual(x + 0, x);
        }

        [TestMethod]
        public void CheckSubtraction()
        {
            ScalarFunction x = "x";
            ScalarFunction y = "y";
            Assert.AreEqual(0 - x, -x);
            Assert.AreEqual(x - 0, x);
            Assert.AreEqual(x - x, 0);
            Assert.AreEqual(x - y, x - y);
            Assert.AreEqual(y - x, y - x);
            Assert.AreNotEqual(y - x, x - y);
            Assert.AreNotEqual(x - y, y - x);
        }

        [TestMethod]
        public void CheckDevirative()
        {
            ScalarFunction x = "x";
            ScalarFunction y = "y";
            Assert.AreEqual(NewDevirative(0, x), 0);
            Assert.AreEqual(NewDevirative(y, x), 0);
            Assert.AreEqual(NewDevirative(x, x), 1);
            //Assert.AreEqual(NewDevirative(x * y, x).Calculate(), y);
            Assert.AreEqual(NewDevirative(op_Exponentiation(x, 2), x), 2*x);
            Assert.AreEqual(NewDevirative(op_Exponentiation(x, 3), x), 3*op_Exponentiation(x, 2));
            Assert.AreEqual(NewDevirative(op_Exponentiation(x, y), x), y* op_Exponentiation(x, y-1));
        }
    }
}
