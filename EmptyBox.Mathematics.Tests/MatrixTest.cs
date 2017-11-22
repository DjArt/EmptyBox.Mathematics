using System;
using System.Text;
using System.Collections.Generic;
using System.Collections;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using EmptyBox.Mathematics;
using System.Linq;

namespace EmptyBox.Mathematics.Tests
{
    /// <summary>
    /// Тесты класса EmptyBox.Mathematics.Matrix<T>
    /// </summary>
    [TestClass]
    public class MatrixTest
    {
        public static Random Random = new Random();
        [TestMethod]
        public void FillAndCheck()
        {
            int rows = Random.Next(3, 1024);
            int columns = Random.Next(3, 1024);
            Matrix<double> matrix = new Matrix<double>(rows, columns);
            double[,] store = new double[rows, columns];
            for (int i0 = 0; i0 < rows; i0++)
            {
                for (int i1 = 0; i1 < columns; i1++)
                {
                    store[i0, i1] = Random.NextDouble();
                    matrix[i0, i1] = store[i0, i1];
                }
            }
            for (int i0 = 0; i0 < rows; i0++)
            {
                for (int i1 = 0; i1 < columns; i1++)
                {
                    Assert.AreEqual(matrix[i0, i1], store[i0, i1]);
                }
            }
        }

        [TestMethod]
        public void DecreaseSizeCheck()
        {
            int rows = Random.Next(3, 1024);
            int columns = Random.Next(3, 1024);
            Matrix<double> matrix = new Matrix<double>(rows, columns);
            double[,] store = new double[rows, columns];
            for (int i0 = 0; i0 < rows; i0++)
            {
                for (int i1 = 0; i1 < columns; i1++)
                {
                    store[i0, i1] = Random.NextDouble();
                    matrix[i0, i1] = store[i0, i1];
                }
            }
            int new_rows = Random.Next(2, rows);
            int new_columns = Random.Next(2, columns);
            matrix.Resize(new_rows, new_columns);
            for (int i0 = 0; i0 < new_rows; i0++)
            {
                for (int i1 = 0; i1 < new_columns; i1++)
                {
                    Assert.AreEqual(matrix[i0, i1], store[i0, i1]);
                }
            }
        }

        [TestMethod]
        public void IncreaseSizeCheck()
        {
            int rows = Random.Next(3, 1024);
            int columns = Random.Next(3, 1024);
            Matrix<double> matrix = new Matrix<double>(rows, columns);
            double[,] store = new double[rows, columns];
            for (int i0 = 0; i0 < rows; i0++)
            {
                for (int i1 = 0; i1 < columns; i1++)
                {
                    store[i0, i1] = Random.NextDouble();
                    matrix[i0, i1] = store[i0, i1];
                }
            }
            int new_rows = Random.Next(rows, 2048);
            int new_columns = Random.Next(columns, 2048);
            matrix.Resize(new_rows, new_columns);
            for (int i0 = 0; i0 < rows; i0++)
            {
                for (int i1 = 0; i1 < columns; i1++)
                {
                    Assert.AreEqual(matrix[i0, i1], store[i0, i1]);
                }
            }
        }

        [TestMethod]
        public void ConvertFromIEnumerable()
        {
            Matrix<int> matrix = (new[] { 0, 1, 2, 3 }).ToMatrix(2, 2);
            Assert.AreEqual(matrix[0, 0], 0);
            Assert.AreEqual(matrix[0, 1], 1);
            Assert.AreEqual(matrix[1, 0], 2);
            Assert.AreEqual(matrix[1, 1], 3);
        }
    }
}
