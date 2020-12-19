using System;
using System.Collections.Generic;
using System.Linq;

namespace day18
{
    class Program
    {
        static void Main(string[] args)
        {
            List<String> lines = new List<string>();
            while (true)
            {
                var line = Console.ReadLine();
                if (line is null)
                {
                    break;
                }
                lines.Add(line);
            }
            ulong n1 = 0;
            ulong n2 = 0;
            List<Expression> expressionsPart1 = lines.Select(
                line => Expression.fromString(line, PrecedenceRule.SameLevel)
            ).ToList();
            List<Expression> expressionsPart2 = lines.Select(
                line => Expression.fromString(line, PrecedenceRule.PlusFirst)
            ).ToList();
            foreach (var ex in expressionsPart1)
            {
                ulong v1 = ex.eval();
                n1 += v1;
                // Console.WriteLine($"{v}");
            }
            foreach (var ex in expressionsPart2)
            {
                ulong v2 = ex.eval();
                n2 += v2;
            }
            Console.WriteLine(n1);
            Console.WriteLine(n2);
        }
    }
}
