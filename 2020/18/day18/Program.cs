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
            long n = 0;
            List<Expression> expressions = lines.Select(Expression.fromString).ToList();
            foreach (var ex in expressions)
            {
                long v = ex.eval();
                n += v;
                Console.Error.WriteLine($"{v} {ex}");
            }
            Console.WriteLine(n);
        }
    }
}
