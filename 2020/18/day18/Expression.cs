using System;
using System.Text.RegularExpressions;
using System.Collections.Generic;
using System.Linq;

namespace day18
{
    class Expression
    {
        public virtual int eval()
        {
            throw new Exception("Not implemented");
        }

        

        public static List<Token>tokenize(String s)
        {
            List<Token> tokens = new List<Token>();
            while (!string.IsNullOrWhiteSpace(s))
            {
                s = s.Trim();
                switch (s[0])
                {
                    case '(':
                        tokens.Add(new LparenToken());
                        s = s.Substring(1);
                        break;
                    case ')':
                        tokens.Add(new RparenToken());
                        s = s.Substring(1);
                        break;
                    case '+':
                        tokens.Add(new BinopToken(BinopKind.Plus));
                        s = s.Substring(1);
                        break;
                    case '*':
                        tokens.Add(new BinopToken(BinopKind.Times));
                        s = s.Substring(1);
                        break;
                    case char n when n <= '9' && n >= '0':
                        var m = Regex.Match(s, @"([0-9]+)(.*)");
                        tokens.Add(new LiteralToken(int.Parse(m.Groups[1].Value)));
                        s = m.Groups[2].Value;
                        break;
                    default:
                        s = s.Substring(1);
                        break;
                }
            }
            return tokens;
        }

        // Woops.  This is left-associative.  I wanted a right-associative tree.
        public static Expression fromString(String s)
        {
            List<Token> tokens = tokenize(s);
            // dumpTokens(tokens);
            // tokens.Reverse();
            return fromTokens(tokens);
        }

        public static void dumpTokens(List<Token> tokens)
        {
            Console.WriteLine(String.Join(" ", tokens.Select(x => x.ToString())));
        }

        public static Expression fromTokens(List<Token> tokens)
        {
            var t = tokens[tokens.Count-1];
            tokens.RemoveAt(tokens.Count-1);
            Expression e1 = null;
            if (t is RparenToken)
            {
                int open = 1;
                List<Token> parenthesizedTokens = new List<Token>();
                while (true)
                {

                    var u = tokens[tokens.Count-1];
                    tokens.RemoveAt(tokens.Count-1);
                    // Console.WriteLine($"u: {u}");
                    // dumpTokens(parenthesizedTokens);
                    if (u is RparenToken)
                    {
                        open++;

                    }
                    else if (u is LparenToken)
                    {
                        open--;
                    }
                    if (open > 0)
                    {
                        parenthesizedTokens.Insert(0, u);
                        // parenthesizedTokens.Add(u);
                    }
                    else
                    {
                        e1 = new Group(fromTokens(parenthesizedTokens));
                        break;
                    }
                }
            }
            else if (t is LiteralToken)
            {
                e1 = new Literal(((LiteralToken)t).Value);
            }
            if (tokens.Count == 0)
            {
                return e1;
            }

            // dumpTokens(tokens);

            Token next = tokens[tokens.Count-1];
            tokens.RemoveAt(tokens.Count-1);
            // Console.WriteLine($"next: {next}");
            if (next is BinopToken)
            {
                return new Binop(fromTokens(tokens), ((BinopToken)next).Kind, e1);
            }
            else if (next is LparenToken)
            {
                tokens.Add(next);
                return fromTokens(tokens);
            }


            throw new Exception("Illegal state");
        }
    }
}
