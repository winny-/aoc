using System;
using System.Text.RegularExpressions;
using System.Collections.Generic;
using System.Linq;

namespace day18
{

    enum PrecedenceRule
    {
        SameLevel,
        PlusFirst,
    }

    class TokenOrExpression
    {
        public readonly Token Token;
        public readonly Expression Expression;
        public TokenOrExpression(Token token)
        {
            this.Token = token;
        }
        public TokenOrExpression(Expression expression)
        {
            this.Expression = expression;
        }
    }
    
    class Expression
    {
        public virtual ulong eval()
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
                        tokens.Add(new LiteralToken(ulong.Parse(m.Groups[1].Value)));
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
        public static Expression fromString(String s, PrecedenceRule rule)
        {
            List<Token> tokens = tokenize(s);
            // dumpTokens(tokens);
            // tokens.Reverse();
            return fromTokens(tokens, rule);
        }

        public static void dumpTokens(List<Token> tokens)
        {
            Console.WriteLine(String.Join(" ", tokens.Select(x => x.ToString())));
        }

        public static Expression fromTokens(List<Token> tokens, PrecedenceRule rule)
        {
            Stack<TokenOrExpression> stack = new Stack<TokenOrExpression>();
            while (true)
            {
                if (tokens.Count == 0 && stack.Count == 1 && stack.Peek()?.Expression != null)
                {
                    return stack.Pop().Expression;
                }

                if (stack.Count == 0)
                {
                    stack.Push(new TokenOrExpression(tokens[0]));
                    tokens.RemoveAt(0);
                }
                else if (stack.Peek()?.Token is LiteralToken)
                {
                    ulong val = (stack.Pop().Token as LiteralToken).Value;
                    stack.Push(new TokenOrExpression(new Literal(val)));
                }
                else if (stack.Peek()?.Token is RparenToken)
                {
                    stack.Pop();  // Rparen
                    Expression g = new Group(stack.Pop().Expression);
                    stack.Pop();  // Lparen
                    stack.Push(new TokenOrExpression(g));
                }
                else if (stack.Peek()?.Expression != null)
                {
                    bool reduce = true;
                    if (rule == PrecedenceRule.PlusFirst)
                    {
                        int open = 0;
                        int seen = 0;
                        foreach (var t in tokens)
                        {
                            if (t is LparenToken)
                            {
                                open++;
                                continue;
                            }
                            else if (t is RparenToken)
                            {
                                if (--open > 0)
                                {
                                    continue;
                                }
                                else if (open < 0)
                                {
                                    break;
                                }
                            }

                            if (open == 0 && ++seen > 1)
                            {
                                break;
                            }

                            if (t is BinopToken)
                            {
                                BinopToken tok = t as BinopToken;
                                if (tok.Kind == BinopKind.Plus)
                                {
                                    reduce = false;
                                    break;
                                }
                            }
                        }
                    }
                    Expression e1 = stack.Pop().Expression;
                    if (reduce && stack.TryPeek(out TokenOrExpression top) && top.Token is BinopToken)
                    {
                        BinopToken binop = stack.Pop().Token as BinopToken;
                        Expression e2 = stack.Pop().Expression;
                        stack.Push(new TokenOrExpression(new Binop(e1, binop.Kind, e2)));
                    }
                    else  // Probably Lparen
                    {
                        stack.Push(new TokenOrExpression(e1));
                        stack.Push(new TokenOrExpression(tokens[0]));
                        tokens.RemoveAt(0);
                    }
                }
                else
                {
                    stack.Push(new TokenOrExpression(tokens[0]));
                    tokens.RemoveAt(0);                    
                }
            }
        }
    }
}
