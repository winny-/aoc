using System;

namespace day18
{
    enum BinopKind
    {
        Times,
        Plus
    }
    
    
    class Binop : Expression
    {
        public readonly Expression Left;
        public readonly Expression Right;
        public readonly BinopKind Kind;
        
        public override ulong eval()
        {
            var le = Left.eval();
            var re = Right.eval();
            ulong result = 0;
            if (Kind == BinopKind.Times)
            {
                result =  le * re;
            } else {
                result =  le + re;
            }

            // Console.Error.WriteLine($"{this} = {le} {(Kind == BinopKind.Plus ? '+' : '*')} {re} = {result}");
            return result;

        }

        public Binop(Expression left, BinopKind kind, Expression right)
        {
            this.Left = left;
            this.Right= right;
            this.Kind = kind;
        }

        public override string ToString()
        {
            return $"{Left} {(Kind == BinopKind.Plus ? '+' : '*')} {Right}";
        }
    }
}
