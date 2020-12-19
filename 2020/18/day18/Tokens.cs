namespace day18
{
    abstract class Token { }

    class LparenToken : Token
    {
        public override string ToString()
        {
            return $"LparenToken";
        }        
    }

    class RparenToken : Token
    {
        public override string ToString()
        {
            return $"RparenToken";
        }        
    }

    class BinopToken : Token
    {
        public readonly BinopKind Kind;
        public BinopToken(BinopKind kind)
        {
            this.Kind = kind;
        }

        public override string ToString()
        {
            return $"BinopToken({Kind})";
        }        
    }

    class LiteralToken : Token
    {
        public readonly int Value;
        public LiteralToken(int Value)
        {
            this.Value = Value;
        }

        public override string ToString()
        {
            return $"LiteralToken({Value})";
        }
    }
}
