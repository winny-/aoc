namespace day18
{
    class Literal : Expression
    {
        public readonly ulong Value;

        public override ulong eval()
        {
            return Value;
        }

        public Literal(ulong val)
        {
            Value = val;
        }

        public override string ToString()
        {
            return $"{Value}";
        }
    }
}
