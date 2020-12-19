namespace day18
{
    class Literal : Expression
    {
        public readonly int Value;

        public override int eval()
        {
            return Value;
        }

        public Literal(int val)
        {
            Value = val;
        }

        public override string ToString()
        {
            return $"{Value}";
        }
    }
}
