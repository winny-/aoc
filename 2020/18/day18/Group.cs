namespace day18
{
    class Group : Expression
    {
        public readonly Expression Expr;

        public override ulong eval()
        {
            return Expr.eval();
        }

        public Group(Expression expr)
        {
            this.Expr = expr;
        }

        public override string ToString()
        {
            return $"({Expr})";
        }
    }
}
