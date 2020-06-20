namespace JmesPath
{
    using System;
    using System.Collections.Generic;

    partial class TreeProjector<T> : IProjector
    {
        readonly Stack<T> _stack = new Stack<T>();
        readonly ITreeProjector<T> _tree;

        public T Result => _stack.Peek();

        public TreeProjector(ITreeProjector<T> tree) =>
            _tree = tree ?? throw new ArgumentNullException(nameof(tree));

        void Push(T node) => _stack.Push(node);
        T Pop() => _stack.Pop();

        (T, T) Pop2()
        {
            var (right, left) = (_stack.Pop(), _stack.Pop());
            return (left, right);
        }

        void Binary(Action<ITreeProjector<T>, T, T> ta)
        {
            var (right, left) = (_stack.Pop(), _stack.Pop());
            ta(_tree, left, right);
        }

        void Ternary(Action<ITreeProjector<T>, T, T, T> ta)
        {
            var (c, b, a) = (_stack.Pop(), _stack.Pop(), _stack.Pop());
            ta(_tree, a, b, c);
        }
        
        T[] Pop(int count)
        {
            var items = new T[count];
            for (var i = count - 1; i >= 0; i--)
                items[i] = Pop();
            return items;
        }

        public void RawString(string s, int index, int length) =>
            Push(_tree.RawString(s, index, length));

        public void UnquotedString(string s, int index, int length) =>
            Push(_tree.UnquotedString(s, index, length));

        public void QuotedString(string s, int index, int length) =>
            Push(_tree.QuotedString(s, index, length));

        public void Literal(string s, int index, int length) =>
            Push(_tree.Literal(s, index, length));

        public void CurrentNode() => Push(_tree.CurrentNode());

        public void Not() => Push(_tree.Not(Pop()));

        public void And()              { var (a, b) = Pop2(); Push(_tree.And(a, b)); }
        public void Or()               { var (a, b) = Pop2(); Push(_tree.Or(a, b)); }
        public void GreaterThan()      { var (a, b) = Pop2(); Push(_tree.GreaterThan(a, b)); }
        public void GreaterThanEqual() { var (a, b) = Pop2(); Push(_tree.GreaterThanEqual(a, b)); }
        public void LessThan()         { var (a, b) = Pop2(); Push(_tree.LessThan(a, b)); }
        public void LessThanEqual()    { var (a, b) = Pop2(); Push(_tree.LessThanEqual(a, b)); }
        public void Equal()            { var (a, b) = Pop2(); Push(_tree.Equal(a, b)); }
        public void NotEqual()         { var (a, b) = Pop2(); Push(_tree.NotEqual(a, b)); }

        public void Flatten() => Push(_tree.Flatten(Pop()));

        public void Field() => Push(_tree.Field(Pop()));
        public void Identity() => Push(_tree.Identity());

        public void ValueProjection()
        {
            var (left, right) = Pop2();
            Push(_tree.ValueProjection(left, right));
        }

        public void MultiSelectList(int count) =>
            _tree.MultiSelectList(Pop(count));

        public void MultiSelectHash(int count)
        {
            var items = new KeyValuePair<string, T>[count];
            for (var i = count - 1; i >= 0; i--)
                items[i] = new KeyValuePair<string, T>(null, Pop());
            _tree.MultiSelectHash(items);
        }

        public void FilterProjection() =>
            Ternary((tree, left, right, condition) => tree.FilterProjection(left, right, condition));

        public void Projection() =>
            Binary((tree, left, right) => tree.Projection(left, right));

        public void Index(int index)
        {
            _tree.Index(Pop(), index);
        }

        public void Slice(int? start, int? stop, int? step)
        {
            throw new NotImplementedException();
        }

        public void Reference()
        {
            throw new NotImplementedException();
        }

        public void SubExpression() =>
            Binary((tree, left, right) => tree.SubExpression(left, right));

        public void Pipe() =>
            Binary((tree, left, right) => tree.Pipe(left, right));

        public void Function(int count)
        {
            var args = Pop(count);
            _tree.Function(Pop(), args);
        }
    }

    partial interface ITreeProjector<T>
    {
        T RawString(string s, int index, int length);
        T UnquotedString(string s, int index, int length);
        T QuotedString(string s, int index, int length);
        T Literal(string s, int index, int length);
        T CurrentNode();
        T Not(T expression);
        T And(T left, T right);
        T Or(T left, T right);
        T GreaterThan(T left, T right);
        T GreaterThanEqual(T left, T right);
        T LessThan(T left, T right);
        T LessThanEqual(T left, T right);
        T Equal(T left, T right);
        T NotEqual(T left, T right);
        T Flatten(T source);
        T Field(T name);
        T Identity();
        T ValueProjection(T left, T right);
        T MultiSelectList(T[] list);
        T MultiSelectHash(KeyValuePair<string, T>[] hash);
        T FilterProjection(T left, T right, T condition);
        T Projection(T left, T right);
        T Index(T source, int index);
        T Slice(int? start, int? stop, int? step);
        T Reference();
        T SubExpression(T left, T right);
        T Pipe(T left, T right);
        void Function(T name, T[] args);
    }

    partial interface IProjector
    {
        void RawString(string s, int index, int length);
        void UnquotedString(string s, int index, int length);
        void QuotedString(string s, int index, int length);
        void Literal(string s, int index, int length);
        void CurrentNode();
        void Not();
        void And();
        void Or();
        void GreaterThan();
        void GreaterThanEqual();
        void LessThan();
        void LessThanEqual();
        void Equal();
        void NotEqual();
        void Flatten();
        void Field();
        void Identity();
        void ValueProjection();
        void MultiSelectList(int count);
        void MultiSelectHash(int count);
        void FilterProjection();
        void Projection();
        void Index(int index);
        void Slice(int? start, int? stop, int? step);
        void Reference();
        void SubExpression();
        void Pipe();
        void Function(int count);
    }

    static partial class Machine
    {
        public static void Run(ParsedExpression pe, IProjector projector)
        {
            var r1 = (int?)null;
            var r2 = (int?)null;
            var r3 = (int?)null;
            var r = 0;

            int? Pop() => r-- switch
            {
                1 => r1,
                2 => r2,
                3 => r3,
                _ => throw new Exception("Implementation error.")
            };

            foreach (var (_, code, arg) in pe.Ops)
                Do(code, arg);

            void Do(OpCode code, int arg)
            {
                switch (code)
                {
                    case OpCode.Nop:
                        break;
                    case OpCode.Literal:
                    case OpCode.Token:
                        var token = pe.Tokens[arg];
                        switch (token.Kind)
                        {
                            case TokenKind.RawString:
                                projector.RawString(pe.Source, token.Index, token.Length);
                                break;
                            case TokenKind.UnquotedString:
                                projector.UnquotedString(pe.Source, token.Index, token.Length);
                                break;
                            case TokenKind.QuotedString:
                                projector.QuotedString(pe.Source, token.Index, token.Length);
                                break;
                            case TokenKind.Literal:
                                projector.Literal(pe.Source, token.Index, token.Length);
                                break;
                            default:
                                throw new Exception("Internal implementation error.");
                        }
                        break;
                    case OpCode.CurrentNode: projector.CurrentNode(); break;
                    case OpCode.Not: projector.Not(); break;
                    case OpCode.And: projector.And(); break;
                    case OpCode.Or: projector.Or(); break;
                    case OpCode.GreaterThan: projector.GreaterThan(); break;
                    case OpCode.GreaterThanEqual: projector.GreaterThanEqual(); break;
                    case OpCode.LessThan: projector.LessThan(); break;
                    case OpCode.LessThanEqual: projector.LessThanEqual(); break;
                    case OpCode.Equal: projector.Equal(); break;
                    case OpCode.NotEqual: projector.NotEqual(); break;
                    case OpCode.Flatten: projector.Flatten(); break;
                    case OpCode.Field: Do(OpCode.Token, arg); projector.Field(); break;
                    case OpCode.Identity: projector.Identity(); break;
                    case OpCode.ValueProjection: projector.ValueProjection(); break;
                    case OpCode.MultiSelectList:
                    {
                        if (Pop() is {} count)
                            projector.MultiSelectList(count);
                        else
                            throw new Exception("Implementation error.");
                        break;
                    }
                    case OpCode.MultiSelectHash:
                    {
                        if (Pop() is {} count)
                            projector.MultiSelectHash(count);
                        else
                            throw new Exception("Implementation error.");
                        break;
                    }
                    case OpCode.FilterProjection: projector.FilterProjection(); break;
                    case OpCode.Projection: projector.Projection(); break;
                    case OpCode.Index:
                    {
                        if (Pop() is {} index)
                            projector.Index(index);
                        else
                            throw new Exception("Implementation error.");
                        break;
                    }
                    case OpCode.Slice:
                        projector.Slice(Pop(), Pop(), Pop());
                        break;
                    case OpCode.Reference: projector.Reference(); break;
                    case OpCode.SubExpression: projector.SubExpression(); break;
                    case OpCode.Pipe: projector.Pipe(); break;
                    case OpCode.Function:
                    {
                        if (Pop() is {} count)
                            projector.Function(count);
                        else
                            throw new Exception("Implementation error.");
                        break;
                    }
                    case OpCode.Null:
                    case OpCode.Const:
                    {
                        var n = code == OpCode.Null ? (int?)null : arg;
                        switch (r++)
                        {
                            case 0: r1 = n; break;
                            case 1: r2 = n; break;
                            case 2: r3 = n; break;
                            default: throw new Exception("Not enough registers.");
                        }
                        break;
                    }
                    case OpCode.MkRef:
                        break;
                    case OpCode.LdRef:
                        break;
                    default:
                        throw new ArgumentOutOfRangeException();
                }
            }
        }
    }
}
