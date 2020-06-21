#region Copyright (c) 2019 Atif Aziz. All rights reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
#endregion

namespace JmesPath
{
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.Collections.ObjectModel;
    using System.Linq;
    using System.Runtime.CompilerServices;
    using System.Text;
    using Parser = Gratt.Parser<ParseContext, TokenKind, Token, Precedence, Unit>;
    using PrefixParselet = System.Func<Token, Gratt.Parser<ParseContext, TokenKind, Token, Precedence, Unit>, Unit>;
    using InfixParselet = System.Func<Token, Unit, Gratt.Parser<ParseContext, TokenKind, Token, Precedence, Unit>, Unit>;

    partial struct Unit {}
    /*
    partial struct TokenOrCode
    {
        public readonly Token Token;
        public readonly OpCode OpCode;

        public TokenOrCode(Token token)
        {
            Token = token;
            OpCode = OpCode.Token;
        }

        public TokenOrCode(OpCode op)
        {
            Token = default;
            OpCode = op;
        }

        public static implicit operator TokenOrCode(Token token) => new TokenOrCode(token);
        public static implicit operator TokenOrCode(OpCode op) => new TokenOrCode(op);
    }
    */
    public enum OpCode
    {
        Nop,
        Token,
        Current,
        And,
        Or,
        Not,
        LessThan,
        Flatten,
        Field,
        Literal,
        Identity,
        ValueProjection,
        MultiSelectList,
        MultiSelectHash,
        FilterProjection,
        Projection,
        Index,
        Null,
        Slice,
        Reference,
        SubExpression,
        Pipe,
        Function,
        GreaterThanEqual,
        LessThanEqual,
        GreaterThan,
        NotEqual,
        Equal,
        Const,
        MkRef,
        LdRef,
    }

    //
    // This is an implementation of a C# pre-processing expression parser found in conditional
    // directives:
    //
    // https://github.com/dotnet/csharplang/blob/892af9016b3317a8fae12d195014dc38ba51cf16/spec/lexical-structure.md#pre-processing-expressions
    //
    // It uses Pratt parser to parse and evaluate the expression immediately as opposed to returning
    // an abstract syntax tree (AST) of the expression.
    //

    static partial class Expression
    {
        public static ParsedExpression Parse(string expression)
        {
            var context = new ParseContext(expression);
            Evaluate(expression, context);
            return new ParsedExpression(expression,
                                        new ReadOnlyCollection<Token>(context.Tokens),
                                        new ReadOnlyCollection<Op>(context.Ops));
        }

        static void Evaluate(string expression, ParseContext context) =>
            Gratt.Parser.Parse(
                context,
                Precedence.Default,
                TokenKind.Eoi, t => new SyntaxErrorException($"Unexpected token <{t.Kind}> at offset {t.Index}."),
                (_, token, __) => Spec.Instance.Prefix(token),
                (kind, _, __) => Spec.Instance.Infix(kind),
                from t in Scanner.Scan(expression, ScanOptions.IgnoreWhiteSpace)
                select (t.Kind, t));
    }

    sealed partial class ParsedExpression
    {
        public string Source { get; }
        public IReadOnlyList<Token> Tokens { get; }
        public IReadOnlyCollection<Op> Ops { get; }

        public ParsedExpression(string source, IReadOnlyList<Token> tokens,
                                               IReadOnlyCollection<Op> ops)
        {
            Source = source;
            Tokens = tokens;
            Ops = ops;
        }

        public override string ToString() => ToString(null);

        public string ToString(string eol)
        {
            var sb = new StringBuilder();

            foreach (var (level, code, arg) in Ops)
            {
                sb.Append(level).Append(": ").Append(code);
                switch (code)
                {
                    case OpCode.Field:
                    case OpCode.Literal:
                    case OpCode.Token:
                        var token = Tokens[arg];
                        sb.Append(' ').Append(Source, token.Index, token.Length);
                        break;
                    case OpCode.Const:
                        sb.Append(' ').Append(arg);
                        break;
                }

                if (eol is null)
                    sb.AppendLine();
                else
                    sb.Append(eol);
            }

            return sb.ToString();
        }
    }

    readonly partial struct Op
    {
        public readonly int Level;
        public readonly OpCode Code;
        public readonly int Arg;

        public Op(int level, OpCode code, int arg = 0) =>
            (Level, Code, Arg) = (level, code, arg);
        public override string ToString() => $"{Level}: {Code} ({Arg})";

        public void Deconstruct(out int level, out OpCode code, out int arg) =>
            (level, code, arg) = (Level, Code, Arg);
    }

    sealed class ParseContext
    {
        public string SourceText { get; }

        public ParseContext(string sourceText) =>
            SourceText = sourceText;

        public List<Token> Tokens { get; } = new List<Token>();
        public List<Op> Ops { get; } = new List<Op>();

        public int Level { get; private set; }

        public void Push() => Level++;
        public void Pop() => Level = Level - 1 is {} n ? n : throw new InvalidOperationException();

        public Op PeekOp(int index = 0) => Ops[index < 0 ? Ops.Count + index : index];

        public int Emit(Token token) { Tokens.Add(token); return Tokens.Count - 1; }
        public void Emit(OpCode code, int arg = 0) => Ops.Add(new Op(Level, code, arg));
    }

    static class TokenKindPrecedence
    {
        public static Precedence Map(TokenKind kind) => PrecedenceByTokenKind[(int)kind];

        static readonly Precedence[] PrecedenceByTokenKind =
        {
            // IMPORTANT! Keep the following list synchronized and in
            // the same order as TokenKind enum.

            Precedence.Default,    // Eoi
            Precedence.Default,    // WhiteSpace
            Precedence.Default,    // At
            Precedence.Default,    // Number
            Precedence.Star   ,    // Star
            Precedence.Dot    ,    // Dot
            Precedence.Default,    // Comma
            Precedence.Default,    // Colon
            Precedence.Default,    // Ampersand
            Precedence.LogicalAnd, // AmpersandAmpersand
            Precedence.Pipe,       // Pipe
            Precedence.LogicalOr,  // PipePipe
            Precedence.LParen,     // LParen
            Precedence.Default,    // RParen
            Precedence.LBrace,     // LBrace
            Precedence.Default,    // RBrace
            Precedence.LBracket,   // LBracket
            Precedence.Default,    // RBracket
            Precedence.Flatten,    // LRBracket
            Precedence.Filter,     // LBracketQuestion
            Precedence.Not,        // Bang
            Precedence.Relational, // GreaterThan
            Precedence.Relational, // GreaterThanEqual
            Precedence.Relational, // LessThan
            Precedence.Relational, // LessThanEqual
            Precedence.Relational, // EqualEqual
            Precedence.Relational, // BangEqual
            Precedence.Default,    // RawString
            Precedence.Default,    // UnquotedString
            Precedence.Default,    // QuotedString
            Precedence.Default,    // Literal
        };
    }

    public enum TokenKind
    {
        Eoi,
        WhiteSpace,         // *(%x20 / %x09 / %x0A / %x0D)
        At,                 // "@"
        Number,             // number = ["-"]1*digit
                            // digit  = % x30 - 39
        Star,               // "*"
        Dot,                // "."
        Comma,              // ","
        Colon,              // ":"
        Ampersand,          // "&"
        AmpersandAmpersand, // "&&"
        Pipe,               // "|"
        PipePipe,           // "||"
        LParen,             // "("
        RParen,             // ")"
        LBrace,             // "{"
        RBrace,             // "}"
        LBracket,           // "["
        RBracket,           // "]"
        LRBracket,          // "[]"
        LBracketQuestion,   // "[?"
        Bang,               // "!"
        GreaterThan,        // ">"
        GreaterThanEqual,   // ">="
        LessThan,           // "<"
        LessThanEqual,      // "<"
        EqualEqual,         // "=="
        BangEqual,          // "!="
        RawString,          // raw-string        = "'" *raw-string-char "'"
                            // raw-string-char   = (%x20-26 / %x28-5B / %x5D-10FFFF) / preserved-escape /
                            //                       raw-string-escape
                            // preserved-escape  = escape (%x20-26 / %28-5B / %x5D-10FFFF)
                            // raw-string-escape = escape ("'" / escape)
        UnquotedString,     // unquoted-string   = (%x41-5A / %x61-7A / %x5F) *(  ; A-Za-z_
                            //                         %x30-39  /  ; 0-9
                            //                         %x41-5A /  ; A-Z
                            //                         %x5F    /  ; _
                            //                         %x61-7A)   ; a-z
        QuotedString,       // quoted-string     = quote 1*(unescaped-char / escaped-char) quote
                            // unescaped-char    = %x20-21 / %x23-5B / %x5D-10FFFF
                            // escape            = %x5C   ; Back slash: \
                            // quote             = %x22   ; Double quote: '"'
                            // escaped-char      = escape (
                            //                         %x22 /          ; "    quotation mark  U+0022
                            //                         %x5C /          ; \    reverse solidus U+005C
                            //                         %x2F /          ; /    solidus         U+002F
                            //                         %x62 /          ; b    backspace       U+0008
                            //                         %x66 /          ; f    form feed       U+000C
                            //                         %x6E /          ; n    line feed       U+000A
                            //                         %x72 /          ; r    carriage return U+000D
                            //                         %x74 /          ; t    tab             U+0009
                            //                         %x75 4HEXDIG )  ; uXXXX                U+XXXX
        Literal,            // literal           = "`" json-value "`"
                            //                   ; The ``json-value`` is any valid JSON value with the one
                            //                   ; exception that the ``%x60`` character must be escaped.
    }

    readonly partial struct Token : IEquatable<Token>
    {
        public readonly TokenKind Kind;
        public readonly int Index;
        public readonly int Length;

        public Token(TokenKind kind, int index, int length) =>
            (Kind, Index, Length) = (kind, index, length);

        public bool Equals(Token other) =>
            Kind == other.Kind && Index == other.Index && Length == other.Length;

        public override bool Equals(object obj) =>
            obj is Token other && Equals(other);

        public override int GetHashCode() =>
            unchecked(((((int)Kind * 397) ^ Index.GetHashCode()) * 397) ^ Length.GetHashCode());

        public static bool operator ==(Token left, Token right) => left.Equals(right);
        public static bool operator !=(Token left, Token right) => !left.Equals(right);

        public override string ToString() =>
            $"{Kind} [{Index}..{Index + Length})";

        public string Substring(string source) =>
            source.Substring(Index, Length);
    }

    enum Precedence
    {
        Default        = 0,
        ExpRef         = Default,
        Pipe           = 1, // |
        LogicalOr      = 2, // ||
        LogicalAnd     = 3, // &&
        Relational     = 5, // > >= < <= == !=
        Flatten        = 9,
        // Everything above stops a projection.
        ProjectionStop = 10,
        Star           = 20,
        Filter         = 21,
        Dot            = 40,
        Prefix         = 45, // !
        Not            = Prefix,
        LBrace         = 50,
        LBracket       = 55,
        LParen         = 60,
    }

    partial class SyntaxErrorException : Exception
    {
        public SyntaxErrorException() {}
        public SyntaxErrorException(string message) : base(message) {}
        public SyntaxErrorException(string message, Exception inner) : base(message, inner) {}
    }

    static class X
    {
        public static void Emit(this Parser parser, OpCode code, int arg = 0) =>
            parser.State.Emit(code, arg);

        public static void Emit(this Parser parser, OpCode code, Token token)
        {
            var i = parser.State.Emit(token);
            parser.Emit(code, i);
        }

        public static void Parse(this Parser parser) =>
            parser.Parse(Precedence.Default);

        public static Token Read(this Parser parser, TokenKind kind) =>
            parser.Read(kind, (TokenKind expected, (TokenKind, Token Token) actual) =>
                            new SyntaxErrorException($"Expected <{expected}> token at offset {actual.Token.Index}, but got: <{actual.Token.Kind}>"));
    }

    sealed class Spec : IEnumerable
    {
        public static readonly Spec Instance = new Spec
        {
            // nud
            { TokenKind.Literal           , (t, p) => p.Emit(OpCode.Literal, t) },
            { TokenKind.RawString         , (t, p) => p.Emit(OpCode.Literal, t) },
            { TokenKind.UnquotedString    , (t, p) => p.Emit(OpCode.Field, t) },
            {
                TokenKind.QuotedString, (t, p) =>
                {
                    p.Emit(OpCode.Field, t);
                    if (p.Peek() is (TokenKind.LParen, var bt))
                        throw new SyntaxErrorException($"Quoted identifier not allowed for function names (see offset {bt.Index}).");
                }
            },
            { TokenKind.Star            , (t, p) => NudStar(p) },
            { TokenKind.LBracketQuestion, (t, p) => TokenLedFilter(p) },
            { TokenKind.LBrace          , (t, p) => ParseMultiSelectHash(p) },
            { TokenKind.LParen          , (t, p) => { p.Parse(); p.Read(TokenKind.RParen); } },
            {
                TokenKind.LRBracket, (t, p) =>
                {
                    p.Emit(OpCode.Identity);
                    p.Emit(OpCode.Flatten); // ...left
                    ParseProjectionRhs(p, Precedence.Flatten); // right
                    p.Emit(OpCode.Projection);
                }
            },
            { TokenKind.Bang, (_, p) => { p.Parse(Precedence.Prefix); p.Emit(OpCode.Not); }},
            {
                TokenKind.LBracket, (_, p) =>
                {
                    switch (p.Peek())
                    {
                        case (TokenKind.Number, _):
                        case (TokenKind.Colon, _):
                            var ios = ParseIndexExpression(p); // ...right
                            // We could optimize this and remove the identity() node.
                            // We don't really need an index_expression node, we can
                            // just use emit an index node here if we're not dealing
                            // with a slice.
                            p.Emit(OpCode.Identity);
                            ProjectIfSlice(p, ios);
                            break;
                        case (TokenKind.Star, _):
                            p.Read();
                            if (p.Peek() is (TokenKind.RBracket, _))
                            {
                                p.Read();
                                p.Emit(OpCode.Identity); // left
                                ParseProjectionRhs(p, Precedence.Star); // right
                                p.Emit(OpCode.Projection);
                            }
                            else
                            {
                                NudStar(p); // [ *, ... ]
                                p.Read(TokenKind.Comma);
                                ParseMultiSelectList(p, 2);
                            }
                            break;
                        default:
                            ParseMultiSelectList(p);
                            break;
                    }
                }
            },
            { TokenKind.At       , (_, p) => p.Emit(OpCode.Current) },
            { TokenKind.Ampersand, (_, p) => { p.Parse(Precedence.ExpRef); p.Emit(OpCode.Reference); } },
            // led
            {
                TokenKind.Dot, Precedence.Dot, (rbp, p) =>
                {   // ...left
                    if (p.Match(TokenKind.Star)) // We're creating a projection
                    {
                        ParseProjectionRhs(p, rbp); // right
                        p.Emit(OpCode.ValueProjection);
                    }
                    else
                    {
                        ParseDotRhs(p, rbp); // right
                        p.Emit(OpCode.SubExpression);
                    }
                }
            },
            { TokenKind.Pipe              , Precedence.Pipe      , (rbp, p) => { p.Parse(rbp); p.Emit(OpCode.Pipe); } },
            { TokenKind.PipePipe          , Precedence.LogicalOr , (rbp, p) => { p.Parse(rbp); p.Emit(OpCode.Or); } },
            { TokenKind.AmpersandAmpersand, Precedence.LogicalAnd, (rbp, p) => { p.Parse(rbp); p.Emit(OpCode.And); } },
            {
                TokenKind.LParen, Precedence.LParen, (rbp, p) =>
                {   // ...left
                    if (!(p.State.PeekOp(-1) is {} left) || left.Code != OpCode.Field)
                    {
                        //  0 - first func arg or closing paren.
                        // -1 - '(' token
                        // -2 - invalid function "name".
                        var i = p.State.PeekOp(-2).Arg;
                        var token = p.State.Tokens[i];
                        throw new SyntaxErrorException($"Invalid function name at offset {token.Index}: {token.Substring(p.State.SourceText)}");
                    }

                    var count = 0;
                    while (!p.Match(TokenKind.RParen))
                    {
                        next:
                        p.Parse();
                        count++;
                        if (p.Match(TokenKind.Comma))
                            goto next;
                    }
                    p.Emit(OpCode.Const, count);
                    p.Emit(OpCode.Function);
                }
            },
            { TokenKind.LBracketQuestion, Precedence.Filter    , (rbp, p) => TokenLedFilter(p, rbp) },
            { TokenKind.EqualEqual      , Precedence.Relational, (rbp, p) => ParseComparator(p, rbp, OpCode.Equal) },
            { TokenKind.BangEqual       , Precedence.Relational, (rbp, p) => ParseComparator(p, rbp, OpCode.NotEqual) },
            { TokenKind.GreaterThan     , Precedence.Relational, (rbp, p) => ParseComparator(p, rbp, OpCode.GreaterThan) },
            { TokenKind.GreaterThanEqual, Precedence.Relational, (rbp, p) => ParseComparator(p, rbp, OpCode.GreaterThanEqual) },
            { TokenKind.LessThan        , Precedence.Relational, (rbp, p) => ParseComparator(p, rbp, OpCode.LessThan) },
            { TokenKind.LessThanEqual   , Precedence.Relational, (rbp, p) => ParseComparator(p, rbp, OpCode.LessThanEqual) },
            {
                TokenKind.LRBracket, Precedence.Flatten, (rbp, p) =>
                {   // ...left
                    p.Emit(OpCode.Flatten);
                    ParseProjectionRhs(p, rbp); // right
                    p.Emit(OpCode.Projection);
                }
            },
            {
                TokenKind.LBracket, Precedence.LBracket, (rbp, p) =>
                {   // ...left
                    switch (p.Peek())
                    {
                        case (TokenKind.Number, _):
                        case (TokenKind.Colon, _):
                            var ios = ParseIndexExpression(p); // right
                            ProjectIfSlice(p, ios);
                            break;
                        default: // We have a projection
                            p.Read(TokenKind.Star);
                            p.Read(TokenKind.RBracket);
                            ParseProjectionRhs(p, Precedence.Star); // right
                            p.Emit(OpCode.Projection);
                            break;
                    }
                }
            },
        };

        static void ParseComparator(Parser parser, Precedence rbp, OpCode op)
        {
            // ...left
            parser.Parse(rbp); // right
            parser.Emit(op);
        }

        static void NudStar(Parser parser)
        {
            parser.Emit(OpCode.Identity); // left
            if (parser.Match(TokenKind.RBracket))
                parser.Emit(OpCode.Identity); // right
            else
                ParseProjectionRhs(parser, Precedence.Star); // right
            parser.Emit(OpCode.ValueProjection);
        }

        static void ProjectIfSlice(Parser parser, IndexOrSlice ios)
        {
            parser.Emit(OpCode.Index);
            if (ios == IndexOrSlice.Slice)
            {
                ParseProjectionRhs(parser, Precedence.Star);
                parser.Emit(OpCode.Projection);
            }
        }

        enum IndexOrSlice { Index, Slice }

        static IndexOrSlice ParseIndexExpression(Parser parser)
        {
            // We're here:
            // [<current>
            //  ^
            //  | current token
            if (parser.Peek() is (TokenKind.Colon, _))
            {
                ParseSliceExpression(parser);
                return IndexOrSlice.Slice;
            }
            else // Parse the syntax [number]
            {
                var number = parser.Read(TokenKind.Number);
                if (parser.Peek() is (TokenKind.Colon, _))
                {
                    ParseSliceExpression(parser, 1);
                    return IndexOrSlice.Slice;
                }
                else
                {
                    var i = NumberToInt32(parser.State.SourceText, number.Index, number.Length);
                    parser.Emit(OpCode.Const, i);
                    parser.Read(TokenKind.RBracket);
                    return IndexOrSlice.Index;
                }
            }
        }

        static void ParseSliceExpression(Parser parser, int index = 0)
        {
            // [start:end:step]
            // Where start, end, and step are optional.
            // The last colon is optional as well.
            while (!parser.Match(TokenKind.RBracket) && index < 3)
            {
                switch (parser.Peek())
                {
                    case (TokenKind.Colon, var colon):
                        index++;
                        if (index == 3)
                            throw new SyntaxErrorException($"Too many slice arguments at offset {colon.Index}.");
                        parser.Read();
                        parser.Emit(OpCode.Null);
                        break;
                    case (TokenKind.Number, var token):
                        parser.Read();
                        var n = NumberToInt32(parser.State.SourceText, token.Index, token.Length);
                        parser.Emit(OpCode.Const, n);
                        break;
                    case var (_, token):
                        throw new SyntaxErrorException($"Unexpected token <{token.Kind}> at offset {token.Index}.");
                }
            }
            parser.Emit(OpCode.Slice);

        }

        static int NumberToInt32(string s, int i, int len)
        {
            var ei = i + len;
            var neg = false;
            if (s[i] == '-')
            {
                i++;
                neg = true;
            }
            var n = 0;
            checked
            {
                for (; i < ei; i++)
                    n = n * 10 + (s[i] - '0');
            }
            return neg ? -n : n;
        }

        static void TokenLedFilter(Parser parser) =>
            TokenLedFilter(parser, Precedence.Filter);

        static void TokenLedFilter(Parser parser, Precedence rbp)
        {
            // Filters are projections.
            // ...left
            parser.Emit(OpCode.MkRef);
            parser.State.Push();
            parser.Parse(); // condition
            parser.State.Pop();
            parser.Emit(OpCode.LdRef);
            parser.Read(TokenKind.RBracket);
            // right...
            if (parser.Peek() is (TokenKind.LRBracket, _))
            {
                parser.Emit(OpCode.Identity);
            }
            else
            {
                parser.Emit(OpCode.MkRef);
                ParseProjectionRhs(parser, rbp);
                parser.Emit(OpCode.LdRef);
            }
            parser.Emit(OpCode.FilterProjection);
        }

        static void ParseProjectionRhs(Parser parser, Precedence bp)
        {
            switch (parser.Peek())
            {
                case var (k, _) when TokenKindPrecedence.Map(k) < Precedence.ProjectionStop:
                    // BP of 10 are all the tokens that stop a projection.
                    parser.Emit(OpCode.Identity);
                    break;
                case (TokenKind.LBracket, _):
                case (TokenKind.LBracketQuestion, _):
                    parser.Parse(bp);
                    break;
                case (TokenKind.Dot, _):
                    parser.Read();
                    ParseDotRhs(parser, bp);
                    break;
                case var (_, token):
                    throw new SyntaxErrorException($"Unexpected token <{token.Kind}> at offset {token.Index}.");
            }
        }

        static void ParseDotRhs(Parser parser, Precedence bp)
        {
            // From the grammar:
            // expression '.' ( identifier /
            //                  multi-select-list /
            //                  multi-select-hash /
            //                  function-expression /
            //                  *
            // In terms of tokens that means that after a '.',
            // you can have:
            var (_, token) = parser.Peek();
            switch (token.Kind)
            {
                // Common case "foo.bar", so first check for an identifier.
                case TokenKind.QuotedString:
                case TokenKind.UnquotedString:
                case TokenKind.Star:
                    parser.Parse(bp);
                    break;
                case TokenKind.LBracket:
                    parser.Read();
                    ParseMultiSelectList(parser);
                    break;
                case TokenKind.LBrace:
                    parser.Read();
                    ParseMultiSelectHash(parser);
                    break;
                default:
                    throw new SyntaxErrorException($"Unexpected token <{token.Kind}> at offset {token.Index}.");
            }
        }

        static void ParseMultiSelectList(Parser parser, int count = 1)
        {
            for (;; count++)
            {
                parser.Parse();
                if (parser.Match(TokenKind.RBracket))
                    break;
                parser.Read(TokenKind.Comma, (TokenKind _, (TokenKind, Token Token) a) =>
                                new SyntaxErrorException($"Expecting comma at offset {a.Token.Index}, but got: <{a.Token.Kind}>"));
            }
            parser.Emit(OpCode.Const, count);
            parser.Emit(OpCode.MultiSelectList);
        }

        static void ParseMultiSelectHash(Parser parser)
        {
            for (var count = 1; ; count++)
            {
                var (_, token) = parser.Peek();
                // Before getting the token value, verify it's
                // an identifier.
                if (!parser.Match(TokenKind.QuotedString) &&
                    !parser.Match(TokenKind.UnquotedString))
                {
                    throw new SyntaxErrorException(/* TODO */);
                }

                parser.Emit(OpCode.Token, token);
                parser.Read(TokenKind.Colon);
                parser.Parse();
                // TODO? node = ast.key_val_pair(key_name=key_name, node=value)
                if (parser.Match(TokenKind.RBrace))
                {
                    parser.Emit(OpCode.Const, count);
                    parser.Emit(OpCode.MultiSelectHash);
                    break;
                }

                parser.Read(TokenKind.Comma, (TokenKind _, (TokenKind, Token Token) a) =>
                                new SyntaxErrorException($"Expecting comma at offset {a.Token.Index}, but got: <{a.Token.Kind}>"));
            }
        }

        Spec() { }

        readonly Dictionary<TokenKind, PrefixParselet> _prefixes = new Dictionary<TokenKind, PrefixParselet>();
        readonly Dictionary<TokenKind, (Precedence, InfixParselet)> _infixes = new Dictionary<TokenKind, (Precedence, InfixParselet)>();

        void Add(TokenKind type, Action<Token, Parser> prefix) =>
            _prefixes.Add(type, (t, p) =>
            {
                //p.State.Push();
                prefix(t, p);
                //p.State.Pop();
                return default;
            });

        void Add(TokenKind type, Precedence precedence, Action<Precedence, Parser> infix) =>
            _infixes.Add(type, (precedence, (t, _, p) =>
            {
                //p.State.Push();
                infix(precedence, p);
                //p.State.Pop();
                return default;
            }));
        /*
        void Add(TokenKind type, Precedence precedence, Func<Unit, Unit, ParseContext, Unit> f) =>
            Add(type, precedence, (token, left, parser) => f(left, parser.Parse(precedence), parser.State));

        void Add(TokenKind type, Precedence precedence, Func<Unit, Unit, Unit> f) =>
            Add(type, precedence, (token, left, parser) => f(left, parser.Parse(precedence)));

        void Add(TokenKind type, Precedence precedence, Func<Unit, Precedence, Parser, Unit> f) =>
            Add(type, precedence, (token, left, parser) => f(left, precedence, parser));
        */
        public PrefixParselet Prefix(Token token)
            => _prefixes.TryGetValue(token.Kind, out var v) ? v
             : throw new SyntaxErrorException($"Unexpected <{token.Kind}> token at offset {token.Index}.");

        public (Precedence, InfixParselet)? Infix(TokenKind type) =>
            _infixes.TryGetValue(type, out var v) ? ((Precedence, InfixParselet)?)v : null;

        IEnumerator IEnumerable.GetEnumerator() =>
            _prefixes.Cast<object>().Concat(_infixes.Cast<object>()).GetEnumerator();
    }

    [Flags]
    public enum ScanOptions
    {
        None,
        IgnoreWhiteSpace,
        NoEoiToken,
    }

    static partial class Scanner
    {
        public static IEnumerable<Token> Scan(string input) =>
            Scan(input, ScanOptions.None);

        public static IEnumerable<Token> Scan(string input, ScanOptions options)
        {
            if (input == null) throw new ArgumentNullException(nameof(input));
            return ScanImpl(input, options);
        }

        enum State
        {
            Scan,
            RawString,
            RawStringSlash,
            QuotedString,
            QuotedStringSlash,
            QuotedStringHex,
            UnquotedString,
            Literal,
            LiteralSlash,
            Ampersand,
            Pipe,  // (|)   
            Equal,    
            Bang,  // (!)   
            WhiteSpace,
            NegativeNumber,
            Number,
            LeftBracket,
            GreaterThan,
            LessThan,
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        static bool IsDigit(char ch) => ch >= '0' && ch <= '9';

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        static bool IsUnquotedStringChar(char ch)
            => ch >= 'A' && ch <= 'Z'
            || ch >= 'a' && ch <= 'z'
            || IsDigit(ch)
            || ch == '_';

        static IEnumerable<Token> ScanImpl(string s, ScanOptions options)
        {
            var ignoreWhiteSpace = (options & ScanOptions.IgnoreWhiteSpace) != 0;

            var si = 0;
            var hi = 0;
            var resetState = false;

            Token Token(TokenKind kind, int len)
            {
                resetState = true;
                return new Token(kind, si, len);
            }

            var state = State.Scan;
            var i = 0;
            for (; i < s.Length; i++)
            {
                var ch = s[i];
            restart:
                if (resetState)
                    (state, resetState) = (State.Scan, false);
                switch (state)
                {
                    case State.Scan:
                    {
                        si = i;
                        switch (ch)
                        {
                            case ' ' : case '\t': case '\r': case '\n': state = State.WhiteSpace; break;
                            case '&' : state = State.Ampersand; break;
                            case '|' : state = State.Pipe; break;
                            case '!' : state = State.Bang; break;
                            case '>' : state = State.GreaterThan; break;
                            case '<' : state = State.LessThan; break;
                            case '=' : state = State.Equal; break;
                            case '\'': state = State.RawString; break;
                            case '\"': state = State.QuotedString; break;
                            case '`' : state = State.Literal; break;
                            case '*' : yield return Token(TokenKind.Star, 1); break;
                            case '.' : yield return Token(TokenKind.Dot, 1); break;
                            case ',' : yield return Token(TokenKind.Comma, 1); break;
                            case ':' : yield return Token(TokenKind.Colon, 1); break;
                            case '@' : yield return Token(TokenKind.At, 1); break;
                            case '(' : yield return Token(TokenKind.LParen, 1); break;
                            case ')' : yield return Token(TokenKind.RParen, 1); break;
                            case '{' : yield return Token(TokenKind.LBrace, 1); break;
                            case '}' : yield return Token(TokenKind.RBrace, 1); break;
                            case '[' : state = State.LeftBracket; break;
                            case ']' : yield return Token(TokenKind.RBracket, 1); break;
                            case '-' : state = State.NegativeNumber; break;
                            case {} when IsDigit(ch): state = State.Number; break;
                            case {} when IsUnquotedStringChar(ch): state = State.UnquotedString; break;
                            default:
                                throw new SyntaxErrorException($"Unexpected at offset {i}: {ch}");
                        }
                        break;
                    }
                    case State.UnquotedString:
                    {
                        if (IsUnquotedStringChar(ch))
                            break;
                        yield return Token(TokenKind.UnquotedString, i - si);
                        goto restart;
                    }
                    case State.Literal:
                    {
                        switch (ch)
                        {
                            case '\\': state = State.LiteralSlash; break;
                            case '`': yield return Token(TokenKind.Literal, i + 1 - si); break;
                        }
                        break;
                    }
                    case State.LiteralSlash:
                    {
                        state = State.Literal;
                        break;
                    }
                    case State.RawString:
                    {
                        switch (ch)
                        {
                            case '\\': state = State.RawStringSlash; break;
                            case '\'': yield return Token(TokenKind.RawString, i + 1 - si); break;
                        }
                        break;
                    }
                    case State.RawStringSlash:
                    {
                        state = State.RawString;
                        break;
                    }
                    case State.QuotedString:
                    {
                        switch (ch)
                        {
                            case '\\': state = State.QuotedStringSlash; break;
                            case '\"': yield return Token(TokenKind.QuotedString, i + 1 - si); break;
                            default:
                                if (ch < 20)
                                    throw new SyntaxErrorException($"Invalid string character at offset {i}.");
                                break;
                        }
                        break;
                    }
                    case State.QuotedStringSlash:
                    {
                        switch (ch)
                        {
                            case 'b':
                            case 'f':
                            case 'n':
                            case 'r':
                            case 't':
                            case '/':
                            case '"':
                            case '\\':
                                state = State.QuotedString;
                                break;
                            case 'u':
                                hi = i;
                                state = State.QuotedStringHex;
                                break;
                            default:
                                throw new SyntaxErrorException($"Invalid escape character at offset {i}.");
                        }
                        break;
                    }
                    case State.QuotedStringHex:
                    {
                        if (i - hi > 4)
                        {
                            state = State.QuotedString;
                            goto restart;
                        }

                        if (ch >= '0' && ch <= '9' || ch >= 'A' && ch <= 'F' || ch >= 'a' && ch <= 'f')
                            break;
                        throw new SyntaxErrorException($"Invalid escape hexadecimal digit at offset {i}.");
                    }
                    case State.LeftBracket:
                    {
                        switch (ch)
                        {
                            case '?': yield return Token(TokenKind.LBracketQuestion, 2); break;
                            case ']': yield return Token(TokenKind.LRBracket, 2); break;
                            default:
                                yield return Token(TokenKind.LBracket, 1);
                                goto restart;
                        }
                        break;
                    }
                    case State.NegativeNumber:
                    {
                        if (IsDigit(ch))
                            state = State.Number;
                        else
                            throw new SyntaxErrorException($"Expected digit (0-9) at offset {i}, but got: {ch}");
                        break;
                    }
                    case State.Number:
                    {
                        if (IsDigit(ch))
                            break;
                        yield return Token(TokenKind.Number, i - si);
                        goto restart;
                    }
                    case State.WhiteSpace:
                    {
                        if (ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n')
                            break;
                        if (ignoreWhiteSpace)
                            state = State.Scan;
                        else
                            yield return Token(TokenKind.WhiteSpace, i - si);
                        goto restart;
                    }
                    case State.GreaterThan:
                    {
                        if (ch == '=')
                        {
                            yield return Token(TokenKind.GreaterThanEqual, 2);
                            break;
                        }
                        yield return Token(TokenKind.GreaterThan, 1);
                        goto restart;
                    }
                    case State.LessThan:
                    {
                        if (ch == '=')
                        {
                            yield return Token(TokenKind.LessThanEqual, 2);
                            break;
                        }
                        yield return Token(TokenKind.LessThan, 1);
                        goto restart;
                    }
                    case State.Ampersand:
                    {
                        if (ch == '&')
                        {
                            yield return Token(TokenKind.AmpersandAmpersand, 2);
                            break;
                        }
                        yield return Token(TokenKind.Ampersand, 1);
                        goto restart;
                    }
                    case State.Pipe:
                    {
                        if (ch == '|')
                        {
                            yield return Token(TokenKind.PipePipe, 2);
                            break;
                        }
                        yield return Token(TokenKind.Pipe, 1);
                        goto restart;
                    }
                    case State.Equal:
                    {
                        if (ch != '=')
                            throw new SyntaxErrorException($"Unexpected at offset {i}: {ch}");
                        yield return Token(TokenKind.EqualEqual, 2);
                        break;
                    }
                    case State.Bang:
                    {
                        if (ch == '=')
                        {
                            yield return Token(TokenKind.BangEqual, 2);
                            break;
                        }
                        yield return Token(TokenKind.Bang, 1);
                        goto restart;
                    }
                    default:
                        throw new Exception("Internal error due to unhandled state: " + state);
                }
            }

            if (state == State.Scan || resetState)
                goto eoi;

            TokenKind kind;

            switch (state)
            {
                case State.WhiteSpace:
                    if (ignoreWhiteSpace)
                        goto eoi;
                    kind = TokenKind.WhiteSpace;
                    break;
                case State.Number           : kind = TokenKind.Number        ; break;
                case State.GreaterThan      : kind = TokenKind.GreaterThan   ; break;
                case State.LessThan         : kind = TokenKind.LessThan      ; break;
                case State.Bang             : kind = TokenKind.Bang          ; break;
                case State.Ampersand        : kind = TokenKind.Ampersand     ; break;
                case State.Pipe             : kind = TokenKind.Pipe          ; break;
                case State.UnquotedString   : kind = TokenKind.UnquotedString; break;
                case State.LeftBracket      : kind = TokenKind.LBracket      ; break;
                case State.RawString        :
                case State.RawStringSlash   :
                case State.QuotedString     :
                case State.QuotedStringSlash:
                case State.QuotedStringHex  :
                    throw new SyntaxErrorException($"Unexpected end of input at offset {i}; unterminated string.");
                case State.Literal:
                case State.LiteralSlash:
                    throw new SyntaxErrorException($"Unexpected end of input at offset {i}; unterminated literal.");
                case State.NegativeNumber:
                    throw new SyntaxErrorException($"Unexpected end of input at offset {i}; missing number digit(s).");
                case State.Equal:
                    throw new SyntaxErrorException($"Unexpected end of input at offset {i}.");
                default:
                    throw new Exception("Internal error due to unhandled state: " + state);
            }

            yield return Token(kind, i - si);

        eoi:
            if ((options & ScanOptions.NoEoiToken) == 0)
                yield return new Token(TokenKind.Eoi, i, 0);
        }
    }
}
