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
    using System.Linq;
    using System.Runtime.CompilerServices;
    using Parser = Gratt.Parser<ParseContext, TokenKind, Token, Precedence, bool>;
    using PrefixParselet = System.Func<Token, Gratt.Parser<ParseContext, TokenKind, Token, Precedence, bool>, bool>;
    using InfixParselet = System.Func<Token, bool, Gratt.Parser<ParseContext, TokenKind, Token, Precedence, bool>, bool>;

    //
    // This is an implementation of a C# pre-processing expression parser found in conditional
    // directives:
    //
    // https://github.com/dotnet/csharplang/blob/892af9016b3317a8fae12d195014dc38ba51cf16/spec/lexical-structure.md#pre-processing-expressions
    //
    // It uses Pratt parser to parse and evaluate the expression immediately as opposed to returning
    // an abstract syntax tree (AST) of the expression.
    //

    static class PreprocessorExpression
    {
        public static bool Evaluate(string expression, Func<string, bool> symbolPredicate) =>
            Gratt.Parser.Parse(
                new ParseContext(expression, symbolPredicate),
                Precedence.Default,
                TokenKind.Eoi, t => new SyntaxErrorException($"Unexpected token <{t.Kind}> at offset <{t.Index}>."),
                (_, token, __) => Spec.Instance.Prefix(token),
                (kind, _, __) => Spec.Instance.Infix(kind),
                from t in Scanner.Scan(expression)
                where t.Kind != TokenKind.WhiteSpace
                select (t.Kind, t));
    }

    sealed class ParseContext
    {
        public string SourceText { get; }
        public Func<string, bool> SymbolPredicate { get; }

        public ParseContext(string sourceText, Func<string, bool> symbolPredicate)
        {
            SourceText = sourceText;
            SymbolPredicate = symbolPredicate;
        }
    }

    public enum TokenKind
    {
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
        Eoi,
    }

    readonly partial struct Token : IEquatable<Token>
    {
        public readonly TokenKind Kind;
        public readonly int Index;
        public readonly int Length;

        public Token(TokenKind kind, int index, int length) =>
            (Kind, Index, Length) = (kind, index, length);

        public bool Equals(Token other) =>
            Kind == other.Kind
            && Index.Equals(other.Index)
            && Length.Equals(other.Length);

        public override bool Equals(object obj) =>
            obj is Token other && Equals(other);

        public override int GetHashCode() =>
            unchecked(((((int)Kind * 397) ^ Index.GetHashCode()) * 397) ^ Length.GetHashCode());

        public static bool operator ==(Token left, Token right) => left.Equals(right);
        public static bool operator !=(Token left, Token right) => !left.Equals(right);

        public override string ToString() =>
            $"{Kind} [{Index}..{Length})";

        public string Substring(string source) =>
            source.Substring(Index, Length);
    }

    enum Precedence
    {
        Default    = 0,
        LogicalOr  = 10, // ||
        LogicalAnd = 15, // &&
        Relational = 20, // == !=
        Prefix     = 30, // !
    }

    partial class SyntaxErrorException : Exception
    {
        public SyntaxErrorException() {}
        public SyntaxErrorException(string message) : base(message) {}
        public SyntaxErrorException(string message, Exception inner) : base(message, inner) {}
    }

    sealed class Spec : IEnumerable
    {
        public static readonly Spec Instance = new Spec
        {
            {
                TokenKind.LParen, (token, parser) =>
                {
                    var result = parser.Parse(0);
                    parser.Read(TokenKind.RParen, (TokenKind expected, (TokenKind, Token Token) actual) =>
                                    throw new SyntaxErrorException($"Expected {expected} token at {actual.Token.Index}."));
                    return result;
                }
            },

            { TokenKind.Bang, (_, parser) => !parser.Parse(Precedence.Prefix) },

            { TokenKind.AmpersandAmpersand, Precedence.LogicalAnd, (a, rbp, p) => a && p.Parse(rbp) },
            { TokenKind.PipePipe          , Precedence.LogicalOr , (a, b) => a || b },
            { TokenKind.EqualEqual        , Precedence.Relational, (a, b) => a == b },
            { TokenKind.BangEqual         , Precedence.Relational, (a, b) => a != b },
        };

        Spec() { }

        readonly Dictionary<TokenKind, PrefixParselet> _prefixes = new Dictionary<TokenKind, PrefixParselet>();
        readonly Dictionary<TokenKind, (Precedence, InfixParselet)> _infixes = new Dictionary<TokenKind, (Precedence, InfixParselet)>();

        void Add(TokenKind type, PrefixParselet prefix) =>
            _prefixes.Add(type, prefix);

        void Add(TokenKind type, Precedence precedence, InfixParselet prefix) =>
            _infixes.Add(type, (precedence, prefix));

        void Add(TokenKind type, Precedence precedence, Func<bool, bool, bool> f) =>
            Add(type, precedence, (token, left, parser) => f(left, parser.Parse(precedence)));

        void Add(TokenKind type, Precedence precedence, Func<bool, Precedence, Parser, bool> f) =>
            Add(type, precedence, (token, left, parser) => f(left, precedence, parser));

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
                            default:
                                if (ch < 20)
                                    throw new SyntaxErrorException($"Invalid string character at offset {i}.");
                                break;
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
                        state = State.QuotedString;
                        break;
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
                        if (!ignoreWhiteSpace)
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
                yield break;

            TokenKind kind;

            switch (state)
            {
                case State.WhiteSpace:
                    if (ignoreWhiteSpace)
                        yield break;
                    kind = TokenKind.WhiteSpace;
                    break;
                case State.Number        : kind = TokenKind.Number        ; break;
                case State.GreaterThan   : kind = TokenKind.GreaterThan   ; break;
                case State.LessThan      : kind = TokenKind.LessThan      ; break;
                case State.Bang          : kind = TokenKind.Bang          ; break;
                case State.Ampersand     : kind = TokenKind.Ampersand     ; break;
                case State.Pipe          : kind = TokenKind.Pipe          ; break;
                case State.UnquotedString: kind = TokenKind.UnquotedString; break;
                case State.LeftBracket   : kind = TokenKind.LBracket      ; break;
                case State.RawString        :
                case State.RawStringSlash   :
                case State.QuotedString     :
                case State.QuotedStringSlash:
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

            yield return Token(kind, i);
        }
    }
}
