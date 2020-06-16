namespace JmesPath.Tests
{
    using System;
    using System.Collections.Generic;
    using System.Text;
    using System.Text.RegularExpressions;
    using NUnit.Framework;

    public class ParserTests
    {
        [TestCase("@ 123", "Unexpected token <Number> at offset <2>.")]
        [TestCase("foo bar", "Unexpected token <UnquotedString> at offset <4>.")]
        public void SyntaxError(string path, string error)
        {
            var e = Assert.Throws<SyntaxErrorException>(() => Expression.Evaluate(path));
            Assert.That(e.Message, Is.EqualTo(error));
        }

        [TestCase("'foobar'", OpCode.Token, "'foobar'")]
        //[TestCase("`null`", OpCode.Token, "`null`")]
        public void Parse1(string path, OpCode code, string data)
        {
            var result = Expression.Evaluate(path);
            Assert.That(result.Count, Is.EqualTo(1));
            Assert.That(result[0].OpCode, Is.EqualTo(code));
            if (code == OpCode.Token)
                Assert.That(result[0].Token.Substring(path), Is.EqualTo(data));
        }

        [TestCase("`null`", "`null`")]
        [TestCase("a && b", "a %FIELD% b %FIELD% &&")]
        [TestCase("a || b && c", "a %FIELD% b %FIELD% c %FIELD% && ||")]
        [TestCase("(a || b) && c", "a %FIELD% b %FIELD% || c %FIELD% &&")]
        [TestCase("a || !b && c", "a %FIELD% b %FIELD% ! c %FIELD% && ||")]
        [TestCase("!a || !b && c", "a %FIELD% ! b %FIELD% ! c %FIELD% && ||")]
        [TestCase("!a || !b && !c", "a %FIELD% ! b %FIELD% ! c %FIELD% ! && ||")]
        [TestCase("@.b", "@ b %FIELD% .")]
        [TestCase("foobar", "foobar %FIELD%")]
        [TestCase("a < b", "a %FIELD% b %FIELD% <")]
        [TestCase("a < b < c", "a %FIELD% b %FIELD% < c %FIELD% <")]
        [TestCase("[]", "[]")]
        [TestCase("a[] && b", "a %FIELD% [] b %FIELD% &&")]
        [TestCase("a.b[] && c", "a %FIELD% b %FIELD% [] . c %FIELD% &&")]
        [TestCase("a.b[] && c[]", "a %FIELD% b %FIELD% [] . c %FIELD% [] &&")]
        [TestCase("\"foobar\"", "\"foobar\" %FIELD%")]
        public void Parse2(string input, string output)
        {
            var expectations = new List<(TokenKind, string, OpCode)>();
            var si = 0;
            foreach (Match match in Regex.Matches(output += " %EOI%", @"%[A-Za-z]+%"))
            {
                var part = output[si..match.Index];
                foreach (var t in Scanner.Scan(part, ScanOptions.IgnoreWhiteSpace | ScanOptions.NoEoiToken))
                {
                    var expectation = t.Kind switch
                    {
                        TokenKind.At => (default(TokenKind), (string)null, OpCode.CurrentNode),
                        TokenKind.AmpersandAmpersand => (default, null, OpCode.And),
                        TokenKind.PipePipe => (default, null, OpCode.Or),
                        TokenKind.Bang => (default, null, OpCode.Not),
                        TokenKind.Dot => (default, null, OpCode.Dot),
                        TokenKind.UnquotedString => (t.Kind, t.Substring(part), default),
                        TokenKind.QuotedString => (t.Kind, t.Substring(part), default),
                        TokenKind.Literal => (t.Kind, t.Substring(part), default),
                        TokenKind.LessThan => (default, null, OpCode.LessThan),
                        TokenKind.LRBracket => (default, null, OpCode.Flatten),
                    };
                    expectations.Add(expectation);
                }

                if (!"%EOI%".Equals(match.Value, StringComparison.OrdinalIgnoreCase))
                    expectations.Add((default, null, Enum.Parse<OpCode>(match.Value[1..^1], ignoreCase: true)));

                si = match.Index + match.Length;
            }
            var result = Expression.Evaluate(input);
            Assert.That(result.Count, Is.EqualTo(expectations.Count));
            using var e = result.GetEnumerator();
            foreach (var (tk, tt, op) in expectations)
            {
                Assert.That(e.MoveNext(), Is.True);
                if (tk != TokenKind.Eoi)
                {
                    Assert.That(e.Current.Token.Kind, Is.EqualTo(tk));
                    Assert.That(e.Current.Token.Substring(input), Is.EqualTo(tt));
                }
                else
                {
                    Assert.That(e.Current.OpCode, Is.EqualTo(op));
                }
            }
            Assert.That(e.MoveNext(), Is.False);
        }
    }
}
