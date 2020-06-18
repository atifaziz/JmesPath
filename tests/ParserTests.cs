namespace JmesPath.Tests
{
    using System;
    using System.Linq;
    using System.Text.RegularExpressions;
    using NUnit.Framework;

    public class ParserTests
    {
        [TestCase("@ 123", "Unexpected token <Number> at offset 2.")]
        [TestCase("foo bar", "Unexpected token <UnquotedString> at offset 4.")]
        public void SyntaxError(string path, string error)
        {
            var e = Assert.Throws<SyntaxErrorException>(() => Expression.Evaluate(path));
            Assert.That(e.Message, Is.EqualTo(error));
        }

        [TestCase("`null`", @"
            Literal `null`")]
        [TestCase("a && b", @"
            Field a
            Field b
            And")]
        [TestCase("a || b && c", @"
            Field a
            Field b
            Field c
            And
            Or")]
        [TestCase("(a || b) && c", @"
            Field a
            Field b
            Or
            Field c
            And")]
        [TestCase("a || !b && c", @"
            Field a
            Field b
            Not
            Field c
            And
            Or")]
        [TestCase("foo.bar", @"
            Field foo
            Field bar
            SubExpression")]
        [TestCase(@"""foo"".""bar""", @"
            Field ""foo""
            Field ""bar""
            SubExpression")]
        [TestCase("foo.bar.baz", @"
            Field foo
            Field bar
            SubExpression
            Field baz
            SubExpression")]
        [TestCase("a < b", @"
            Field a
            Field b
            LessThan")]
        [TestCase("a < b < c", @"
            Field a
            Field b
            LessThan
            Field c
            LessThan")]
        [TestCase("[]", @"
            Identity
            Flatten
            Identity
            Projection")]
        [TestCase("[foo, bar]", @"
            Field foo
            Field bar
            Const 2
            MultiSelectList")]
        [TestCase("[*, foo, bar]", @"
            Identity
            Identity
            ValueProjection
            Field foo
            Field bar
            Const 3
            MultiSelectList")]
        [TestCase("[*]", @"
            Identity
            Identity
            Projection")]
        [TestCase("foo[1]", @"
            Field foo
            Token 1
            Index")]
        [TestCase("foo[*]", @"
            Field foo
            Identity
            Projection")]
        [TestCase("foo[*].bar", @"
            Field foo
            Field bar
            Projection")]
        [TestCase("foo[*].bar.baz", @"
            Field foo
            Field bar
            Field baz
            SubExpression
            Projection")]
        [TestCase("foo.bar || foo.baz", @"
            Field foo
            Field bar
            SubExpression
            Field foo
            Field baz
            SubExpression
            Or")]
        [TestCase("foo.{bar: bar, baz: baz}", @"
            Field foo
            Token bar
            Field bar
            Token baz
            Field baz
            Const 2
            MultiSelectHash
            SubExpression")]
        [TestCase(@"foo.{""bar.baz"": bar.baz, qux: qux}", @"
            Field foo
            Token ""bar.baz""
            Field bar
            Field baz
            SubExpression
            Token qux
            Field qux
            Const 2
            MultiSelectHash
            SubExpression")]
        [TestCase(@"foo.{""bar"": bar.baz, ""qux"": qux}", @"
            Field foo
            Token ""bar""
            Field bar
            Field baz
            SubExpression
            Token ""qux""
            Field qux
            Const 2
            MultiSelectHash
            SubExpression")]
        [TestCase(@"f(@ && @)", @"
            Field f
            CurrentNode
            CurrentNode
            And
            Const 1
            Function")]
        [TestCase("locations[?state == 'WA'].name | sort(@) | {WashingtonCities: join(', ', @)}", @"
            Field locations
            Field state
            Literal 'WA'
            Equal
            Field name
            FilterProjection
            Field sort
            CurrentNode
            Const 1
            Function
            Pipe
            Token WashingtonCities
            Field join
            Literal ', '
            CurrentNode
            Const 2
            Function
            Const 1
            MultiSelectHash
            Pipe")]
        public void Parse(string expression, string expected)
        {
            var result = Expression.Evaluate(expression);
            var actuals = result.ToString("\n").Split('\n', StringSplitOptions.RemoveEmptyEntries);
            var expectations =
                from s in Regex.Split(expected, @"\r?\n")
                where !string.IsNullOrWhiteSpace(s)
                select s.Trim();
            Assert.That(actuals, Is.EqualTo(expectations));
        }

        /*
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
        [TestCase("[ foo, bar ]", "foo bar %MULTI_SELECT_LIST%")]
        public void Parse2(string input, string output)
        {
            var expectations = new List<(TokenKind, string, OpCode)>();
            var si = 0;
            foreach (Match match in Regex.Matches(output += " %EOI%", @"%[A-Za-z_]+%"))
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
                    expectations.Add((default, null, Enum.Parse<OpCode>(match.Value[1..^1].Replace("_", string.Empty), ignoreCase: true)));

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
        */
    }
}
