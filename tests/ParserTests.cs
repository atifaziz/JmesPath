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
        [TestCase("@(foo)", "Invalid function near offset 2.")]
        public void SyntaxError(string path, string error)
        {
            var e = Assert.Throws<SyntaxErrorException>(() => Expression.Parse(path));
            Assert.That(e.Message, Is.EqualTo(error));
        }

        [TestCase("`null`", @"
            0: Literal `null`")]
        [TestCase("a && b", @"
            0: Field a
            0: Field b
            0: And")]
        [TestCase("a || b && c", @"
            0: Field a
            0: Field b
            0: Field c
            0: And
            0: Or")]
        [TestCase("(a || b) && c", @"
            0: Field a
            0: Field b
            0: Or
            0: Field c
            0: And")]
        [TestCase("a || !b && c", @"
            0: Field a
            0: Field b
            0: Not
            0: Field c
            0: And
            0: Or")]
        [TestCase("foo.bar", @"
            0: Field foo
            0: Field bar
            0: SubExpression")]
        [TestCase(@"""foo"".""bar""", @"
            0: Field ""foo""
            0: Field ""bar""
            0: SubExpression")]
        [TestCase("foo.bar.baz", @"
            0: Field foo
            0: Field bar
            0: SubExpression
            0: Field baz
            0: SubExpression")]
        [TestCase("a < b", @"
            0: Field a
            0: Field b
            0: LessThan")]
        [TestCase("a < b < c", @"
            0: Field a
            0: Field b
            0: LessThan
            0: Field c
            0: LessThan")]
        [TestCase("[]", @"
            0: Identity
            0: Flatten
            0: Identity
            0: Projection")]
        [TestCase("[foo, bar]", @"
            0: Field foo
            0: Field bar
            0: Const 2
            0: MultiSelectList")]
        [TestCase("[*, foo, bar]", @"
            0: Identity
            0: Identity
            0: ValueProjection
            0: Field foo
            0: Field bar
            0: Const 3
            0: MultiSelectList")]
        [TestCase("[*]", @"
            0: Identity
            0: Identity
            0: Projection")]
        [TestCase("foo[1]", @"
            0: Field foo
            0: Const 1
            0: Index")]
        [TestCase("foo[*]", @"
            0: Field foo
            0: Identity
            0: Projection")]
        [TestCase("foo[*].bar", @"
            0: Field foo
            0: Field bar
            0: Projection")]
        [TestCase("foo[*].bar.baz", @"
            0: Field foo
            0: Field bar
            0: Field baz
            0: SubExpression
            0: Projection")]
        [TestCase("foo.bar || foo.baz", @"
            0: Field foo
            0: Field bar
            0: SubExpression
            0: Field foo
            0: Field baz
            0: SubExpression
            0: Or")]
        [TestCase("foo.{bar: bar, baz: baz}", @"
            0: Field foo
            0: Token bar
            0: Field bar
            0: Token baz
            0: Field baz
            0: Const 2
            0: MultiSelectHash
            0: SubExpression")]
        [TestCase(@"foo.{""bar.baz"": bar.baz, qux: qux}", @"
            0: Field foo
            0: Token ""bar.baz""
            0: Field bar
            0: Field baz
            0: SubExpression
            0: Token qux
            0: Field qux
            0: Const 2
            0: MultiSelectHash
            0: SubExpression")]
        [TestCase(@"foo.{""bar"": bar.baz, ""qux"": qux}", @"
            0: Field foo
            0: Token ""bar""
            0: Field bar
            0: Field baz
            0: SubExpression
            0: Token ""qux""
            0: Field qux
            0: Const 2
            0: MultiSelectHash
            0: SubExpression")]
        [TestCase(@"f(@ && @)", @"
            0: Field f
            0: Current
            0: Current
            0: And
            0: Const 1
            0: Function")]
        [TestCase("people[].[name, state.name.boo]", @"
            0: Field people
            0: Flatten
            0: Field name
            0: Field state
            0: Field name
            0: SubExpression
            0: Field boo
            0: SubExpression
            0: Const 2
            0: MultiSelectList
            0: Projection
            ")]
        [TestCase("locations[?state == 'WA'].name[0] | sort(@) | {WashingtonCities: join(', ', @)}", @"
            0: Field locations
            0: MkRef
            1: Field state
            1: Literal 'WA'
            1: Equal
            0: LdRef
            0: MkRef
            0: Field name
            0: Const 0
            0: Index
            0: LdRef
            0: FilterProjection
            0: Field sort
            0: Current
            0: Const 1
            0: Function
            0: Pipe
            0: Token WashingtonCities
            0: Field join
            0: Literal ', '
            0: Current
            0: Const 2
            0: Function
            0: Const 1
            0: MultiSelectHash
            0: Pipe")]
        public void Parse(string expression, string expected)
        {
            var result = Expression.Parse(expression);
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
