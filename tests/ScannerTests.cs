namespace JmesPath.Tests
{
    using System.Linq;
    using MoreLinq;
    using NUnit.Framework;

    public class ScannerTests
    {
        [TestCase("@", TokenKind.At)]
        [TestCase("123", TokenKind.Number)]
        [TestCase("-123", TokenKind.Number)]
        [TestCase("*", TokenKind.Star)]
        [TestCase(".", TokenKind.Dot)]
        [TestCase(",", TokenKind.Comma)]
        [TestCase(":", TokenKind.Colon)]
        [TestCase("&", TokenKind.Ampersand)]
        [TestCase("&&", TokenKind.AmpersandAmpersand)]
        [TestCase("|", TokenKind.Pipe)]
        [TestCase("||", TokenKind.PipePipe)]
        [TestCase("(", TokenKind.LParen)]
        [TestCase(")", TokenKind.RParen)]
        [TestCase("{", TokenKind.LBrace)]
        [TestCase("}", TokenKind.RBrace)]
        [TestCase("[", TokenKind.LBracket)]
        [TestCase("]", TokenKind.RBracket)]
        [TestCase("[]", TokenKind.LRBracket)]
        [TestCase("[?", TokenKind.LBracketQuestion)]
        [TestCase(">", TokenKind.GreaterThan)]
        [TestCase(">=", TokenKind.GreaterThanEqual)]
        [TestCase("<", TokenKind.LessThan)]
        [TestCase("<=", TokenKind.LessThanEqual)]
        [TestCase("==", TokenKind.EqualEqual)]
        [TestCase("!", TokenKind.Bang)]
        [TestCase("!=", TokenKind.BangEqual)]
        [TestCase(@"'foo bar'", TokenKind.RawString)]
        [TestCase(@"'foo\\bar'", TokenKind.RawString)]
        [TestCase(@"""foo bar""", TokenKind.QuotedString)]
        [TestCase(@"""foo\\bar""", TokenKind.QuotedString)]
        [TestCase(@"`foo bar`", TokenKind.Literal)]
        [TestCase(@"`\`foo bar\``", TokenKind.Literal)]
        [TestCase("foo_bar", TokenKind.UnquotedString)]
        public void Tokens(string path, TokenKind kind)
        {
            foreach (var (i, s) in new[]
            {
                (0, path),
                (1, $"\t{path}"),
                (0, $"{path}\t"),
                (1, $"\t{path}\t"),
            })
            {
                var tokens = Scanner.Scan(s, ScanOptions.IgnoreWhiteSpace | ScanOptions.NoEoiToken);
                var token = tokens.Single();
                Assert.That(token.Kind, Is.EqualTo(kind));
                Assert.That(token.Index, Is.EqualTo(i));
                var trimmed = s.Trim();
                Assert.That(token.Length, Is.EqualTo(trimmed.Length));
                Assert.That(token.Substring(s), Is.EqualTo(trimmed));
            }
        }

        [TestCase("@ \t\r\n@")]
        public void WhiteSpace(string input)
        {
            var tokens = Scanner.Scan(input, ScanOptions.NoEoiToken);
            var token = tokens.Single(t => t.Kind == TokenKind.WhiteSpace);
            Assert.That(token.Kind, Is.EqualTo(TokenKind.WhiteSpace));
            Assert.That(token.Index, Is.EqualTo(1));
            Assert.That(token.Length, Is.EqualTo(4));
            Assert.That(token.Substring(input), Is.EqualTo(" \t\r\n"));
        }

        [TestCase(" \t\r\n")]
        public void IgnoreWhiteSpace(string input)
        {
            Assert.That(Scanner.Scan(input, ScanOptions.IgnoreWhiteSpace
                                          | ScanOptions.NoEoiToken),
                        Is.Empty);
        }

        [TestCase("$", "Unexpected at offset 0: $")]
        [TestCase("~", "Unexpected at offset 0: ~")]
        [TestCase("=", "Unexpected end of input at offset 1.")]
        [TestCase("=!", "Unexpected at offset 1: !")]
        [TestCase("-", "Unexpected end of input at offset 1; missing number digit(s).")]
        [TestCase("foo-bar", "Expected digit (0-9) at offset 4, but got: b")]
        [TestCase("foo-(bar)", "Expected digit (0-9) at offset 4, but got: (")]
        [TestCase("'foo", "Unexpected end of input at offset 4; unterminated string.")]
        [TestCase("\"foo", "Unexpected end of input at offset 4; unterminated string.")]
        [TestCase("`\"foo\"", "Unexpected end of input at offset 6; unterminated literal.")]
        [TestCase("\"\x0", "Invalid string character at offset 1.")]
        [TestCase(@"""\u""", "Invalid escape hexadecimal digit at offset 3.")]
        [TestCase(@"""\u1""", "Invalid escape hexadecimal digit at offset 4.")]
        [TestCase(@"""\u12""", "Invalid escape hexadecimal digit at offset 5.")]
        [TestCase(@"""\u123""", "Invalid escape hexadecimal digit at offset 6.")]
        [TestCase(@"""\u", "Unexpected end of input at offset 3; unterminated string.")]
        [TestCase(@"""\u1", "Unexpected end of input at offset 4; unterminated string.")]
        [TestCase(@"""\u12", "Unexpected end of input at offset 5; unterminated string.")]
        [TestCase(@"""\u123", "Unexpected end of input at offset 6; unterminated string.")]
        public void SyntaxError(string path, string message)
        {
            var e = Assert.Throws<SyntaxErrorException>(() =>
                Scanner.Scan(path).Consume());
            Assert.That(e.Message, Is.EqualTo(message));
        }
    }
}
