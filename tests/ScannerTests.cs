using NUnit.Framework;

namespace JmesPath.Tests
{
    using System.Linq;
    using MoreLinq;

    public class ScannerTests
    {
        [TestCase("@", TokenKind.At)]
        [TestCase("123", TokenKind.Number)]
        [TestCase("123\t", TokenKind.Number)]
        [TestCase("-123", TokenKind.Number)]
        [TestCase("*", TokenKind.Star)]
        [TestCase(".", TokenKind.Dot)]
        [TestCase(",", TokenKind.Comma)]
        [TestCase(":", TokenKind.Colon)]
        [TestCase("&", TokenKind.Ampersand)]
        [TestCase("&\t", TokenKind.Ampersand)]
        [TestCase("&&", TokenKind.AmpersandAmpersand)]
        [TestCase("|", TokenKind.Pipe)]
        [TestCase("|\t", TokenKind.Pipe)]
        [TestCase("||", TokenKind.PipePipe)]
        [TestCase("(", TokenKind.LParen)]
        [TestCase(")", TokenKind.RParen)]
        [TestCase("{", TokenKind.LBrace)]
        [TestCase("}", TokenKind.RBrace)]
        [TestCase("[", TokenKind.LBracket)]
        [TestCase("[\t", TokenKind.LBracket)]
        [TestCase("]", TokenKind.RBracket)]
        [TestCase("[]", TokenKind.LRBracket)]
        [TestCase("[?", TokenKind.LBracketQuestion)]
        [TestCase(">", TokenKind.GreaterThan)]
        [TestCase(">\t", TokenKind.GreaterThan)]
        [TestCase(">=", TokenKind.GreaterThanEqual)]
        [TestCase("<", TokenKind.LessThan)]
        [TestCase("<\t", TokenKind.LessThan)]
        [TestCase("<=", TokenKind.LessThanEqual)]
        [TestCase("==", TokenKind.EqualEqual)]
        [TestCase("!", TokenKind.Bang)]
        [TestCase("!\t", TokenKind.Bang)]
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
            var tokens = Scanner.Scan(path, ScanOptions.IgnoreWhiteSpace);
            var token = tokens.Single();
            Assert.That(token.Kind, Is.EqualTo(kind));
            Assert.That(token.Index, Is.EqualTo(0));
            var trimmed = path.Trim();
            Assert.That(token.Length, Is.EqualTo(trimmed.Length));
            Assert.That(token.Substring(trimmed), Is.EqualTo(trimmed));
        }

        [TestCase(" \t\r\n")]
        public void WhiteSpace(string input)
        {
            var tokens = Scanner.Scan(input);
            var token = tokens.Single();
            Assert.That(token.Kind, Is.EqualTo(TokenKind.WhiteSpace));
            Assert.That(token.Index, Is.EqualTo(0));
            Assert.That(token.Length, Is.EqualTo(input.Length));
            Assert.That(token.Substring(input), Is.EqualTo(input));
        }

        [TestCase(" \t\r\n")]
        public void IgnoreWhiteSpace(string input)
        {
            Assert.That(Scanner.Scan(input, ScanOptions.IgnoreWhiteSpace), Is.Empty);
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
        [TestCase("'\x0", "Invalid string character at offset 1.")]
        public void SyntaxError(string path, string message)
        {
            var e = Assert.Throws<SyntaxErrorException>(() =>
                Scanner.Scan(path).Consume());
            Assert.That(e.Message, Is.EqualTo(message));
        }
    }
}
