namespace JmesPath.Tests
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Linq;
    using System.Text.Json;
    using NUnit.Framework;
    using NUnit.Framework.Interfaces;

    public class ComplianceTests
    {
        [TestCaseSource(nameof(TestData), new object[] { true })]
        public void Result(JsonElement given, string expression, JsonElement result)
        {
        }

        [TestCaseSource(nameof(TestData), new object[] { false })]
        public void Error(JsonElement given, string expression, string error)
        {
            switch (error)
            {
                case "syntax":
                    Assert.Throws<SyntaxErrorException>(() => Evaluate(given, expression));
                    break;
                case "invalid-type":
                    throw new NotImplementedException();
                case "invalid-value":
                    throw new NotImplementedException();
                case "unknown-function":
                    throw new NotImplementedException();
            }
        }

        static JsonValue Evaluate(JsonElement given, string expression)
        {
            var pe = Expression.Parse(expression);
            var projector = new TreeProjector<Node>(new TreeProjector());
            Machine.Run(pe, projector);
            return projector.Result.Evaluate(JsonValue.From(given), JsonSystem.Default);
        }

        static IEnumerable<ITestCaseData> TestData(bool success) =>
            from t in ComplianceTestData
            let re = t.Match(r => new { Result = r, Error = (string)null },
                             e => new { Result = default(JsonElement), Error = e })
            where !success && re.Error is {}
               || success && re.Error is null
            select new TestCaseData(t.Given, t.Expression, re.Error ?? (object)re.Result)
                .SetName($"{(success ? "Result" : "Error")}.{t.Name}({t.Expression}) << {t.Given})");

        abstract class ComplianceRecord
        {
            public readonly string Name;
            public readonly JsonElement Given;
            public readonly string Expression;

            public static ComplianceRecord
                Result(string name, JsonElement given, string expression, JsonElement result) =>
                new SuccessRecord(name, given, expression, result);

            public static ComplianceRecord
                Error(string name, JsonElement given, string expression, string error) =>
                new ErrorRecord(name, given, expression, error);

            ComplianceRecord(string name, JsonElement given, string expression) =>
                (Name, Given, Expression) = (name, given, expression);

            public abstract T Match<T>(Func<JsonElement, T> result, Func<string, T> error);

            sealed class SuccessRecord : ComplianceRecord
            {
                readonly JsonElement _result;
                
                public SuccessRecord(string name, JsonElement given, string expression, JsonElement result) :
                    base(name, given, expression) => _result = result;

                public override T Match<T>(Func<JsonElement, T> result,
                                           Func<string, T> error) => result(_result);
            }

            sealed class ErrorRecord : ComplianceRecord
            {
                readonly string _error;

                public ErrorRecord(string name, JsonElement given, string expression, string error) :
                    base(name, given, expression) => _error = error;

                public override T Match<T>(Func<JsonElement, T> result,
                                           Func<string, T> error) => error(_error);
            }
        }

        static IEnumerable<ComplianceRecord> ComplianceTestData =>
            from path in Directory.EnumerateFiles(Path.Join(TestContext.CurrentContext.TestDirectory, "tests"), "*.json")
            let source = Path.GetFileNameWithoutExtension(path)
            from t in JsonDocument.Parse(File.ReadAllText(path)).RootElement.EnumerateArray()
            let given = t.GetProperty("given")
            from c in t.GetProperty("cases").EnumerateArray()
            let e = c.GetProperty("expression").GetString()
            where !c.TryGetProperty("bench", out _)
            select c.TryGetProperty("result", out var res)
                 ? ComplianceRecord.Result(source, given, e, res)
                 : ComplianceRecord.Error(source, given, e, c.GetProperty("error").GetString());
    }
}
