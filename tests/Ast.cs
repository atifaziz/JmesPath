namespace JmesPath.Tests
{
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text;
    using System.Text.Json;

    enum JsonValueKind { Null, Boolean, Number, String, Array, Object }

    interface IJsonSystem<T>
    {
        T True { get; }
        T False { get; }
        T Null { get; }
        JsonValueKind GetKind(T value);
        T Parse(string json);
        T String(string value);
        bool GetMemberValue(T obj, string name, out T value);
        bool GetBooleanValue(T value);
        int GetLength(T value);
    }

    struct JsonObject : IDictionary<string, JsonValue>
    {
        readonly List<KeyValuePair<string, JsonValue>> _members;
        ICollection<string> _keys;
        ICollection<JsonValue> _values;

        public JsonObject(List<KeyValuePair<string, JsonValue>> members) =>
            (_members, _keys, _values) = (members, null, null);

        public IEnumerator<KeyValuePair<string, JsonValue>> GetEnumerator() =>
            _members.GetEnumerator();

        IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();

        bool ICollection<KeyValuePair<string, JsonValue>>.Contains(KeyValuePair<string, JsonValue> item) =>
            _members is {} ms && ms.Contains(item);

        public void CopyTo(KeyValuePair<string, JsonValue>[] array, int arrayIndex)
        {
            if (_members is {} ms)
                ms.CopyTo(array, arrayIndex);
        }

        public int Count => _members is {} ms ? ms.Count : 0;

        bool ICollection<KeyValuePair<string, JsonValue>>.IsReadOnly => true;

        public bool ContainsKey(string key) =>
            _members is {} ms && ms.Any(m => m.Key == key);

        public bool TryGetValue(string key, out JsonValue value)
        {
            if (_members is {} ms && ms.SingleOrDefault(m => m.Key == key) is {} m
                                  && m.Key is {})
            {
                value = m.Value;
                return true;
            }

            value = default;
            return false;
        }

        public JsonValue this[string key]
        {
            get => TryGetValue(key, out var value) ? value : JsonValue.Null;
            set => throw ReadOnlyError();
        }

        public ICollection<string> Keys
            => _keys ??= _members is {} ms
             ? ms.Select(e => e.Key).ToList()
             : (ICollection<string>)Array.Empty<string>();

        public ICollection<JsonValue> Values
            => _values ??= _members is {} ms
             ? ms.Select(e => e.Value).ToList()
             : (ICollection<JsonValue>)Array.Empty<JsonValue>();

        static InvalidOperationException ReadOnlyError() => new InvalidOperationException("Collection is read-only.");

        void ICollection<KeyValuePair<string, JsonValue>>.Add(KeyValuePair<string, JsonValue> item) => throw ReadOnlyError();
        void ICollection<KeyValuePair<string, JsonValue>>.Clear() => throw ReadOnlyError();
        bool ICollection<KeyValuePair<string, JsonValue>>.Remove(KeyValuePair<string, JsonValue> item) => throw ReadOnlyError();
        void IDictionary<string, JsonValue>.Add(string key, JsonValue value) => throw ReadOnlyError();
        bool IDictionary<string, JsonValue>.Remove(string key) => throw ReadOnlyError();
    }

    struct JsonValue
    {
        enum K : byte { Null, False, True, Number, String, Object, Array }

        readonly K _kind;
        readonly double _number;
        readonly object _object;

        JsonValue(K kind, double number = default, object obj = default) =>
            (_kind, _number, _object) = (kind, number, obj);

        public JsonValueKind Kind => _kind switch
        {
            K.Null   => JsonValueKind.Null,
            K.False  => JsonValueKind.Boolean,
            K.True   => JsonValueKind.Boolean,
            K.Number => JsonValueKind.Number,
            K.String => JsonValueKind.String,
            K.Object => JsonValueKind.Object,
            K.Array  => JsonValueKind.Array,
        };

        public static readonly JsonValue Null = new JsonValue(K.Null);
        public static readonly JsonValue True = new JsonValue(K.True);
        public static readonly JsonValue False = new JsonValue(K.False);

        public static JsonValue Number(double num) => new JsonValue(K.Number, number: num);
        public static JsonValue String(string str) => new JsonValue(K.String, obj: str);
        public static JsonValue Array(IList<JsonValue> arr) => new JsonValue(K.Array, obj: arr);
        public static JsonValue Object(IDictionary<string, JsonValue> obj) => new JsonValue(K.Object, obj: obj);

        public static JsonValue Object(params (string, JsonValue)[] members)
        {
            var obj = new Dictionary<string, JsonValue>();
            foreach (var (name, value) in members)
                obj.Add(name, value);
            return Object(obj);
        }

        public T Match<T>(T nul = default,
                          Func<bool, T> bit = null,
                          Func<double, T> num = null,
                          Func<string, T> str = null,
                          Func<IList<JsonValue>, T> arr = null,
                          Func<IDictionary<string, JsonValue>, T> obj = null) =>
            Match(() => nul, bit, num, str, arr, obj);

        public T Match<T>(Func<T> nul = null,
                          Func<bool, T> bit = null,
                          Func<double, T> num = null,
                          Func<string, T> str = null,
                          Func<IList<JsonValue>, T> arr = null,
                          Func<IDictionary<string, JsonValue>, T> obj = null) =>
            _kind switch
            {
                K.Null   => nul is {} ? nul() : throw new InvalidOperationException(),
                K.True   => bit is {} ? bit(true) : throw new InvalidOperationException(),
                K.False  => bit is {} ? bit(false) : throw new InvalidOperationException(),
                K.Number => num is {} ? num(_number) : throw new InvalidOperationException(),
                K.String => str is {} ? str((string)_object) : throw new InvalidOperationException(),
                K.Array  => arr is {} ? arr((IList<JsonValue>)_object) : throw new InvalidOperationException(),
                K.Object => obj is {} ? obj((IDictionary<string, JsonValue>)_object) : throw new InvalidOperationException(),
            };

        public T Match<A, T>(A arg,
                             Func<A, T> nul = null,
                             Func<A, bool, T> bit = null,
                             Func<A, double, T> num = null,
                             Func<A, string, T> str = null,
                             Func<A, IList<JsonValue>, T> arr = null,
                             Func<A, IDictionary<string, JsonValue>, T> obj = null) =>
            _kind switch
            {
                K.Null   => nul is {} ? nul(arg) : throw new InvalidOperationException(),
                K.True   => bit is {} ? bit(arg, true) : throw new InvalidOperationException(),
                K.False  => bit is {} ? bit(arg, false) : throw new InvalidOperationException(),
                K.Number => num is {} ? num(arg, _number) : throw new InvalidOperationException(),
                K.String => str is {} ? str(arg, (string)_object) : throw new InvalidOperationException(),
                K.Array  => arr is {} ? arr(arg, (IList<JsonValue>)_object) : throw new InvalidOperationException(),
                K.Object => obj is {} ? obj(arg, (IDictionary<string, JsonValue>)_object) : throw new InvalidOperationException(),
            };

        public static JsonValue From(JsonElement e) => e.ValueKind switch
        {
            System.Text.Json.JsonValueKind.Null => Null,
            System.Text.Json.JsonValueKind.Undefined => Null,
            System.Text.Json.JsonValueKind.Object =>
                Object(new JsonObject(e.EnumerateObject()
                                       .Select(m => KeyValuePair.Create(m.Name, From(m.Value)))
                                       .ToList())),
            System.Text.Json.JsonValueKind.Array =>
                Array(e.EnumerateArray().Select(From).ToList()),
            System.Text.Json.JsonValueKind.String => String(e.GetString()),
            System.Text.Json.JsonValueKind.Number => Number(e.GetDouble()),
            System.Text.Json.JsonValueKind.True => True,
            System.Text.Json.JsonValueKind.False => False,
            _ => throw new ArgumentOutOfRangeException(nameof(e), e, null)
        };
    }

    static class JsonSystem
    {
        public static readonly IJsonSystem<JsonValue> Default = new DefaultJsonSystem();

        sealed class DefaultJsonSystem : IJsonSystem<JsonValue>
        {
            public JsonValue True => JsonValue.True;
            public JsonValue False => JsonValue.False;
            public JsonValue Null => JsonValue.Null;

            public JsonValueKind GetKind(JsonValue value) => value.Kind;

            public JsonValue Parse(string json)
            {
                throw new NotImplementedException();
            }

            public JsonValue String(string value) => JsonValue.String(value);

            public bool GetMemberValue(JsonValue obj, string name, out JsonValue value)
            {
                if (obj.Kind != JsonValueKind.Object)
                    throw new ArgumentOutOfRangeException(nameof(obj), obj, null);
                if (obj.Match(name, obj: (n, obj) => obj.TryGetValue(n, out var v) ? v : (JsonValue?)null) is {} v)
                {
                    value = v;
                    return true;
                }
                else
                {
                    value = default;
                    return false;
                }
            }

            public bool GetBooleanValue(JsonValue value) =>
                value.Match(nul: (bool?)null,
                            bit: bit => bit,
                            num: _ => null,
                            str: _ => null,
                            arr: _ => null,
                            obj: _ => null)
                    is {} bit ? bit : throw new ArgumentOutOfRangeException(nameof(value), value, null);

            public int GetLength(JsonValue value) =>
                value.Match(nul: (int?)null,
                            bit: _ => null,
                            num: _ => null,
                            str: str => str.Length,
                            arr: arr => arr.Count,
                            obj: obj => obj.Count)
                    is {} len ? len : throw new ArgumentOutOfRangeException(nameof(value), value, null);
        }

        public static bool IsTruthy<T>(this IJsonSystem<T> system, T value)
        {
            // A false value corresponds to any of the following conditions:
            //
            // - Empty list   : []
            // - Empty object : {}
            // - Empty string : ""
            // - False boolean: false
            // - Null value   : null
            //
            // A true value corresponds to any value that is not false.

            switch (system.GetKind(value))
            {
                case JsonValueKind.Number:
                case JsonValueKind.Null:
                    return false;
                case JsonValueKind.Boolean:
                    return system.GetBooleanValue(value);
                case JsonValueKind.String:
                case JsonValueKind.Array:
                case JsonValueKind.Object:
                    return system.GetLength(value) > 0;
                default:
                    throw new Exception("Implementation error.");
            }
        }
    }

    abstract class Node
    {
        public abstract T Evaluate<T>(T value, IJsonSystem<T> system);
    }

    sealed class IdentityNode : Node
    {
        public static readonly IdentityNode Value = new IdentityNode();

        public override T Evaluate<T>(T value, IJsonSystem<T> system) => value;
    }

    sealed class StringNode : Node
    {
        public string Value { get; }

        public StringNode(string value) =>
            Value = value;

        public override T Evaluate<T>(T value, IJsonSystem<T> system) =>
            system.String(Value);

        public override string ToString() => Value;

        public static implicit operator string(StringNode node) => node.Value;
    }

    sealed class LiteralNode : Node
    {
        public string Json { get; }

        public LiteralNode(string json) =>
            Json = json;

        public override T Evaluate<T>(T value, IJsonSystem<T> system) =>
            system.Parse(Json);

        public override string ToString() => Json;
    }

    sealed class FieldNode : Node
    {
        public string Name { get; }

        public FieldNode(string name) => Name = name;

        public override T Evaluate<T>(T value, IJsonSystem<T> system)
            => system.GetKind(value) == JsonValueKind.Object
            && system.GetMemberValue(value, Name, out var v) ? v : system.Null;
    }

    sealed class NotNode : Node
    {
        public Node Expression { get; }

        public NotNode(Node expression) => Expression = expression;

        public override T Evaluate<T>(T value, IJsonSystem<T> system)
            => system.IsTruthy(Expression.Evaluate(value, system)) ? system.False : system.True;
    }

    abstract class BinaryNode : Node
    {
        public Node Left { get; }
        public Node Right { get; }

        public BinaryNode(Node left, Node right) =>
            (Left, Right) = (left, right);
    }

    sealed class AndNode : BinaryNode
    {
        public AndNode(Node left, Node right) : base(left, right) {}

        public override T Evaluate<T>(T value, IJsonSystem<T> system)
            => system.IsTruthy(Left.Evaluate(value, system))
            && system.IsTruthy(Right.Evaluate(value, system)) ? system.True : system.False;
    }

    sealed class OrNode : BinaryNode
    {
        public OrNode(Node left, Node right) : base(left, right) { }

        public override T Evaluate<T>(T value, IJsonSystem<T> system)
            => system.IsTruthy(Left.Evaluate(value, system))
            && system.IsTruthy(Right.Evaluate(value, system)) ? system.True : system.False;
    }

    sealed class TreeProjector : ITreeProjector<Node>
    {
        public Node RawString(string s, int index, int length)
        {
            return new StringNode(Unescape(s.Substring(index + 1, length - 2)));

            static string Unescape(string s)
            {
                StringBuilder sb = null;
                for (var si = 0; ;)
                {
                    var i = s.IndexOf('\\', si);
                    if (i < 0)
                        break;
                    sb ??= new StringBuilder();
                    sb.Append(s, si, i - si).Append(s[i + 1]);
                    si = i + 2;
                }
                return sb?.ToString() ?? s;
            }
        }

        public Node UnquotedString(string s, int index, int length) =>
            new StringNode(s.Substring(index, length));

        public Node QuotedString(string s, int index, int length)
        {
            return new StringNode(Unescape(s.Substring(index + 1, length - 2)));

            static string Unescape(string s)
            {
                StringBuilder sb = null;
                for (var si = 0; ;)
                {
                    var i = s.IndexOf('\\', si);
                    if (i < 0)
                        break;
                    var replacement = s[i + 1] switch
                    {
                        '\\' => '\\',
                        '"' => '"',
                        '/' => '/',
                        'b' => '\b',
                        'f' => '\f',
                        'n' => '\n',
                        'r' => '\r',
                        'v' => '\v',
                        't' => '\t',
                        _ => (char?)null
                    };
                    sb ??= new StringBuilder();
                    sb.Append(s, si, i - si);
                    si = i + 1;
                    if (replacement is {} ch)
                    {
                        sb.Append(ch);
                        si++;
                    }
                }
                return sb?.ToString() ?? s;
            }
        }

        public Node Literal(string s, int index, int length) =>
            new LiteralNode(s.Substring(index, length));

        public Node CurrentNode()
        {
            throw new System.NotImplementedException();
        }

        public Node Not(Node expression)
        {
            throw new System.NotImplementedException();
        }

        public Node And(Node left, Node right)
        {
            throw new System.NotImplementedException();
        }

        public Node Or(Node left, Node right)
        {
            throw new System.NotImplementedException();
        }

        public Node GreaterThan(Node left, Node right)
        {
            throw new System.NotImplementedException();
        }

        public Node GreaterThanEqual(Node left, Node right)
        {
            throw new System.NotImplementedException();
        }

        public Node LessThan(Node left, Node right)
        {
            throw new System.NotImplementedException();
        }

        public Node LessThanEqual(Node left, Node right)
        {
            throw new System.NotImplementedException();
        }

        public Node Equal(Node left, Node right)
        {
            throw new System.NotImplementedException();
        }

        public Node NotEqual(Node left, Node right)
        {
            throw new System.NotImplementedException();
        }

        public Node Flatten(Node source)
        {
            throw new System.NotImplementedException();
        }

        public Node Field(Node name) => new FieldNode((StringNode)name);

        public Node Identity() => IdentityNode.Value;

        public Node ValueProjection(Node left, Node right)
        {
            throw new System.NotImplementedException();
        }

        public Node MultiSelectList(Node[] list)
        {
            throw new System.NotImplementedException();
        }

        public Node MultiSelectHash(KeyValuePair<string, Node>[] hash)
        {
            throw new System.NotImplementedException();
        }

        public Node FilterProjection(Node left, Node right, Node condition)
        {
            throw new System.NotImplementedException();
        }

        public Node Projection(Node left, Node right)
        {
            throw new System.NotImplementedException();
        }

        public Node Index(Node source, int index)
        {
            throw new System.NotImplementedException();
        }

        public Node Slice(int? start, int? stop, int? step)
        {
            throw new System.NotImplementedException();
        }

        public Node Reference()
        {
            throw new System.NotImplementedException();
        }

        public Node SubExpression(Node left, Node right)
        {
            throw new System.NotImplementedException();
        }

        public Node Pipe(Node left, Node right)
        {
            throw new System.NotImplementedException();
        }

        public void Function(Node name, Node[] args)
        {
            throw new System.NotImplementedException();
        }
    }
}
