namespace JmesPath.Tests
{
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.Globalization;
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
        T Array(IList<T> values);
        T Object(IEnumerable<KeyValuePair<string, T>> members);
        bool GetMemberValue(T obj, string name, out T value);
        bool GetBooleanValue(T value);
        double GetNumberValue(T value);
        string GetStringValue(T value);
        int GetLength(T value);
        T Index(T value, int index);
        IEnumerable<T> GetArrayValues(T value);
        IEnumerable<KeyValuePair<string, T>> GetObjectMembers(T value);
    }

    struct JsonObject : IDictionary<string, JsonValue>, IEquatable<JsonObject>
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

        public override string ToString() => JsonSerializer.Serialize(this);

        public bool Equals(JsonObject other)
        {
            if (Count != other.Count)
                return false;

            foreach (var (name, value) in _members)
            {
                if (!other.TryGetValue(name, out var otherValue) || value != otherValue)
                    return false;
            }

            return true;
        }

        public override bool Equals(object obj) =>
            obj is JsonObject other && Equals(other);

        public override int GetHashCode() => _members.GetHashCode();

        public static bool operator ==(JsonObject left, JsonObject right) => left.Equals(right);
        public static bool operator !=(JsonObject left, JsonObject right) => !left.Equals(right);
    }

    struct JsonValue : IEquatable<JsonValue>
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

        public bool GetBoolean() =>
            Kind == JsonValueKind.Boolean ? _kind == K.True : throw new InvalidOperationException();

        public double GetNumber() =>
            Kind == JsonValueKind.Number ? _number : throw new InvalidOperationException();

        public string GetString() =>
            Kind == JsonValueKind.String ? (string)_object : throw new InvalidOperationException();

        public IList<JsonValue> GetArray() =>
            Kind == JsonValueKind.Array ? (IList<JsonValue>)_object : throw new InvalidOperationException();

        public IDictionary<string, JsonValue> GetObject() =>
            Kind == JsonValueKind.Object ? (IDictionary<string, JsonValue>)_object : throw new InvalidOperationException();

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

        public bool Equals(JsonValue other)
            => _kind == other._kind
            && Match(other,
                     nul: _ => true,
                     bit: (a, v) => true,
                     num: (a, v) => Math.Abs(a._number - v) < double.Epsilon,
                     str: (a, v) => a.GetString() == v,
                     arr: (a, v) => v.SequenceEqual(a.GetArray()),
                     obj: (a, v) => (JsonObject)a.GetObject() == (JsonObject)v);

        public override bool Equals(object obj)
        {
            return obj is JsonValue other && Equals(other);
        }

        public override int GetHashCode() =>
            HashCode.Combine((int) _kind, _number, _object);

        public static bool operator ==(JsonValue left, JsonValue right) => left.Equals(right);
        public static bool operator !=(JsonValue left, JsonValue right) => !left.Equals(right);

        public override string ToString() =>
            Match(nul: () => "null",
                  bit: v => v ? "true" : "false",
                  num: v => JsonSerializer.Serialize(v),
                  str: v => JsonSerializer.Serialize(v),
                  arr: v => JsonSerializer.Serialize(v),
                  obj: v => v.ToString());
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

            public JsonValue Parse(string json) =>
                JsonValue.From(JsonDocument.Parse(json).RootElement);

            public JsonValue String(string value) => JsonValue.String(value);

            public JsonValue Array(IList<JsonValue> values) =>
                JsonValue.Array(values);

            public JsonValue Object(IEnumerable<KeyValuePair<string, JsonValue>> members) =>
                JsonValue.Object(new JsonObject(members.ToList()));

            public bool GetMemberValue(JsonValue obj, string name, out JsonValue value)
            {
                if (obj.Kind != JsonValueKind.Object)
                    throw new ArgumentOutOfRangeException(nameof(obj), obj, null);
                if (!obj.GetObject().TryGetValue(name, out var v))
                {
                    value = default;
                    return false;
                }
                value = v;
                return true;
            }

            public bool GetBooleanValue(JsonValue value) =>
                value.Kind == JsonValueKind.Boolean ? value.GetBoolean() : throw new ArgumentOutOfRangeException(nameof(value), value, null);

            public double GetNumberValue(JsonValue value) =>
                value.Kind == JsonValueKind.Number ? value.GetNumber() : throw new ArgumentOutOfRangeException(nameof(value), value, null);

            public string GetStringValue(JsonValue value) =>
                value.Kind == JsonValueKind.String ? value.GetString() : throw new ArgumentOutOfRangeException(nameof(value), value, null);

            public int GetLength(JsonValue value) =>
                value.Kind switch
                {
                    JsonValueKind.String => value.GetString().Length,
                    JsonValueKind.Array => value.GetArray().Count,
                    JsonValueKind.Object => value.GetObject().Count,
                    _ => throw new ArgumentOutOfRangeException(nameof(value), value, null)
                };

            public JsonValue Index(JsonValue value, int index) =>
                value.Kind == JsonValueKind.Array ? value.GetArray()[index] : throw new ArgumentOutOfRangeException(nameof(value), value, null);

            public IEnumerable<JsonValue> GetArrayValues(JsonValue value) =>
                value.Kind == JsonValueKind.Array ? value.GetArray() : throw new ArgumentOutOfRangeException(nameof(value), value, null);

            public IEnumerable<KeyValuePair<string, JsonValue>> GetObjectMembers(JsonValue value) =>
                value.Kind == JsonValueKind.Object ? value.GetObject() : throw new ArgumentOutOfRangeException(nameof(value), value, null);
        }

        public static T Boolean<T>(this IJsonSystem<T> system, bool value) =>
            value ? system.True : system.False;

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
                    return true;
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

    sealed class CurrentNode : Node
    {
        public static readonly CurrentNode Value = new CurrentNode();

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

        protected BinaryNode(Node left, Node right) =>
            (Left, Right) = (left, right);
    }

    sealed class AndNode : BinaryNode
    {
        public AndNode(Node left, Node right) : base(left, right) {}

        public override T Evaluate<T>(T value, IJsonSystem<T> system)
        {
            var l = Left.Evaluate(value, system);
            return system.IsTruthy(l) && Right.Evaluate(value, system) is {} r ? r : l;
        }
    }

    sealed class OrNode : BinaryNode
    {
        public OrNode(Node left, Node right) : base(left, right) { }

        public override T Evaluate<T>(T value, IJsonSystem<T> system)
            => Left.Evaluate(value, system) is {} l && system.IsTruthy(l) ? l
             : Right.Evaluate(value, system);
    }

    sealed class EqualNode : BinaryNode
    {
        public EqualNode(Node left, Node right) : base(left, right) { }

        public override T Evaluate<T>(T value, IJsonSystem<T> system) =>
            system.Boolean(Left.Evaluate(value, system) is {} l
            && Right.Evaluate(value, system) is {} r
            && system.GetKind(l) switch
               {
                   JsonValueKind.Null =>
                       system.GetKind(r) == JsonValueKind.Null,
                   JsonValueKind.Boolean =>
                       system.GetKind(r) == JsonValueKind.Boolean && system.GetBooleanValue(l) == system.GetBooleanValue(r),
                   JsonValueKind.Number =>
                       system.GetKind(r) == JsonValueKind.Number && Math.Abs(system.GetNumberValue(l) - system.GetNumberValue(r)) < double.Epsilon,
                   JsonValueKind.String =>
                       system.GetKind(r) == JsonValueKind.String && system.GetStringValue(l) == system.GetStringValue(r),
                   JsonValueKind.Array => throw new NotImplementedException(),
                   JsonValueKind.Object => throw new NotImplementedException(),
                       _ => throw new ArgumentOutOfRangeException()
               });
    }

    sealed class SubExpressionNode : BinaryNode
    {
        public SubExpressionNode(Node left, Node right) : base(left, right) {}

        public override T Evaluate<T>(T value, IJsonSystem<T> system) =>
            Right.Evaluate(Left.Evaluate(value, system), system);
    }

    sealed class IndexNode : Node
    {
        public Node Source { get; }
        public int Index { get; }

        public IndexNode(Node source, int index) =>
            (Source, Index) = (source, index);

        public override T Evaluate<T>(T value, IJsonSystem<T> system)
        {
            var result = Source.Evaluate(value, system);
            if (system.GetKind(result) != JsonValueKind.Array)
                return system.Null;
            var length = system.GetLength(result);
            var index = Index is {} i && i >= 0 ? i : length + i;
            return index < 0 || index >= length ? system.Null : system.Index(result, index);
        }
    }

    sealed class ValueProjectionNode : BinaryNode
    {
        public ValueProjectionNode(Node left, Node right) :
            base(left, right) {}

        public override T Evaluate<T>(T value, IJsonSystem<T> system)
        {
            var obj = Left.Evaluate(value, system);
            if (system.GetKind(obj) != JsonValueKind.Object)
                return system.Null;
            var list = new List<T>(system.GetLength(obj));
            foreach (var (_, v) in system.GetObjectMembers(obj))
            {
                var p = Right.Evaluate(v, system);
                if (system.GetKind(p) != JsonValueKind.Null)
                    list.Add(p);
            }
            return system.Array(list);
        }
    }

    sealed class ProjectionNode : BinaryNode
    {
        public ProjectionNode(Node left, Node right) :
            base(left, right) {}

        public override T Evaluate<T>(T value, IJsonSystem<T> system)
        {
            var obj = Left.Evaluate(value, system);
            if (system.GetKind(obj) != JsonValueKind.Array)
                return system.Null;
            var list = new List<T>(system.GetLength(obj));
            foreach (var v in system.GetArrayValues(obj))
            {
                var p = Right.Evaluate(v, system);
                if (system.GetKind(p) != JsonValueKind.Null)
                    list.Add(p);
            }
            return system.Array(list);
        }
    }

    sealed class MultiSelectListNode : Node
    {
        public IReadOnlyList<Node> List { get; }

        public MultiSelectListNode(IReadOnlyList<Node> list) =>
            List = list;

        public override T Evaluate<T>(T value, IJsonSystem<T> system)
        {
            if (system.GetKind(value) == JsonValueKind.Null)
                return value;
            var list = new List<T>(List.Count);
            foreach (var node in List)
                list.Add(node.Evaluate(value, system));
            return system.Array(list);
        }
    }

    sealed class MultiSelectHashNode : Node
    {
        public IReadOnlyList<KeyValuePair<string, Node>> Hash { get; }

        public MultiSelectHashNode(IReadOnlyList<KeyValuePair<string, Node>> hash) =>
            Hash = hash;

        public override T Evaluate<T>(T value, IJsonSystem<T> system)
        {
            if (system.GetKind(value) == JsonValueKind.Null)
                return value;
            var obj = new List<KeyValuePair<string, T>>(Hash.Count);
            foreach (var (name, v) in Hash)
                obj.Add(KeyValuePair.Create(name, v.Evaluate(value, system)));
            return system.Object(obj);
        }
    }

    sealed class FlattenNode : Node
    {
        public Node Source { get; }

        public FlattenNode(Node source) =>
            Source = source;

        public override T Evaluate<T>(T value, IJsonSystem<T> system)
        {
            var source = Source.Evaluate(value, system);
            if (system.GetKind(source) != JsonValueKind.Array)
                return system.Null;
            var list = new List<T>();
            foreach (var item in system.GetArrayValues(source))
            {
                if (system.GetKind(item) == JsonValueKind.Array)
                    list.AddRange(system.GetArrayValues(item));
                else
                    list.Add(item);
            }
            return system.Array(list);
        }
    }

    sealed class TreeProjector : ITreeProjector<Node>
    {
        public string GetString(Node node) => (StringNode)node;

        public Node RawString(string s, int index, int length)
        {
            return new StringNode(Unescape(s.Substring(index + 1, length - 2)));

            static string Unescape(string s)
            {
                StringBuilder sb = null;
                var si = 0;

                while (true)
                {
                    var i = s.IndexOf(@"\'", si, StringComparison.Ordinal);
                    if (i < 0)
                        break;
                    sb ??= new StringBuilder();
                    sb.Append(s, si, i - si).Append('\'');
                    si = i + 2;
                }

                if (sb == null)
                    return s;

                sb.Append(s, si, s.Length - si);
                return sb.ToString();
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
                var si = 0;

                while (true)
                {
                    var i = s.IndexOf('\\', si);
                    if (i < 0)
                        break;
                    var (replacement, skip) = s[i + 1] switch
                    {
                        '\\' => ('\\', 1),
                        '"'  => ('"',  1),
                        '/'  => ('/',  1),
                        'b'  => ('\b', 1),
                        'f'  => ('\f', 1),
                        'n'  => ('\n', 1),
                        'r'  => ('\r', 1),
                        'v'  => ('\v', 1),
                        't'  => ('\t', 1),
                        'u'  => ((char)int.Parse(s.AsSpan(i + 2, 4), NumberStyles.HexNumber), 5),
                        _ => default
                    };
                    sb ??= new StringBuilder();
                    sb.Append(s, si, i - si);
                    si = i + 1;
                    if (skip > 0)
                    {
                        sb.Append(replacement);
                        si += skip;
                    }
                }

                if (sb == null)
                    return s;

                sb.Append(s, si, s.Length - si);
                return sb.ToString();
            }
        }

        public Node Literal(string s, int index, int length) =>
            new LiteralNode(s.Substring(index + 1, length - 2).Replace(@"\`", "`"));

        public Node Current() => CurrentNode.Value;

        public Node Not(Node expression) =>
            new NotNode(expression);

        public Node And(Node left, Node right) =>
            new AndNode(left, right);

        public Node Or(Node left, Node right) =>
            new OrNode(left, right);

        public Node GreaterThan(Node left, Node right)
        {
            throw new NotImplementedException();
        }

        public Node GreaterThanEqual(Node left, Node right)
        {
            throw new NotImplementedException();
        }

        public Node LessThan(Node left, Node right)
        {
            throw new NotImplementedException();
        }

        public Node LessThanEqual(Node left, Node right)
        {
            throw new NotImplementedException();
        }

        public Node Equal(Node left, Node right) =>
            new EqualNode(left, right);

        public Node NotEqual(Node left, Node right)
        {
            throw new NotImplementedException();
        }

        public Node Flatten(Node source) =>
            new FlattenNode(source);

        public Node Field(Node name) => new FieldNode((StringNode)name);

        public Node Identity() => IdentityNode.Value;

        public Node ValueProjection(Node left, Node right) =>
            new ValueProjectionNode(left, right);

        public Node MultiSelectList(Node[] list) =>
            new MultiSelectListNode(list);

        public Node MultiSelectHash(KeyValuePair<string, Node>[] hash) =>
            new MultiSelectHashNode(hash);

        public Node FilterProjection(Node left, Node right, Node condition)
        {
            throw new NotImplementedException();
        }

        public Node Projection(Node left, Node right) =>
            new ProjectionNode(left, right);

        public Node Index(Node source, int index) =>
            new IndexNode(source, index);

        public Node Slice(int? start, int? stop, int? step)
        {
            throw new NotImplementedException();
        }

        public Node Reference()
        {
            throw new NotImplementedException();
        }

        public Node SubExpression(Node left, Node right) =>
            new SubExpressionNode(left, right);

        public Node Pipe(Node left, Node right)
        {
            throw new NotImplementedException();
        }

        public void Function(Node name, Node[] args)
        {
            throw new NotImplementedException();
        }
    }
}
