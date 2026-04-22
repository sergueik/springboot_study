namespace Json.Grafana.DataSources.Models
{
    using System.Collections.Generic;
    using System.Runtime.Serialization;
    using Newtonsoft.Json;

    public class SendData
    {
        [JsonProperty(PropertyName = "name")]
        public string Name { get; set; }

        [JsonProperty(PropertyName = "json_data")]
        public dynamic Json_data { get; set; }
    }

    public class SendKeyValueData : SendData
    {
        [JsonProperty(PropertyName = "subject")]
        public string Subject { get; set; }
    }

    public class GetInfo
    {
        [JsonProperty(PropertyName = "name")]
        public string Name { get; set; }

        [JsonProperty(PropertyName = "info")]
        public GetInfoJson Info { get; set; }

        [JsonProperty(PropertyName = "table")]
        public List<InfoJsonColumn> Table { get; set; }
    }

    public class JsonExport
    {
        [JsonProperty(PropertyName = "name")]
        public string Name { get; set; }

        [JsonProperty(PropertyName = "json_data")]
        public Table Json_data { get; set; }

        [JsonProperty(PropertyName = "info")]
        public GetInfoJson Info { get; set; }
    }

    public class GetInfoJson
    {
        [JsonProperty(PropertyName = "description")]
        public string Description { get; set; }

        [JsonProperty(PropertyName = "type")]
        public TypeData Type { get; set; }
    }

    public enum TypeData
    {
        [EnumMember(Value = "default")]
        Default,
        [EnumMember(Value = "key_value")]
        KeyValue
    }

    public enum TypeDataColumn
    {
        [EnumMember(Value = "bool")] // word omgezet naar 0 en 1 voor grafana 
        Bool,
        [EnumMember(Value = "time")]
        DateTime,
        [EnumMember(Value = "string")]
        String,
        [EnumMember(Value = "number")]
        Number
    }

    public class InfoJsonColumn
    {
        [JsonProperty(PropertyName = "jsonvalue")]
        public string JsonValue { get; set; }

        [JsonProperty(PropertyName = "type")]
        public TypeDataColumn Type { get; set; }

        [JsonProperty(PropertyName = "text")]
        public string Text { get; set; }
    }
}
