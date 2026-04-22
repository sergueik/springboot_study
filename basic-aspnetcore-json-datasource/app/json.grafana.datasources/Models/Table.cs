namespace Json.Grafana.DataSources.Models
{
    using System.Collections.Generic;
    using Newtonsoft.Json;

    public class Table : QueryResponse
    {
        [JsonProperty(PropertyName = "columns")]
        public List<InfoJsonColumn> Columns { get; set; }

        [JsonProperty(PropertyName = "rows")]
        public List<dynamic> Rows { get; set; }

        [JsonProperty(PropertyName = "type")]
        public string Type { get; set; }
    }
}