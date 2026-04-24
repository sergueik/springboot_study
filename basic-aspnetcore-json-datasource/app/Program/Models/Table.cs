using System.Collections.Generic;
using Newtonsoft.Json;

namespace Json.Grafana.DataSources.Models
{
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