namespace Json.Grafana.DataSources.Models
{
    using System.Collections.Generic;
    using Newtonsoft.Json;

    public class TimeSerie : QueryResponse
    {
        [JsonProperty(PropertyName = "target")]
        public string Target { get; set; }

        [JsonProperty(PropertyName = "datapoints")]
        public List<float[]> Datapoints { get; set; }
    }
}
