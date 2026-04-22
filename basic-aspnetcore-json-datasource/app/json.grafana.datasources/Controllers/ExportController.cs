namespace Json.Grafana.DataSources.Controllers
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Text;
    using Logic;
    using Microsoft.AspNetCore.Mvc;
    using Models;

    [Route("export")]
    [ApiController]
    public class ExportController : ControllerBase
    {
        private readonly IPathServices pathServices;

        public ExportController(IPathServices pathServices)
        {
            this.pathServices = pathServices;
        }

        // TODO Datum als string!
        [HttpGet]
        [Route("json/{name}/{date}")]
        public ActionResult<JsonExport> GetJSON(string name, DateTime? date)
        {
            if (!date.HasValue)
            {
                date = DateTime.Today;
            }

            var response = new JsonExport {Name = name};
            string docPath = pathServices.GetNamePath(name);
            if (Directory.Exists(docPath))
            {                
                response.Info = FileHelper.GetJson<GetInfoJson>(Path.Combine(docPath, "info.json"));
                response.Json_data = GetTable(docPath, response.Info, date.Value);
            }
            
            return response;
        }

        // TODO Datum als string!
        [HttpGet]
        [Route("csv/{name}/{date}")]
        public IActionResult GetCSV(string name, DateTime? date)
        {
            if (!date.HasValue)
            {
                date = DateTime.Today;
            }
            
            string docPath = pathServices.GetNamePath(name);
            var result = new List<string>();

            if (Directory.Exists(docPath))
            {
                var info = FileHelper.GetJson<GetInfoJson>(Path.Combine(docPath, "info.json"));

                // ophalen data
                var data = GetTable(docPath, info, date.Value);

                // Headers
                string csvrow = string.Empty;
                foreach (var header in data.Columns)
                {
                    // waarde toevoegen maar als ; voorkomt in de waarde vervangen voor .
                    // We scheide de waardes op ;
                    csvrow = csvrow + header.Text.Replace(";", ".") + ";";
                }

                // laatste punt-komma weg
                csvrow = csvrow.Remove(csvrow.Length - 1);
                result.Add(csvrow);

                // Alle waarder
                foreach (var row in data.Rows)
                {
                    csvrow = string.Empty;
                    foreach (var keyvalue in row)
                    {
                        // waarde toevoegen maar als ; voorkomt in de waarde vervangen voor .
                        // We scheide de waardes op ;
                        if (keyvalue == null)
                        {
                            csvrow = csvrow + ";";
                        }
                        else
                        {
                            csvrow = csvrow + keyvalue.ToString().Replace(";", ".") + ";";
                        }                        
                    }

                    // laatste punt-komma weg
                    csvrow = csvrow.Remove(csvrow.Length - 1);
                    result.Add(csvrow);
                }

            }

            var report = String.Join(Environment.NewLine, result);
            return DownloadResponse(report, $"Report_{name}_{date.Value:yyyy-MM-dd}.csv");
        }

        private Table GetTable(string docPath, GetInfoJson info, DateTime date)
        {

            // We halen de data net zo op alsof het een uitvraag is van grafana
            if (info.Type == TypeData.KeyValue)
            {
                return SimpelJsonController.GetTableKeyValue(docPath, date, GetValueOfDynamic, GetDefaultValueOfDynamic, false);
            }
            else
            {
                return SimpelJsonController.GetTableDefault(docPath, GetValueOfDynamic, GetDefaultValueOfDynamic, false);
            }
        }

        public static dynamic GetDefaultValueOfDynamic(TypeDataColumn type)
        {
            if (type == TypeDataColumn.Bool)
            {
                dynamic value = false;
                return value;
            }

            if (type == TypeDataColumn.DateTime)
            {
                return null;
            }

            return string.Empty;
        }

        public static dynamic GetValueOfDynamic(dynamic value)
        {
            if (value.Value is DateTime c)
            {
                return c.ToString("yyyy-MM-dd HH:mm:ss");
            }
            return value.Value;
        }

        private IActionResult DownloadResponse(string data_string, string fileName)
        {            
            var content = new MemoryStream(Encoding.UTF8.GetBytes(data_string));
            var contentType = "APPLICATION/octet-stream";
            return File(content, contentType, fileName);
        }
    }
}
