namespace Json.Grafana.DataSources.Controllers
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Linq;
    using Logic;
    using Microsoft.AspNetCore.Mvc;
    using Models;
    using Newtonsoft.Json.Linq;

    // Meer info kijk op https://github.com/grafana/simple-json-datasource/

    [Route("simpeljson")]
    [ApiController]
    public class SimpelJsonController : ControllerBase
    {
        private readonly IPathServices pathServices;

        public SimpelJsonController(IPathServices pathServices)
        {
            this.pathServices = pathServices;
        }

        [HttpGet]
        [Route("")]
        public ActionResult<string> Get()
        {
            return "Hack the planet!";
        }

        [Produces("application/json")]
        [Route("search")]
        [HttpPost]
        public ActionResult<IEnumerable<string>> Search()
        {
            // Set a variable to the Documents path.
            string docPath = pathServices.DirectoryGrafanaJSON;
            Console.WriteLine(docPath);

            var dirPrograms = new DirectoryInfo(docPath);

            var dirs = dirPrograms.EnumerateDirectories()
                .Select(dir => dir.Name);


            return new ActionResult<IEnumerable<string>>(dirs);
        }

        [Produces("application/json")]
        [Route("annotations")]
        [HttpPost]
        public ActionResult<IEnumerable<TimeSerie>> Annotations([FromBody] dynamic value)
        {
            return null;
        }

        public static dynamic GetDefaultValueOfDynamic(TypeDataColumn type)
        {
            if (type == TypeDataColumn.Bool)
            {
                return 0;
            }

            if (type == TypeDataColumn.DateTime)
            {
                return null;
            }


            return string.Empty;
        }

        public static dynamic GetValueOfDynamic(dynamic value)
        {
            if (value is bool b)
            {
                return b ? 1 : 0;
            }

            if (value is DateTime date)
            {
                return date.GetTimeGrafana();
            }
            return value;
        }

        // multi response kan wel met nswag:
        // https://blog.dangl.me/archive/different-response-schemas-in-aspnet-core-swagger-api-definition-with-nswag/
        [Produces("application/json")]
        [Route("query")]        
        [ProducesResponseType(typeof(List<TimeSerie>), 200)] // Helaas werkt dit niet.. :(
        [ProducesResponseType(typeof(List<Table>), 200)]        
        [ProducesResponseType(500)]
        [HttpPost]
        public ActionResult<List<QueryResponse>> Query([FromBody] dynamic value)
        {
            string docPath = pathServices.DirectoryGrafanaJSON;
            dynamic data = value;

            var response = new List<QueryResponse>();

            if (data.targets[0].target == null || string.IsNullOrEmpty(data.targets[0].target.Value))
            {
                // geen target meegegeven, die alle dirs timeseries
                var dirPrograms = new DirectoryInfo(docPath);

                foreach (var enumerateDirectory in dirPrograms.EnumerateDirectories()
                    .Where(b => !FileHelper.IsDirectoryEmpty(b.FullName)).OrderBy(b => b.Name))
                {
                    if (Directory.Exists(enumerateDirectory.FullName) &&
                        System.IO.File.Exists($"{enumerateDirectory.FullName}/info.json"))
                    {
                        response.Add(GetTimeSerie(GetDescription(enumerateDirectory.FullName), enumerateDirectory.FullName));
                    }
                }
            }else
            {
                foreach (var target in data.targets)
                {
                    string name = target.target;
                    var dir = $"{docPath}/{name}";

                    var typeData_target = GetTypeData(dir);

                    switch (typeData_target)
                    {
                        case TypeData.Default:
                            if (target.type == "table")
                            {
                                response.Add(GetTableDefault(dir, GetValueOfDynamic, GetDefaultValueOfDynamic, true));
                            }
                            else
                            {
                                response.Add(GetTimeSerie(GetDescription(dir), dir));
                            }                            
                            break;
                        case TypeData.KeyValue:
                            if (target.type == "table")
                            {
                                var dateData = DateTime.Today;
                                response.Add(GetTableKeyValue(dir, dateData, GetValueOfDynamic, GetDefaultValueOfDynamic, true));
                            }
                            // TODO
                            break;
                    }
                }
            }

            return response;
        }

        private TypeData GetTypeData(string dir)
        {
            if (System.IO.File.Exists($"{dir}/info.json"))
            {
                var infoJSON = FileHelper.GetJson<GetInfoJson>($"{dir}/info.json");
                return infoJSON.Type;
            }

            return TypeData.Default;
        }

        public static Table GetTableKeyValue(string dir, DateTime dateData, Func<dynamic, dynamic> getValueOfDynamic,
            Func<TypeDataColumn, dynamic> getDefaultValueOfDynamic, bool boolToNumber)
        {
            FileHelper.CheckDataFiles(dir);
            var table = new Table {Type = "table", Rows = new List<dynamic>(), Columns = new List<InfoJsonColumn>()};
            var columns = FileHelper.GetJson<List<InfoJsonColumn>>($"{dir}/table.json");
            //We geven alleen dag standen!            
            var keys = new List<string>();
            var todayDir = dateData.ToString("yyyy-MM-dd");
            foreach (var column in columns)
            {
                table.Columns.Add(column);

                var filePath = $"{dir}/{column.JsonValue}/{todayDir}/data.json";
                var items = new Dictionary<string, string>();
                if (System.IO.File.Exists(filePath))
                {
                    items = FileHelper.GetJson<Dictionary<string, string>>(filePath);
                }

                foreach (var item in items)
                {
                    if (!keys.Contains(item.Key.ToLower()))
                    {
                        keys.Add(item.Key.ToLower());
                    }
                }
            }

            // op volgorde zetten
            keys = keys.OrderBy(b => b).ToList();

            foreach (var key in keys)
            {
                var values = new List<dynamic>();
                foreach (var column in columns)
                {
                    if (column.JsonValue.ToLower() == "key")
                    {
                        values.Add(key);
                    }
                    else
                    {
                        var filePath = $"{dir}/{column.JsonValue}/{todayDir}/data.json";
                        var items = new JObject();
                        if (System.IO.File.Exists(filePath))
                        {
                            items = FileHelper.GetJson<JObject>(filePath);
                        }

                        if (items.ContainsKey(key))
                        {
                            values.Add(getValueOfDynamic(items[key]));
                        }
                        else
                        {
                            values.Add(getDefaultValueOfDynamic(column.Type));
                        }
                    }
                }

                table.Rows.Add(values);
            }

            columns.BoolToNumber(boolToNumber);

            return table;
        }

        public static Table GetTableDefault(string dir, Func<dynamic, dynamic> getValueOfDynamic,
            Func<TypeDataColumn, dynamic> getDefaultValueOfDynamic, bool boolToNumber)
        {
            FileHelper.CheckDataFiles(dir);
            var table = new Table { Type = "table", Rows = new List<dynamic>(), Columns = new List<InfoJsonColumn>() };
            // We geven alleen het eerste query object terug bij type = table
            // We weten niet of alle targets wel de zelfde kolommen hebben                        
            // Eerste kolom is altijd een Time kolom
            var timeColum = new InfoJsonColumn();
            timeColum.Text = "Time";
            timeColum.Type = TypeDataColumn.DateTime;
            timeColum.JsonValue = "Time";
            table.Columns.Add(timeColum);
            var columns = FileHelper.GetJson<List<InfoJsonColumn>>($"{dir}/table.json");

            table.Columns.AddRange(columns);            

            var dirPrograms = new DirectoryInfo(dir);
            // laatste gegevens alleen weergeven in table
            var enumerateDirectory = dirPrograms.EnumerateDirectories().Where(b => !FileHelper.IsDirectoryEmpty(b.FullName)).OrderByDescending(b => b.Name).First();
            var dateData = GetDateTime(enumerateDirectory.Name);

            // JObject, we weten niet hoe de kolomen heten real-time
            var items = FileHelper.GetJson<List<JObject>>($"{enumerateDirectory.FullName}/data.json");
            if (items != null)
            {
                foreach (var item in items)
                {
                    var values = new List<dynamic>();
                    foreach (var tableColumn in table.Columns)
                    {
                        // Time kan je halen uit de directory naam
                        values.Add(
                            tableColumn.JsonValue == "Time"
                                ? getValueOfDynamic(dateData)
                                : getValueOfDynamic(item.GetValue(tableColumn.JsonValue)));
                    }

                    table.Rows.Add(values);
                }                    
            }

            columns.BoolToNumber(boolToNumber);

            return table;
        }



        private static DateTime GetDateTime(string directoryName)
        {
            // van directory naam weer terug naar een datetime
            var datetimeSplit = directoryName.Split(" ");
            var dateStrings = datetimeSplit[0].Split("-");
            var timeStrings = datetimeSplit[1].Split("_");
            var dateData = new DateTime(Int32.Parse(dateStrings[0]), Int32.Parse(dateStrings[1]),
                Int32.Parse(dateStrings[2]), Int32.Parse(timeStrings[0]), Int32.Parse(timeStrings[1]), Int32.Parse(timeStrings[2]));
            return dateData;
        }

        private string GetDescription(string dir)
        {
            var info = FileHelper.GetJson<GetInfoJson>($"{dir}/info.json");
            return info.Description;
        }

        private TimeSerie GetTimeSerie(string name, string dir)
        {
            Console.WriteLine(dir);
            var dirPrograms = new DirectoryInfo(dir);
            var floatList = new List<float[]>();
            foreach (var enumerateDirectory in dirPrograms.EnumerateDirectories().Where(b => !FileHelper.IsDirectoryEmpty(b.FullName)).OrderBy(b => b.Name))
            {
                var dateData = GetDateTime(enumerateDirectory.Name);
                var items = FileHelper.GetJson<List<dynamic>>($"{enumerateDirectory}/data.json");

                if (items != null && items.Count != 0)
                {
                    floatList.Add(new[] {items.Count, dateData.GetTimeGrafana() });
                }
                else
                {
                    floatList.Add(new[] { 0, dateData.GetTimeGrafana() });
                }
            }

            return new TimeSerie {Target = name, Datapoints = floatList};
        }
    }
}
