namespace Json.Grafana.DataSources.Controllers
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Net;
    using Logic;
    using Microsoft.AspNetCore.Mvc;
    using Models;
    using Newtonsoft.Json;

    [Route("storedata")]
    [ApiController]
    public class StoreDataController : ControllerBase
    {
        private readonly IPathServices pathServices;

        public StoreDataController(IPathServices pathServices)
        {
            this.pathServices = pathServices;
        }

        [Produces("application/json")]
        [HttpGet]
        [Route("{name}")]
        public ActionResult<GetInfo> Get(string name)
        {
            var response = new GetInfo{Name = name };
            string docPath = pathServices.GetNamePath(name);
            if (Directory.Exists(docPath))
            {
                response.Table = FileHelper.GetJson<List<InfoJsonColumn>>(Path.Combine(docPath, "table.json"));
                response.Info = FileHelper.GetJson<GetInfoJson>(Path.Combine(docPath, "info.json"));
                return response;
            }
            return null;
        }

        [Route("set_info")]
        [HttpPost]
        public IActionResult SetInfo([FromBody] GetInfo value)
        {
            var docPath = pathServices.GetNamePath(value.Name, null,true);
            FileHelper.SaveJson(Path.Combine(docPath, "table.json"), value.Table, Formatting.Indented);
            FileHelper.SaveJson(Path.Combine(docPath, "info.json"), value.Info, Formatting.Indented);
            return StatusCode((int)HttpStatusCode.OK);
        }

        [Route("send_data")]
        [HttpPost]
        public IActionResult SendData([FromBody] SendData data)
        {
            try
            {
                string fullPath = pathServices.CreateDateTimePath(data.Name, true);
                // controle op geldige json
                JsonConvert.DeserializeObject<List<dynamic>>(data.Json_data.ToString());
                FileHelper.SaveJson(Path.Combine(fullPath, "data.json"), data.Json_data);
                return StatusCode((int)HttpStatusCode.OK);
            }
            catch (Exception e)
            {
                Console.WriteLine(e);
                return StatusCode((int)HttpStatusCode.InternalServerError);
            }
        }
    }
}
