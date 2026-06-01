using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Caching.Memory;
using System.IO;
using Syncfusion.EJ2.PdfViewer;
using System.Text.Json;
using System.Drawing;
using Microsoft.AspNetCore.Cors;
using Microsoft.Extensions.Caching.Distributed;
using Microsoft.Extensions.Configuration;
using System.Net.Http;

namespace ej2_pdfviewer_web_service.Controllers
{
    [Route("api/[controller]")]
    public class PdfViewerController : Controller
    {
        private readonly IHostingEnvironment _hostingEnvironment;
        private IMemoryCache _mCache;
        private IDistributedCache _dCache;
        private IConfiguration _configuration;
        private int _slidingTime = 0;
        string path;
        public PdfViewerController(IMemoryCache memoryCache, IHostingEnvironment hostingEnvironment,IDistributedCache cache, IConfiguration configuration)
        {
            _mCache = memoryCache;
            _dCache = cache;
            _hostingEnvironment = hostingEnvironment;
            _configuration = configuration;
            _slidingTime = int.Parse(_configuration["DOCUMENT_SLIDING_EXPIRATION_TIME"]);
            path = _configuration["DOCUMENT_PATH"];
            //check the document path environment variable value and assign default data folder
            //if it is null.
            path = string.IsNullOrEmpty(path) ? Path.Combine(_hostingEnvironment.ContentRootPath, "Data") : Path.Combine(_hostingEnvironment.ContentRootPath, path);
        }

        [AcceptVerbs("Post")]
        [HttpPost]
        [EnableCors("AllowAllOrigins")]
        [Route("Load")]
        public IActionResult Load([FromBody] Dictionary<string, object> args)
        {
            Dictionary<string, string> jsonObject = args.ToDictionary(k => k.Key, k => k.Value?.ToString());
            PdfRenderer pdfviewer;
            if (Startup.isRedisCacheEnable)
                pdfviewer = new PdfRenderer(_mCache, _dCache, _slidingTime);
            else
                pdfviewer = new PdfRenderer(_mCache,_slidingTime);
            MemoryStream stream = new MemoryStream();
            object jsonResult = new object();
            if (jsonObject != null && jsonObject.ContainsKey("document"))
            {
                string documentName = jsonObject["document"];
                if (bool.Parse(jsonObject["isFileName"]))
                {
                    string documentPath = GetDocumentPath(documentName);
                    if (!string.IsNullOrEmpty(documentPath))
                    {
                        byte[] bytes = System.IO.File.ReadAllBytes(documentPath);
                        stream = new MemoryStream(bytes);
                    }
                    else
                    {
                        bool result = Uri.TryCreate(documentName, UriKind.Absolute, out Uri uriResult)
      && (uriResult.Scheme == Uri.UriSchemeHttp || uriResult.Scheme == Uri.UriSchemeHttps);
                        if (result)
                        {
                            stream = GetDocumentFromURL(documentName).Result;
                            if(stream!=null)
                            stream.Position = 0;
                            else
                                return this.Content(documentName + " is not a PDF document");
                        }else
                            return this.Content(documentName + " is not found");
                    }
                }
                else
                {
                    byte[] bytes = Convert.FromBase64String(documentName);
                    stream = new MemoryStream(bytes);
                }
            }
            jsonResult = pdfviewer.Load(stream, jsonObject);
            return Content(JsonSerializer.Serialize(jsonResult));
        }

     async Task<MemoryStream> GetDocumentFromURL(string url)
        {
            var client = new HttpClient();;
            var response = await client.GetAsync(url);
            var rawStream = await response.Content.ReadAsStreamAsync();
            if(response.IsSuccessStatusCode && response.Content.Headers.ContentType.MediaType == "application/pdf")
            {
                MemoryStream docStream = new MemoryStream();
                rawStream.CopyTo(docStream);
                return docStream;
            }
            else { return null; }
        }

        [AcceptVerbs("Post")]
        [HttpPost]
        [EnableCors("AllowAllOrigins")]
        [Route("Bookmarks")]
        public IActionResult Bookmarks([FromBody] Dictionary<string, object> args)
        {
            Dictionary<string, string> jsonObject = args.ToDictionary(k => k.Key, k => k.Value?.ToString());
            PdfRenderer pdfviewer;
            if (Startup.isRedisCacheEnable)
                pdfviewer = new PdfRenderer(_mCache, _dCache, _slidingTime);
            else
                pdfviewer = new PdfRenderer(_mCache, _slidingTime);
            object jsonResult = pdfviewer.GetBookmarks(jsonObject);
            return Content(JsonSerializer.Serialize(jsonResult));
        }

        [AcceptVerbs("Post")]
        [HttpPost]
        [EnableCors("AllowAllOrigins")]
        [Route("RenderPdfPages")]
        public IActionResult RenderPdfPages([FromBody] Dictionary<string, object> args)
        {
            Dictionary<string, string> jsonObject = args.ToDictionary(k => k.Key, k => k.Value?.ToString());
            PdfRenderer pdfviewer;
            if (Startup.isRedisCacheEnable)
                pdfviewer = new PdfRenderer(_mCache, _dCache, _slidingTime);
            else
                pdfviewer = new PdfRenderer(_mCache, _slidingTime);
            object jsonResult = pdfviewer.GetPage(jsonObject);
            return Content(JsonSerializer.Serialize(jsonResult));
        }

        [AcceptVerbs("Post")]
        [HttpPost]
        [EnableCors("AllowAllOrigins")]
        [Route("RenderPdfTexts")]
        //Post action for processing the PDF texts  
        public IActionResult RenderPdfTexts([FromBody] Dictionary<string, object> args)
        {
            Dictionary<string, string> jsonObject = args.ToDictionary(k => k.Key, k => k.Value?.ToString());
            PdfRenderer pdfviewer;
            if (Startup.isRedisCacheEnable)
                pdfviewer = new PdfRenderer(_mCache, _dCache, _slidingTime);
            else
                pdfviewer = new PdfRenderer(_mCache, _slidingTime);
            object jsonResult = pdfviewer.GetDocumentText(jsonObject);
            return Content(JsonSerializer.Serialize(jsonResult));
        }

        [AcceptVerbs("Post")]
        [HttpPost]
        [EnableCors("AllowAllOrigins")]
        [Route("RenderAnnotationComments")]
        public IActionResult RenderAnnotationComments([FromBody] Dictionary<string, object> args)
        {
            Dictionary<string, string> jsonObject = args.ToDictionary(k => k.Key, k => k.Value?.ToString());
            PdfRenderer pdfviewer;
            if (Startup.isRedisCacheEnable)
                pdfviewer = new PdfRenderer(_mCache, _dCache, _slidingTime);
            else
                pdfviewer = new PdfRenderer(_mCache, _slidingTime);
            object jsonResult = pdfviewer.GetAnnotationComments(jsonObject);
            return Content(JsonSerializer.Serialize(jsonResult));
        }

        [AcceptVerbs("Post")]
        [HttpPost]
        [EnableCors("AllowAllOrigins")]
        [Route("Unload")]
        public IActionResult Unload([FromBody] Dictionary<string, object> args)
        {
            Dictionary<string, string> jsonObject = args.ToDictionary(k => k.Key, k => k.Value?.ToString());
            PdfRenderer pdfviewer;
            if (Startup.isRedisCacheEnable)
                pdfviewer = new PdfRenderer(_mCache, _dCache, _slidingTime);
            else
                pdfviewer = new PdfRenderer(_mCache, _slidingTime);
            pdfviewer.ClearCache(jsonObject);
            return this.Content("Document cache is cleared");
        }

        [AcceptVerbs("Post")]
        [HttpPost]
        [EnableCors("AllowAllOrigins")]
        [Route("RenderThumbnailImages")]
        public IActionResult RenderThumbnailImages([FromBody] Dictionary<string, object> args)
        {
            Dictionary<string, string> jsonObject = args.ToDictionary(k => k.Key, k => k.Value?.ToString());
            PdfRenderer pdfviewer;
            if (Startup.isRedisCacheEnable)
                pdfviewer = new PdfRenderer(_mCache, _dCache, _slidingTime);
            else
                pdfviewer = new PdfRenderer(_mCache, _slidingTime);
            object result = pdfviewer.GetThumbnailImages(jsonObject);
            return Content(JsonSerializer.Serialize(result));
        }

        [HttpPost]
        [EnableCors("AllowAllOrigins")]
        [Route("Download")]
        public IActionResult Download([FromBody] Dictionary<string, object> args)
        {
            Dictionary<string, string> jsonObject = args.ToDictionary(k => k.Key, k => k.Value?.ToString());
            PdfRenderer pdfviewer;
            if (Startup.isRedisCacheEnable)
                pdfviewer = new PdfRenderer(_mCache, _dCache, _slidingTime);
            else
                pdfviewer = new PdfRenderer(_mCache, _slidingTime);
            string documentBase = pdfviewer.GetDocumentAsBase64(jsonObject);
            return Content(documentBase);
        }

        [HttpPost]
        [EnableCors("AllowAllOrigins")]
        [Route("SaveUrl")]
        public async Task<IActionResult> SaveUrl([FromBody] Dictionary<string, object> args)
        {
            Dictionary<string, string> jsonObject = args.ToDictionary(k => k.Key, k => k.Value?.ToString());
            PdfRenderer pdfviewer;
            if (Startup.isRedisCacheEnable)
                pdfviewer = new PdfRenderer(_mCache, _dCache, _slidingTime);
            else
                pdfviewer = new PdfRenderer(_mCache, _slidingTime);
            try
            {
                HttpClient client = new HttpClient();
                Dictionary<string, string> RequestDetails = new Dictionary<string, string>();
                RequestDetails.Add("Url", jsonObject["RequestedUrl"]);
                RequestDetails.Add("base64", jsonObject["base64Data"]);
                string jsonString = JsonSerializer.Serialize(RequestDetails);
                StringContent Data = new StringContent(jsonString);
                string UriAddress = jsonObject["UriAddress"];
                var result = await client.PutAsync(UriAddress, Data);
                if (result.StatusCode.ToString() == "OK")
                {
                    return Content("Document saved successfully!");
                }
                else
                {
                    return Content("Failed to save the document!");
                }
            } catch (Exception exception)
            {
                return Content(exception.Message);
            }
        }

        [AcceptVerbs("Post")]
        [HttpPost]
        [EnableCors("AllowAllOrigins")]
        [Route("PrintImages")]
        public IActionResult PrintImages([FromBody] Dictionary<string, object> args)
        {
            Dictionary<string, string> jsonObject = args.ToDictionary(k => k.Key, k => k.Value?.ToString());
            PdfRenderer pdfviewer;
            if (Startup.isRedisCacheEnable)
                pdfviewer = new PdfRenderer(_mCache, _dCache, _slidingTime);
            else
                pdfviewer = new PdfRenderer(_mCache, _slidingTime);
            object pageImage = pdfviewer.GetPrintImage(jsonObject);
            return Content(JsonSerializer.Serialize(pageImage));
        }

        [AcceptVerbs("Post")]
        [HttpPost]
        [EnableCors("AllowAllOrigins")]
        [Route("ExportAnnotations")]
        public IActionResult ExportAnnotations([FromBody] Dictionary<string, object> args)
        {
            Dictionary<string, string> jsonObject = args.ToDictionary(k => k.Key, k => k.Value?.ToString());
            PdfRenderer pdfviewer;
            if (Startup.isRedisCacheEnable)
                pdfviewer = new PdfRenderer(_mCache, _dCache, _slidingTime);
            else
                pdfviewer = new PdfRenderer(_mCache, _slidingTime);
            string jsonResult = pdfviewer.ExportAnnotation(jsonObject);
            return Content(jsonResult);
        }

        [AcceptVerbs("Post")]
        [HttpPost]
        [EnableCors("AllowAllOrigins")]
        [Route("ImportAnnotations")]
        public IActionResult ImportAnnotations([FromBody] Dictionary<string, object> args)
        {
            Dictionary<string, string> jsonObject = args.ToDictionary(k => k.Key, k => k.Value?.ToString());
            PdfRenderer pdfviewer;
            if (Startup.isRedisCacheEnable)
                pdfviewer = new PdfRenderer(_mCache, _dCache, _slidingTime);
            else
                pdfviewer = new PdfRenderer(_mCache, _slidingTime);
            string jsonResult = string.Empty;
            object JsonResult;
            if (jsonObject != null && jsonObject.ContainsKey("fileName"))
            {
                string documentPath = GetDocumentPath(jsonObject["fileName"]);
                if (!string.IsNullOrEmpty(documentPath))
                {
                    jsonResult = System.IO.File.ReadAllText(documentPath);
                }
                else
                {
                    return this.Content(jsonObject["document"] + " is not found");
                }
            } 
            else
            {
                string extension = Path.GetExtension(jsonObject["importedData"]);
                if (extension != ".xfdf")
                {
                    JsonResult = pdfviewer.ImportAnnotation(jsonObject);
                    return Content(JsonSerializer.Serialize(JsonResult));
                }
                else
                {
                    string documentPath = GetDocumentPath(jsonObject["importedData"]);
                    if (!string.IsNullOrEmpty(documentPath))
                    {
                        byte[] bytes = System.IO.File.ReadAllBytes(documentPath);
                        jsonObject["importedData"] = Convert.ToBase64String(bytes);
                        JsonResult = pdfviewer.ImportAnnotation(jsonObject);
                        return Content(JsonSerializer.Serialize(JsonResult));
                    }
                    else
                    {
                        return this.Content(jsonObject["document"] + " is not found");
                    }
                }

            }
            return Content(jsonResult);
        }

        [AcceptVerbs("Post")]
        [HttpPost]
        [EnableCors("AllowAllOrigins")]
        [Route("ExportFormFields")]
        public IActionResult ExportFormFields([FromBody] Dictionary<string, object> args)
        {
            Dictionary<string, string> jsonObject = args.ToDictionary(k => k.Key, k => k.Value?.ToString());
            PdfRenderer pdfviewer;
            if (Startup.isRedisCacheEnable)
                pdfviewer = new PdfRenderer(_mCache, _dCache, _slidingTime);
            else
                pdfviewer = new PdfRenderer(_mCache, _slidingTime);
            string jsonResult = pdfviewer.ExportFormFields(jsonObject);
            return Content(jsonResult);
        }
        [AcceptVerbs("Post")]
        [HttpPost]
        [EnableCors("AllowAllOrigins")]
        [Route("ImportFormFields")]
        public IActionResult ImportFormFields([FromBody] Dictionary<string, object> args)
        {
            Dictionary<string, string> jsonObject = args.ToDictionary(k => k.Key, k => k.Value?.ToString());
            PdfRenderer pdfviewer;
            if (Startup.isRedisCacheEnable)
                pdfviewer = new PdfRenderer(_mCache, _dCache, _slidingTime);
            else
                pdfviewer = new PdfRenderer(_mCache, _slidingTime);
            object jsonResult = pdfviewer.ImportFormFields(jsonObject);
            return Content(JsonSerializer.Serialize(jsonResult));
        }


        // GET api/values
        [HttpGet]
        public IEnumerable<string> Get()
        {
            return new string[] { "value1", "value2" };
        }

        private string GetDocumentPath(string document)
        {
            if (!System.IO.File.Exists(document))
            {
                string documentPath = Path.Combine(path, document);
                if (System.IO.File.Exists(documentPath))
                    return documentPath;
                else
                    return string.Empty;
            }
            else
                return document;
        }
    }
}
