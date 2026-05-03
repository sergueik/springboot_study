using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Utils {
    public class HttpService : HttpServer {
        private List<ServiceModule> modules;

        public HttpService(string ipAddress, int port)
            : base(ipAddress, port) {
            modules = new List<ServiceModule>();
        }

        public void RegisterModule(ServiceModule module) {
            this.modules.Add(module);
        }

        public void RemoveModule(ServiceModule module) {
            this.modules.Remove(module);
        }

        public override void OnDefault(HttpRequest request, HttpResponse response)  {
            base.OnDefault(request, response);
        }

        public override void OnGet(HttpRequest request, HttpResponse response) {
            ServiceRoute route = ServiceRoute.Parse(request);
            ServiceModule module = modules.FirstOrDefault(m => m.SearchRoute(route));
            if (module != null){
                var result = module.ExecuteRoute(route);
            }
        }

        public override void OnPost(HttpRequest request, HttpResponse response) {
            base.OnPost(request, response);
        }
    }
}
