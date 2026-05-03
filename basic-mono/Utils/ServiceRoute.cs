using System;
using System.Linq;

namespace Utils {
    public class ServiceRoute {
        public RouteMethod Method { get; private set; }
        public string RoutePath { get; private set; }

        public static ServiceRoute Parse(HttpRequest request) {
            var route = new ServiceRoute();
            route.Method = (RouteMethod)Enum.Parse(typeof(RouteMethod), request.Method);
            route.RoutePath = request.URL;
            return route;
        }
    }
}
