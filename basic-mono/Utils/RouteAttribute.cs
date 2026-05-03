using System;
using System.Linq;

namespace Utils {
    [AttributeUsage(AttributeTargets.Method)]
    class RouteAttribute:Attribute {
        public RouteMethod Method { get; set; }
        public string RoutePath { get; set; }
    }
}
