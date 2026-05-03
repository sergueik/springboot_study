using System;
using System.Linq;

namespace Utils {
    public interface IServer {
        void OnGet(HttpRequest request, HttpResponse response);
        void OnPost(HttpRequest request, HttpResponse response);
       	void OnDefault(HttpRequest request, HttpResponse response);
    }
}
