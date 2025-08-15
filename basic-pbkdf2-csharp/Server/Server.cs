using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Hosting;
using Microsoft.AspNetCore.Http;
using System;
using Utils;

// NOTE: avoid .NET 6 minimal APIs encouraged top-level statements code layout
//
namespace CryptoService {

    public class Server {
		private static String saltString = null;
		private static String password = "password";
		private static bool debug = false;
		public static bool Debug { set { debug = value; } }
		private static bool strong = false;
        public static void Main(string[] args) {
            var builder = WebApplication.CreateBuilder(args);
            var app = builder.Build();

            app.Use(async (context, next) => {
                Console.WriteLine($"{context.Request.Method} {context.Request.Path}");
                await next();
            });

            app.MapGet("/encrypt", (string value, string password) => {
			    var result = Encrypt(value, password,saltString);
                return Results.Json(new { result = result, original = value });
            });

            app.MapGet("/decrypt", (string value, string password) => {
			    var result = Decrypt(value, password);
                return Results.Json(new { result = result, original = value });
            });

            app.Run();
        }
		public static string Decrypt(String payloadString, String passwordString) {
			var aes = new AES();
			aes.Strong = strong;
			aes.Debug = debug;
			var decrypted = aes.Decrypt(payloadString, passwordString);        
			Console.Error.WriteLine((debug) ? "decrypted: " + decrypted : decrypted);
			return decrypted;
		}

		public static string Encrypt(String payloadString, String passwordString, String saltString) {
			var aes = new AES();
			aes.Strong = strong;
			aes.Debug = debug;
			var encrypted = aes.Encrypt(payloadString, passwordString, saltString);
			Console.Error.WriteLine((debug) ? "encrypted: " + encrypted : encrypted);
			return encrypted;
		}
    }
}

