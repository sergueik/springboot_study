using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Hosting;
using Microsoft.AspNetCore.Http;
using System;

// NOTE: avoid .NET 6 minimal APIs encouraged top-level statements code layout
//
namespace CryptoService
{
    public class Server
    {
        public static void Main(string[] args)
        {
            var builder = WebApplication.CreateBuilder(args);
            var app = builder.Build();

            // Optional logging middleware
            app.Use(async (context, next) =>
            {
                Console.WriteLine($"{context.Request.Method} {context.Request.Path}");
                await next();
            });

            // Dummy /encrypt route
            app.MapGet("/encrypt", (string value, string password) =>
            {
                return Results.Json(new { encrypted = "dummy", original = value });
            });

            // Dummy /decrypt route
            app.MapGet("/decrypt", (string value, string password) =>
            {
                return Results.Json(new { decrypted = "dummy", original = value });
            });

            app.Run();
        }
    }
}

