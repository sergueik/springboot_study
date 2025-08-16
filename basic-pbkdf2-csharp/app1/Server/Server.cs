using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Hosting;
using Microsoft.AspNetCore.Http;
using System.IO;
using System;
using Utils;

using VaultSharp;
using VaultSharp.V1.AuthMethods.Token;
using VaultSharp.V1.Commons;

using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Options;


// NOTE: avoid .NET 6 minimal APIs encouraged top-level statements code layout
//
namespace CryptoService
{
	public class VaultConfig
	{
		public string EnvironmentName { get; set; } = string.Empty;
		public string Uri { get; set; }
		public string SecretPath { get; set; }
		public string Token { get; set; } = string.Empty;
	}
	public class Server
	{
		private static String saltString = null;
		private static String password = "password";
		private static bool debug = false;
		public static bool Debug { set { debug = value; } }
		private static bool strong = false;
		private static VaultConfig VaultSettings;
		
		public static void Main(string[] args)
		{
			var env = Environment.GetEnvironmentVariable("DOTNET_ENVIRONMENT") ?? "Development";
			var fileName = $"appsettings.{env}.json";
			var builder = WebApplication.CreateBuilder(args);

			// var configurationBuilder = new ConfigurationBuilder().SetBasePath(Directory.GetCurrentDirectory()).AddJsonFile("appsettings.json", optional: true).AddJsonFile($"appsettings.{env}.json", optional: true);
			// discard
			
			var configurationBuilder = builder.Configuration;
			
			builder.Services.Configure<VaultConfig>(options => {
				builder.Configuration.GetSection("VaultConfig").Bind(options);

				// enforce setting from runtime environment
				options.EnvironmentName = env;
			});

			builder.Services.Configure<VaultConfig>(
				builder.Configuration.GetSection("VaultConfig")
			);

			builder.Services.PostConfigure<VaultConfig>(options => {
				options.EnvironmentName = env; // builder.Environment.EnvironmentName;
			});
			// IConfiguration configuration = configurationBuilder.Build();
			IConfiguration configuration = builder.Configuration;
			VaultSettings = configuration.GetSection("Vault").Get<VaultConfig>();

			Console.WriteLine($"Vault URI: {VaultSettings.Uri}");
			Console.WriteLine($"Env: {VaultSettings.EnvironmentName}");
			var app = builder.Build();

			app.Use(async (context, next) => {
				Console.WriteLine($"{context.Request.Method} {context.Request.Path}");
				await next();
			});
			app.MapGet("/password", async () => {

				// var vaultUri = builder.Configuration["Vault:Uri"] ?? "http://vault:8200";
				// var vaultToken = builder.Configuration["Vault:Token"] ?? throw new ArgumentNullException("Vault:Token");
				// var secretPath = builder.Configuration["Vault:SecretPath"] ?? "secret/data/myapp";

				string result = "";
				string vaultUri = VaultSettings.Uri; // "http://app2:8200/";
				string vaultToken = VaultSettings.Token; // "dmF1bHQgdG9rZW4=";
				string secretPath = VaultSettings.SecretPath; // "app1/config"; 

				try {
					var vaultClientSettings = new VaultClientSettings(vaultUri, new TokenAuthMethodInfo(vaultToken));

					IVaultClient vaultClient = new VaultClient(vaultClientSettings);

					Console.WriteLine($"getting secret path {secretPath} with vault token {vaultToken}");

					Secret<SecretData> secret = await vaultClient.V1.Secrets.KeyValue.V2.ReadSecretAsync(
						                            path: secretPath,
						                            mountPoint: "secret" // KV mount name
					                            );
					if (secret?.Data?.Data != null) {
						foreach (var entry in secret.Data.Data) {
							Console.WriteLine($"Key: {entry.Key}, Value: {entry.Value}");
						}
						// NOTE: if password is boxed, "as string" will return null
						// result = secret.Data.Data["password"] as string;		
						result = secret.Data.Data["password"]?.ToString();
					} else {
						Console.WriteLine($"Secret at path '{secretPath}' not found or has no data.");
					}
				} catch (VaultSharp.Core.VaultApiException ex) {
					Console.WriteLine($"Message: {ex.Message}");
					Console.WriteLine($"Stack Trace:\n{ex.StackTrace}");

				} catch (Exception ex) {
					Console.WriteLine($"A general exception {ex.GetType()} occurred: {ex.Message}");
				}
				return Results.Json(new { result = result });
			});


			app.MapGet("/encrypt", (string value, string password) => {
				var result = Encrypt(value, password, saltString);
				return Results.Json(new { result = result, original = value });
			});

			app.MapGet("/decrypt", (string value, string password) => {
				var result = Decrypt(value, password);
				return Results.Json(new { result = result, original = value });
			});

			app.Run();
		}
		public static string Decrypt(String payloadString, String passwordString)
		{
			var aes = new AES();
			aes.Strong = strong;
			aes.Debug = debug;
			var decrypted = aes.Decrypt(payloadString, passwordString);        
			Console.Error.WriteLine((debug) ? "decrypted: " + decrypted : decrypted);
			return decrypted;
		}

		public static string Encrypt(String payloadString, String passwordString, String saltString)
		{
			var aes = new AES();
			aes.Strong = strong;
			aes.Debug = debug;
			var encrypted = aes.Encrypt(payloadString, passwordString, saltString);
			Console.Error.WriteLine((debug) ? "encrypted: " + encrypted : encrypted);
			return encrypted;
		}
	}
}

