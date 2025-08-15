using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Hosting;
using Microsoft.AspNetCore.Http;
using System;
using Utils;

using VaultSharp;
using VaultSharp.V1.AuthMethods.Token;
using VaultSharp.V1.Commons;

// NOTE: avoid .NET 6 minimal APIs encouraged top-level statements code layout
//
namespace CryptoService
{

	public class Server
	{
		private static String saltString = null;
		private static String password = "password";
		private static bool debug = false;
		public static bool Debug { set { debug = value; } }
		private static bool strong = false;
		public static void Main(string[] args)
		{
			var builder = WebApplication.CreateBuilder(args);
			var app = builder.Build();

			app.Use(async (context, next) => {
				Console.WriteLine($"{context.Request.Method} {context.Request.Path}");
				await next();
			});
			app.MapGet("/password", async () => {

				var result = "";
				string vaultUri = "http://127.0.0.1:8200";

				string vaultToken = "YOUR_VAULT_TOKEN"; 

				string secretPath = "secret/data/my-application/config"; 

				try {
					var vaultClientSettings = new VaultClientSettings(vaultUri, new TokenAuthMethodInfo(vaultToken));

					IVaultClient vaultClient = new VaultClient(vaultClientSettings);

					Secret<SecretData> secret = await vaultClient.V1.Secrets.KeyValue.V2.ReadSecretAsync(path: secretPath);

					if (secret?.Data?.Data != null) {
						foreach (var entry in secret.Data.Data) {
							Console.WriteLine($"Key: {entry.Key}, Value: {entry.Value}");
						}
						result = secret.Data.Data["password"] as string;
					} else {
						Console.WriteLine($"Secret at path '{secretPath}' not found or has no data.");
					}
				} catch (Exception ex) {
					Console.WriteLine($"An error occurred: {ex.Message}");
				}
				return Results.Json(new { result = result});
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

