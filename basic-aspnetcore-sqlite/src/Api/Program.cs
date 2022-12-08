using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;

using Elastic.Apm.SerilogEnricher; 
using Serilog;

namespace EfcoreTest.Api {
    public class Program {
        public static void Main(string[] args) {
            Log.Logger = new LoggerConfiguration().WriteTo.Console().CreateBootstrapLogger();
            
            Log.Information("Starting up!");
            
            try {
              CreateHostBuilder(args).Build().Run();
            
              Log.Information("Stopped cleanly");
              return ;
            } catch (Exception ex) {
              Log.Fatal(ex, "An unhandled exception occured during bootstrapping");
              return ;
            } finally {
              Log.CloseAndFlush();
            }
        }

        public static IHostBuilder CreateHostBuilder(string[] args)
        {
            return Host.CreateDefaultBuilder(args)
                .UseSerilog((context, services, configuration) => configuration
                    .ReadFrom.Configuration(context.Configuration)
                    .ReadFrom.Services(services)
                    .Enrich.FromLogContext()
                    .Enrich.WithProperty("ApplicationName", typeof(Program).Assembly.GetName().Name)
                    .Enrich.WithElasticApmCorrelationInfo()
                    .WriteTo.Console()
		    ).ConfigureWebHostDefaults(webBuilder => {
                    webBuilder.UseStartup<Startup>();
                });
        }
    }
}
