using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Routing;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.AspNetCore.ResponseCompression;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Options;
using Microsoft.AspNetCore.Antiforgery;
using Microsoft.AspNetCore.Http;

namespace ej2_pdfviewer_web_service
{
    public class Startup
    {
        public Startup(IConfiguration configuration)
        {
            Configuration = configuration;
        }

        public IConfiguration Configuration { get; }
        internal static bool isRedisCacheEnable = false;
        // This method gets called by the runtime. Use this method to add services to the container.
        public void ConfigureServices(IServiceCollection services)
        {
            services.AddMemoryCache();
            services.AddMvc(endPoint => endPoint.EnableEndpointRouting = false);            
            string redisCacheConnectionString = Configuration["REDIS_CACHE_CONNECTION_STRING"];
            if (redisCacheConnectionString != null && redisCacheConnectionString != string.Empty)
            {
                
                services.AddDistributedRedisCache(options =>
                {
                    options.Configuration = redisCacheConnectionString;
                });
                isRedisCacheEnable = true;
            }
            services.AddCors(o => o.AddPolicy("AllowAllOrigins", builder =>
            {
                builder.AllowAnyOrigin()
                       .AllowAnyMethod()
                       .AllowAnyHeader();
            }));            
            services.Configure<GzipCompressionProviderOptions>(options => options.Level = System.IO.Compression.CompressionLevel.Optimal);
            services.AddResponseCompression();
        }

        // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
        public void Configure(IApplicationBuilder app, IHostingEnvironment env)
        {
            string license_key = Configuration["SYNCFUSION_LICENSE_KEY"];
            if (license_key != null && license_key != string.Empty)
                Syncfusion.Licensing.SyncfusionLicenseProvider.RegisterLicense(license_key);
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
            }           
            app.UseResponseCompression();
            app.UseCors("AllowAllOrigins");
            app.UseMvc(routes =>
            {
                routes.MapRoute("default", "{api}/{controller}/{action}/{id?}");
            });            
        }
    }
}
