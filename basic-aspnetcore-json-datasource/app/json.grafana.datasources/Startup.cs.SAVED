using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Newtonsoft.Json;

namespace Json.Grafana.DataSources
{
    using Logic;
    using Swashbuckle.AspNetCore.Swagger;

    public class Startup
    {
        private static string title = "Json Grafana DataSources";

        public Startup(IConfiguration configuration)
        {
            Configuration = configuration;
        }

        public IConfiguration Configuration { get; }

        // This method gets called by the runtime. Use this method to add services to the container.
        public void ConfigureServices(IServiceCollection services)
        {
		services
        .AddControllers()
        .AddNewtonsoftJson(options =>
        {
            options.SerializerSettings.NullValueHandling =
                NullValueHandling.Ignore;
        });


            var appSettings = Configuration.GetSection("AppSettings").Get<AppSettings>();
            
            services.AddSingleton<IPathServices>(b => new PathServices(appSettings));
services
    .AddMvc()
    .AddNewtonsoftJson(options =>
    {
        options.SerializerSettings.NullValueHandling =
            NullValueHandling.Ignore;
    });
/*
            services.AddMvc().SetCompatibilityVersion(CompatibilityVersion.Version_2_2).AddJsonOptions(
                options =>
                {
                    options.SerializerSettings.Converters.Add(new Newtonsoft.Json.Converters.StringEnumConverter());
                });
*/
            services.AddSwaggerGen(c =>
            {
                c.SwaggerDoc("v1", new Info { Title = title, Version = "v1" });
            });
        }

        // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
        public void Configure(IApplicationBuilder app, IHostingEnvironment env)
        {
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
            }
            else
            {
                // The default HSTS value is 30 days. You may want to change this for production scenarios, see https://aka.ms/aspnetcore-hsts.
                //app.UseHsts();
            }

            //app.UseHttpsRedirection();
            app.UseMvc();
            app.UseSwagger();

            app.UseSwaggerUI(c =>
            {
                c.SwaggerEndpoint("/swagger/v1/swagger.json", title);
            });
        }
    }
}
