using System.Diagnostics;
using System.Diagnostics.Metrics;

namespace WeatherForecast.Web.Api;

public static class SharedTelemetryUtilities
{
    public static string MeterName = "Weather.Forecast.Meter";
    public static string TracerName = "Weather.Forecast.Tracing";
    private static readonly Meter Meter = new(MeterName, "1.0");
    public static ActivitySource Writer = null;
        
    public static void Init()
    {
        Writer = new ActivitySource(TracerName,"1.0.0");
        var listener = new ActivityListener()
        {
            ShouldListenTo = _ => true,
            Sample = (ref ActivityCreationOptions<ActivityContext> _) => ActivitySamplingResult.AllData
        };
        ActivitySource.AddActivityListener(listener);
    }
        
    public static void InitCounters()
    {
        RequestCounter = Meter.CreateCounter<long>("data.request_counter");
    }

    public static Counter<long> RequestCounter { get; set; }
}