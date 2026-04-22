using System;

namespace Json.Grafana.DataSources.Logic
{
    using System.Collections.Generic;
    using Models;

    public static class GrafanaExtensions
    {
        public static float GetTimeGrafana(this DateTime dateTime)
        {
            float unixTimestamp = (int)(dateTime.Subtract(new DateTime(1970, 1, 1))).TotalSeconds;
            return unixTimestamp * 1000; // grafana werkt met ms
        }

        public static void BoolToNumber(this List<InfoJsonColumn> columns, bool boolToNumber)
        {
            foreach (var column in columns)
            {
                // We kennen geen bools in grafana, dus even opzetten.
                column.Type = column.Type == TypeDataColumn.Bool ? TypeDataColumn.Number : column.Type;
            }
        }
    }
}
