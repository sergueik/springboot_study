namespace Json.Grafana.DataSources.Logic
{
    using System;
    using System.IO;
    using System.Linq;
    using Newtonsoft.Json;

    public static class FileHelper
    {
        public static void SaveJson<T>(string filePath, T jsonData, Formatting formatting = Formatting.None)
        {
            File.WriteAllText(filePath, JsonConvert.SerializeObject(jsonData, formatting));
        }

        public static void CheckDataFiles(string dir)
        {
            if (Directory.Exists(dir))
            {
                if (!System.IO.File.Exists($"{dir}/info.json") || !System.IO.File.Exists($"{dir}/table.json"))
                {
                    throw new Exception($"info.json or table.json not found in dir {dir}, use storedata/set_info");
                }
            }
        }

        public static bool IsDirectoryEmpty(string path)
        {
            return !Directory.EnumerateFileSystemEntries(path).Any();
        }

        public static T GetJson<T>(string filePath)
        {
            T response;
            using (StreamReader r = new StreamReader(filePath))
            {
                string json = r.ReadToEnd();
                response = JsonConvert.DeserializeObject<T>(json);
            }

            return response;
        }
    }
}
