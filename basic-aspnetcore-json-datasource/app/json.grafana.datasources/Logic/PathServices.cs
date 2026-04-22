namespace Json.Grafana.DataSources.Logic
{
    using System;
    using System.IO;

    public class PathServices : IPathServices
    {
        public static AppSettings Settings;
        public string DirectoryGrafanaJSON => Settings.DirectoryGrafanaJSON;


        public PathServices(AppSettings settings)
        {
            Settings = settings;
        }

        public string GetNamePath(string name, string subject, bool create)
        {
            if (name.StartsWith('.') || name.Contains('/'))
            {
                throw new Exception("invalid name");
            }
            string docPath = Path.Combine(DirectoryGrafanaJSON, name);
            if (create)
            {
                if (!Directory.Exists(docPath))
                {
                    Directory.CreateDirectory(docPath);
                }
            }
            if (subject != null)
            {
                if ((subject.StartsWith('.') || subject.Contains('/')))
                {
                    throw new Exception("invalid subject");
                }
                docPath = Path.Combine(docPath, subject);
                if (create)
                {
                    if (!Directory.Exists(docPath))
                    {
                        Directory.CreateDirectory(docPath);
                    }
                }
            }

            return docPath;
        }

        public string CreateDateTimePath(string name, bool create = false)
        {
            string docPath = GetNamePath(name, null,true);
            // create dir met data
            var datetimeDir = DateTime.Now.ToString("yyyy-MM-dd HH_mm_ss");
            var fullPath = Path.Combine(docPath, datetimeDir);
            Directory.CreateDirectory(fullPath);
            return fullPath;
        }

        public string CreateToDayPath(string name, string subject, bool create = false)
        {
            string docPath = GetNamePath(name, subject, true);
            // create dir met data
            var datetimeDir = DateTime.Now.ToString("yyyy-MM-dd");
            var fullPath = Path.Combine(docPath, datetimeDir);
            if (!Directory.Exists(fullPath))
            {
                Directory.CreateDirectory(fullPath);
            }
            return fullPath;
        }
    }

    public interface IPathServices
    {
        string DirectoryGrafanaJSON { get; }

        string GetNamePath(string name, string subject = null, bool create = false);

        string CreateDateTimePath(string name, bool create = false);

        string CreateToDayPath(string name, string subject, bool create = false);
    }
}
