using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Utils {
	public class BaseHeader {
		public string Body { get; set; }
		public Encoding Encoding { get; set; }
		public string Content_Type { get; set; }
		public string Content_Length { get; set; }
		public string Content_Encoding { get; set; }
		public string ContentLanguage { get; set; }
		private Dictionary<string, string> headers = new Dictionary<string, string>();
		public Dictionary<string, string> Headers { 
			get { return headers;}
			set { headers = value;  }
		}
		protected string GetHeaderByKey(Enum header) {
			var fieldName = header.GetDescription();
			if (fieldName == null)
				return null;
			return (Headers.ContainsKey(fieldName)) ? Headers[fieldName]: null;
		}

		protected string GetHeaderByKey(string fieldName) {
			if (string.IsNullOrEmpty(fieldName))
				return null;
			return (Headers.ContainsKey(fieldName)) ? Headers[fieldName]: null;
		}

		protected void SetHeaderByKey(Enum header, string value) {
			var fieldName = header.GetDescription();
			if (fieldName == null)
				return;
			if (!Headers.ContainsKey(fieldName))
				Headers.Add(fieldName, value);
			Headers[fieldName] = value;
		}

		protected void SetHeaderByKey(string fieldName, string value)
		{
			if (string.IsNullOrEmpty(fieldName))
				return;
			if (!Headers.ContainsKey(fieldName))
				Headers.Add(fieldName, value);
			Headers[fieldName] = value;
		}
	}
}
