using OfficeIMO;
using OfficeIMO.Core;
using OfficeIMO.Access;
// https://github.com/commandlineparser/commandline
using CommandLine;
using System;
using System.Linq;
using System.Diagnostics;

namespace Program {
	public enum ExtractionTarget {
		Modules,
		Forms,
		QueriesL,
		References
	}

	public class Program {
		public  static void Main(string[] args) {
			AccessDocument source = AccessDocument.Load("application.accdb", new AccessLoadOptions { AccessMode = DocumentAccessMode.ReadOnly });
			// https://github.com/EvotecIT/OfficeIMO/blob/master/OfficeIMO.Access/Model/AccessOpaqueValue.cs
			Debug.WriteLine(String.Format("Opened {0} bytes: {1} streams", source.NativeProperties.Length, source.ApplicationStreams.Count));
			// Console.Error.WriteLine(String.Format("Opened {0} {1}", source.NativeProperties.NativeType, source.ApplicationStreams.Count));
			if (source.VbaProject != null && source.VbaProject.Modules.Count > 0) {
				Debug.WriteLine("Modules:");
				foreach (AccessVbaModuleInfo module in source.VbaProject.Modules)
					Debug.WriteLine(module.Source ?? module.SourceLimitation);
				foreach (AccessVbaReferenceInfo reference in source.VbaProject.References)
					Debug.WriteLine(String.Format("{0}: {1}", reference.Name, reference.LibraryId));
			} else {
				Debug.WriteLine("No Modules");
			}
			if (source.Forms.Count != 0) {
				Debug.WriteLine("Forms:");
				foreach (AccessApplicationObject form in source.Forms) {
					Debug.WriteLine(String.Format("{0}: {1}", form.Name, (form.Definition != null) ? form.Definition.RecordSource : ""));
					foreach (AccessStorageStream stream in form.Streams)
						Debug.WriteLine(String.Format("{0}: {1} bytes", stream.Path, stream.Payload.GetBytes().Length));
				}
			} else {
				Debug.WriteLine("No Forms");
			}
			if (source.Queries.Count != 0) {
				Debug.WriteLine("Queries:");
				foreach (AccessQueryDefinition queryDefinition in source.Queries)
					Debug.WriteLine(String.Format("Query: {0}\n{1}", queryDefinition.Name, (queryDefinition.HasSql ? queryDefinition.Sql : "SQL unavailable")));
			} else {
				Debug.WriteLine("No Queries");
			}
		}
	}
}