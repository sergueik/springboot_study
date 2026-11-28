using OfficeIMO;
using OfficeIMO.Core;
using OfficeIMO.Access;
using System;
using System.Linq;
using System.Collections.Generic;
using System.Diagnostics;
using Utils;

namespace Program {
	public enum ExtractionTarget  {
		Modules,
		Forms,
		Queries,
		References
	}

	public class Program {
		private static bool debug = false;
		public static bool Debug { set { debug = value; } }
		private static string inputfile = null;
		private static ExtractionTarget[] targets = (ExtractionTarget[]) Enum.GetValues(typeof(ExtractionTarget));

		public  static void Main(string[] args) {

			
			
			if (Debugger.IsAttached) {
				debug = true;
				System.Diagnostics.Debug.AutoFlush = true;
			}
			string line = "check";
			if (Debugger.IsAttached){
				// outside of Debug configuration the Debug.WriteLine() calls are normally removed by compilation
				System.Diagnostics.Debug.WriteLine(line);
			} else {
				Console.Error.WriteLine(line);
			}

			var parseArgs = new ParseArgs(System.Environment.CommandLine);
	
			targets = ParseExtractionTargets(parseArgs, "extract");
			
			if (parseArgs.GetMacro("debug") != String.Empty)
				debug = true;
			if (debug)
				targets = (ExtractionTarget[])Enum.GetValues(typeof(ExtractionTarget));
			else if (parseArgs.GetMacro("extract") != String.Empty)
				targets = ParseExtractionTargets(parseArgs.GetMacro("extract"));
			else { 
				Logger.Log("Missing argument: extract");
				Environment.Exit(1);
			}
			
			AccessDocument source = AccessDocument.Load("application.accdb", new AccessLoadOptions { AccessMode = DocumentAccessMode.ReadOnly });
			// https://github.com/EvotecIT/OfficeIMO/blob/master/OfficeIMO.Access/Model/AccessOpaqueValue.cs
			Logger.Log(String.Format("Opened {0} bytes: {1} streams", source.NativeProperties.Length, source.ApplicationStreams.Count));
			// Console.Error.WriteLine(String.Format("Opened {0} {1}", source.NativeProperties.NativeType, source.ApplicationStreams.Count));
			if (targets.Contains(ExtractionTarget.Modules)) {
				if (source.VbaProject != null && source.VbaProject.Modules.Count > 0) {
					Logger.Log("Modules:");
					foreach (AccessVbaModuleInfo module in source.VbaProject.Modules)
						Logger.Log(module.Source ?? module.SourceLimitation);
					foreach (AccessVbaReferenceInfo reference in source.VbaProject.References)
						Logger.Log(String.Format("{0}: {1}", reference.Name, reference.LibraryId));
				} else {
					Logger.Log("No Modules");
				}
			}
			if (targets.Contains(ExtractionTarget.Forms)) {
				if (source.Forms.Count != 0) {
					Logger.Log("Forms:");
					foreach (AccessApplicationObject form in source.Forms) {
						Logger.Log(String.Format("{0}: {1}", form.Name, (form.Definition != null) ? form.Definition.RecordSource : ""));
						foreach (AccessStorageStream stream in form.Streams)
							Logger.Log(String.Format("{0}: {1} bytes", stream.Path, stream.Payload.GetBytes().Length));
					}
				} else {
					Logger.Log("No Forms");
				}
			}
			if (targets.Contains(ExtractionTarget.Queries)) {
				if (source.Queries.Count != 0) {
					Logger.Log("Queries:");
					foreach (AccessQueryDefinition queryDefinition in source.Queries)
						Logger.Log(String.Format("Query: {0}\n{1}", queryDefinition.Name, (queryDefinition.HasSql ? queryDefinition.Sql : "SQL unavailable")));
				} else {
					Logger.Log("No Queries");
				}
			}
		}

		public static ExtractionTarget[] ParseExtractionTargets(string value) {
			string name = "name";
			var args = new ParseArgs(name + "=" + value);
			return ParseExtractionTargets( args,  name);
		}
		public static ExtractionTarget[] ParseExtractionTargets(ParseArgs args, string name) {
			string value = args.GetMacro(name);

			if (String.IsNullOrEmpty(value))
				return new[] { ExtractionTarget.Modules };

			var result = new List<ExtractionTarget>();

			foreach (string item in value.Split(',')) {
				ExtractionTarget target;

				if (!Enum.TryParse<ExtractionTarget>(
					        item.Trim(), true, out target)
				        || !Enum.IsDefined(typeof(ExtractionTarget), target)) {
					throw new ArgumentException(
						"Invalid extraction target: " + item);
				}

				if (!result.Contains(target))
					result.Add(target);
			}

			return result.ToArray();
		}

	}
}