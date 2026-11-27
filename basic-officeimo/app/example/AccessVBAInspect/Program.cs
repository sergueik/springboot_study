using OfficeIMO;
using OfficeIMO.Access;
using OfficeIMO.Core;

using System.CommandLine;
using System.CommandLine.Parsing;
using System.Linq;

namespace AccessVBAInspect {
  public enum ExtractionTarget {
    Modules,
    Forms,
    Queries,
    References
  }

	public class Program {

		public static int InspectFiles(FileInfo[] files, ExtractionTarget[] targets) {
			var exitCode = 0;
			foreach(FileInfo file in files) {
				try {
					using (AccessDocument source = AccessDocument.Load(file.FullName, new AccessLoadOptions { AccessMode = DocumentAccessMode.ReadOnly })) {
						if (source is null){
							Console.Error.WriteLine(String.Format("Source file {0} not read", file.FullName));
							exitCode = 1;
							continue;
						}
						InspectDocument(source, targets);
					}
				} catch (InvalidDataException e) {
					throw new InvalidOperationException( "Failed to inspect Access document.", e);
				}
			}
			return exitCode;
		}

		public static int InspectDocument(AccessDocument source, ExtractionTarget[] targets) {
      if ( targets.Contains(ExtractionTarget.Modules)) {
        if (source.VbaProject != null && source.VbaProject.Modules.Count > 0) {
         Console.Error.WriteLine("Modules:" );
          foreach (AccessVbaModuleInfo module in source.VbaProject.Modules)
            Console.WriteLine(module.Source ?? module.SourceLimitation);
        } else {
          Console.Error.WriteLine("No Modules");
        }
      }
      if ( targets.Contains(ExtractionTarget.References)) {
        if (source.VbaProject != null && source.VbaProject.References.Count > 0) {
          Console.Error.WriteLine("References:" );
          foreach (AccessVbaReferenceInfo reference in source.VbaProject.References)
            Console.WriteLine($"{reference.Name}: {reference.LibraryId}");
        } else {
          Console.Error.WriteLine("No References");
        }
      }
      if ( targets.Contains(ExtractionTarget.Forms)) {
        if (source.Forms.Count != 0) {        
          Console.Error.WriteLine("Forms:" );
          foreach (AccessApplicationObject form in source.Forms) {
            Console.WriteLine($"{form.Name}: {form.Definition?.RecordSource}");
            foreach (AccessStorageStream stream in form.Streams)
              Console.Error.WriteLine($"{stream.Path}: {stream.Payload.GetBytes().Length} bytes");
          }
        } else {
          Console.Error.WriteLine("No Forms");
        }
      }
      if ( targets.Contains(ExtractionTarget.Queries)) {
        if (source.Queries.Count != 0 ) {
          Console.Error.WriteLine("Queries:" );
          foreach (AccessQueryDefinition queryDefinition in source.Queries)
            Console.WriteLine(String.Format("Query: {0}\n{1}", queryDefinition.Name, ((queryDefinition.HasSql)? queryDefinition.Sql : "SQL unavailable")));
        } else {
          Console.Error.WriteLine("No Queries");
        }
      }
      return 0;
		}

		public static void Main(string[] args) {
			// https://learn.microsoft.com/en-us/dotnet/api/system.io.fileinfo?view=netframework-4.8
			Argument <FileInfo[]>filesArgiment = new("files") {
				Arity = ArgumentArity.OneOrMore,
				Description = "files to examine"
			};
			Option <ExtractionTarget[]>extractOption = new ("--extract") {
				Arity = ArgumentArity.ZeroOrMore,
				Description = "Content to extract. Specify additional content by repetition"
			};
      // https://learn.microsoft.com/en-us/dotnet/api/system.commandline.rootcommand?view=net-11.0-pp&viewFallbackFrom=netframework-4.8
			RootCommand rootCommand = new ("Examine Access file. Extract VBA modules, saved queries SQL, References, Forms");
			rootCommand.Arguments.Add(filesArgiment);
			rootCommand.Options.Add(extractOption);
			// https://learn.microsoft.com/en-us/dotnet/api/system.commandline.command.setaction?view=net-11.0-pp&viewFallbackFrom=netframework-4.8
      // https://learn.microsoft.com/en-us/dotnet/api/system.commandline.parseresult?view=net-11.0-pp&viewFallbackFrom=netframework-4.8
			rootCommand.SetAction((ParseResult parseResult) => InspectFiles(parseResult.GetValue(filesArgiment)!, parseResult.GetValue(extractOption) ?? new[] { ExtractionTarget.Modules }));
      Console.Error.WriteLine(String.Format("Status: {0}", ((rootCommand.Parse(args).Invoke()) == 0 ? "Success":"Error")));
      return;
    }
	}
}
