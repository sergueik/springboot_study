using OfficeIMO;
using OfficeIMO.Access;
using System.CommandLine;
using System.CommandLine.Parsing;

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
				if (!file.Exists) {
					Console.Error.WriteLine(String.Format("Source file {0} not found", file.FullName));
					exitCode = 1;
					continue;
				}
				Console.Error.WriteLine(String.Format("File: {0}", file.FullName));
				Console.Error.WriteLine(String.Format("Extract: {0} targets", targets.Count()));
        // NOTE: warning CS8600: Converting null literal or possible null value to non-nullable type
				AccessDocument source = null;
				try {
					// Code that may throws an initial exception
					source = AccessDocument.Load(file.FullName);
          if (source is null)
            return -1;
          if ( targets.Contains(ExtractionTarget.Modules)) {
            Console.Error.WriteLine("Modules: " );
            foreach (AccessVbaModuleInfo module in source.VbaProject.Modules)
              Console.WriteLine(module.Source ?? module.SourceLimitation);
          }
          if ( targets.Contains(ExtractionTarget.References)) {
            Console.Error.WriteLine("References: " );
            foreach (AccessVbaReferenceInfo reference in source.VbaProject.References)
              Console.WriteLine($"{reference.Name}: {reference.LibraryId}");
          }
          if ( targets.Contains(ExtractionTarget.Forms)) {
            Console.Error.WriteLine("Forms: " );
            foreach (AccessApplicationObject form in source.Forms) {
              Console.WriteLine($"{form.Name}: {form.Definition?.RecordSource}");
              foreach (AccessStorageStream stream in form.Streams)
                Console.Error.WriteLine($"{stream.Path}: {stream.Payload.GetBytes().Length} bytes");
            }
          }
          if ( targets.Contains(ExtractionTarget.Queries)) {
            if (source.Queries.Count() != 0 ) {
              Console.Error.WriteLine("Queries: " );
              foreach (AccessQueryDefinition queryDefinition in source.Queries)
                Console.WriteLine(String.Format("Query: {0}\n{1}", queryDefinition.Name, ((queryDefinition.HasSql)? queryDefinition.Sql : "SQL unavailable")));
              }
            }
				} catch (InvalidDataException e) { 
					// Wrap the original domain specific exception inside a own
          // The REST controller or exception-handling middleware can translate the same exception into an HTTP response without diving into AccessNativeRowCursor details 
					throw new InvalidOperationException("Failed to calculate the result.", e);
				} finally {
          if (source is not null)         
            source.Dispose(); 
				}
      }
      return exitCode;
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
  public class AccessInspectionException : Exception {

      public AccessInspectionException(
          string message,
          Exception innerException)
          : base(message, innerException) {
      }
  }
}

