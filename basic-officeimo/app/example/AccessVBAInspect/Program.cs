using OfficeIMO;
using OfficeIMO.Access;

namespace PdfDocGenerator.CLI {
	public class Program {
		public  static void Main(string[] args) {
			using AccessDocument source = AccessDocument.Load("application.accdb");
			foreach (AccessApplicationObject form in source.Forms) {
				Console.WriteLine($"{form.Name}: {form.Definition?.RecordSource}");
				foreach (AccessStorageStream stream in form.Streams)
					Console.WriteLine($"{stream.Path}: {stream.Payload.GetBytes().Length} bytes");
			}
			foreach (AccessVbaModuleInfo module in source.VbaProject.Modules)
				Console.WriteLine(module.Source ?? module.SourceLimitation);
			foreach (AccessVbaReferenceInfo reference in source.VbaProject.References)
				Console.WriteLine($"{reference.Name}: {reference.LibraryId}");
	}
	}
}
