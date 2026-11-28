using System;
using System.Diagnostics;


namespace Utils {
	public class Logger {

		private bool debug = false;
		public bool Debug {
			get { return debug; }
			set { debug = value; }
		}
		public static void Log(String line) {
			// Visual Studio does not provide a single, universal API specifically designed to detect the IDE itself
			// one should proceed indirectly
			if (Debugger.IsAttached){
				// outside of Debug configuration the Debug.WriteLine() calls are normally removed by compilation
				System.Diagnostics.Debug.WriteLine(line);
			} else {
				Console.Error.WriteLine(line);
			}
		}

	}
}
