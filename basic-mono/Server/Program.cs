using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using HTTPServerLib;
using HttpServer;

namespace HTTPServerLib
{
	class Program
	{
		static ManualResetEvent quitEvent = new ManualResetEvent(false);

		static ExampleServer server;
		static void Main(string[] args)
		{
		 	
			Console.CancelKeyPress += (Object sender, ConsoleCancelEventArgs  e) => {
				Console.WriteLine("Ctrl+C pressed — shutting down...");
				server.Stop();
				e.Cancel = true; // prevents immediate process kill
				quitEvent.Set();  // signal shutdown
				Console.WriteLine("Done.");
			};

			server = new ExampleServer("0.0.0.0", 4050);
			server.SetRoot(System.IO.Directory.GetCurrentDirectory());
			server.Logger = new ConsoleLogger();
			server.Start();
			Console.WriteLine("Running. Press Ctrl+C to exit.");

		}
	}
}

