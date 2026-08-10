Add-Type -TypeDefinition @'
using System;
using System.Diagnostics;
using System.IO;

namespace Utils {
	// NOTE:
	// will cannot declare instance members in a static class
	public class Program {
		private string tempPath;
		public string TempPath {
			get { return tempPath; }
		}
		private string location;
		public string Location {
			get { return location; }
			set { location = value; }
		}
		// .Net 4.0: Default parameter specifiers are not permitted
		public static string CreateTempSubdirectory() {
			return CreateTempSubdirectory("");
		}

		public static string CreateTempSubdirectory(string prefix) {
			// Get the system temp path
			string tempRoot = Path.GetTempPath();

			// Create a unique folder name (optional prefix + GUID)
			string uniqueFolderName = string.IsNullOrEmpty(prefix)
            ? Guid.NewGuid().ToString()
            : prefix + "_" + Guid.NewGuid().ToString();

			// Combine root and unique subfolder name
			string uniquePath = Path.Combine(tempRoot, uniqueFolderName);

			// Create and return the physical directory
			Directory.CreateDirectory(uniquePath);
			return uniquePath;
		}

		public void Run() {

			// NOTE: custom extension returning String, not DirectryInfo hence no FullPath
			// built-in Directory.CreateTempSubdirectory() method does not exist in .NET 4.5 (it was introduced later in .NET Core 3.0 / .NET Standard 2.1).
			// therefore the following
			// https://learn.microsoft.com/en-us/dotnet/api/system.io.directory.createtempsubdirectory?view=net-10.0?view=netframework-4.5
			//	is redirect
			// https://learn.microsoft.com/en-us/dotnet/api/system.io.directory.createtempsubdirectory?view=net-10.0&viewFallbackFrom=net-10.0%3Fview%3Dnetframework-4.5
			this.tempPath = CreateTempSubdirectory(Path.GetTempPath());
			// .Net 4.0  A new expression requires () or [] after type
			// .Net 4.0 The type or namespace name 'var' could not be found
			Process process = new Process();
			// https://learn.microsoft.com/en-us/dotnet/api/system.diagnostics.processstartinfo?view=netframework-4.5
			process.StartInfo = new ProcessStartInfo();

			process.StartInfo.WorkingDirectory = tempPath;
			// NOTE: @"" does not appear to work under add-type
			// FileName = @"C:\Program Files\Git\bin\git.exe";
			process.StartInfo.FileName = "C:\\Program Files\\Git\\bin\\git.exe";
			process.StartInfo.Arguments = String.Format("clone --depth 1 \"{0}\"", this.location);
			process.StartInfo.UseShellExecute = false;

			process.Start();
			process.WaitForExit();

			try {
				Directory.Delete(tempPath, true);
			} catch (Exception) {
			}
		}
	}
}
'@
# $git_install_path = ( Get-ItemProperty -Path 'HKLM:\SOFTWARE\GitForWindows').InstallPath

# $window_handle = [System.Diagnostics.Process]::GetCurrentProcess().MainWindowHandle
$helper = new-object Utils.Program
# -ArgumentList (([int]$window_handle))
$helper.Location = "https://github.com/membranedev/application-skills";
$helper.Run()
write-output $helper.TempPath

# Exception calling "run" with "0" argument(s): "Access to the path 'C:\Users\kouzm\AppData\Local\Temp\_e57611bf-0351-4731-916f-f082e1bd671e' is denied."