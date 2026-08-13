
param (
  $location = 'https://github.com/membranedev/application-skills'
)

Add-Type -TypeDefinition @'
using System;
using System.Diagnostics;
using System.IO;

namespace Utils {
	// NOTE:
	// will cannot declare instance members in a static class
	public class Program {
		private string filename = "SKILL.md";
		public string Filename {
			get { return filename; }
			set { filename = value; } }
		private string[] files;

		public string[] Files { get { return files; }}
		private string tempPath;
		public string TempPath {
			get { return tempPath; }
		}
		private string location;
		private string project; 
		public string Location {
			get { return location; }
			set { location = value;
				string[] parts = location.Split('/');
				project = parts[parts.Length - 1 ];
			}
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
      if (!Directory.Exists(Path.Combine(tempPath, project)))
        return;      
			this.files = Directory.GetFiles( Path.Combine(tempPath, project), filename, SearchOption.AllDirectories);
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
function read_location {
  
  param(
    $helper_ref = $null,
    [string]$location = $null,
    [string]$logfile = $null
  )
  $helper = $helper_ref.Value
  $helper = new-object Utils.Program
  # -ArgumentList (([int]$window_handle))
  
  $helper.Location = $location
  $helper.Run()
  # Exception calling "Run" with "0" argument(s): "Could not find a part of the path 'C:\Documents and Settings\Admin\Local Settings\Temp\_d6a741e8-48b4-48fe-a141-26ef3d393b86\application-skills'."
  
  # Powershell 2.x The term 'new-temporaryfile' is not recognized
  if ($PSBoundParameters.ContainsKey('Verbose')) {
    tee-object -filepath $tempFile -inputObject @($helper.Files)
  } else {
    out-file -filepath $tempFile -inputObject @($helper.Files)
  }
  # why (measure-object -inputObject @($helper.Files)).Count = 1
  write-host ('{0} items ({1} bytes) written' -f ($helper.Files.Count), (get-item $tempFile).Length)
}

$tempFile = new-temporaryfile

read_location -helper_ref [ref]($helper) -logfile $tempFile -location $location

# Exception calling "run" with "0" argument(s): "Access to the path 'C:\Users\kouzm\AppData\Local\Temp\_e57611bf-0351-4731-916f-f082e1bd671e' is denied."
<#
Cloning into 'application-skills'...
fatal: unable to access 'https://github.com/membranedev/application-skills/': Couldn't resolve host 'github.com'
C:\Documents and Settings\Admin\Local Settings\Temp\_1b1db0d8-4001-457d-8128-6f8655389fe8
#>