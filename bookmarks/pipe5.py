import subprocess
import threading

def read_output(process):
    """Reads output from the process's stdout and stderr in real-time."""
    try:
        with process.stdout, process.stderr:
            while True:
                stdout_line = process.stdout.readline()
                stderr_line = process.stderr.readline()

                if not stdout_line and not stderr_line:
                    break  # Exit when both streams are empty

                if stdout_line:
                    print(stdout_line.strip())

                if stderr_line:
                    print(stderr_line.strip())

    except Exception as e:
        print(f"Error reading output: {e}")

def upload_to_google_artifact_registry(repository_url):
    """Executes 'twine upload --repository-url <URL> dist/*' using subprocess and prints output in real-time."""
    cmd = ["twine", "upload", "--repository-url", repository_url, "dist/*"]

    process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, bufsize=1)

    output_thread = threading.Thread(target=read_output, args=(process,))
    output_thread.start()

    process.wait()  # Wait for the subprocess to finish
    output_thread.join()

    if process.returncode == 0:
        print("Upload completed successfully.")
    else:
        print(f"Upload failed with return code {process.returncode}.")

if __name__ == "__main__":
    artifact_registry_url = "https://your-artifact-registry-url"  # Replace with actual Google Artifact Registry URL
    upload_to_google_artifact_registry(artifact_registry_url)
