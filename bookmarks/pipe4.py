import subprocess
import threading
import queue

def enqueue_output(pipe, output_queue):
    """Reads output from a pipe and puts it into a queue."""
    try:
        with pipe:
            for line in pipe:
                output_queue.put(line.strip())
    finally:
        output_queue.put(None)  # Signal that this thread is done

def upload_to_google_artifact_registry(repository_url):
    """Executes 'twine upload --repository-url <URL> dist/*' using subprocess and prints output in real-time."""
    cmd = ["twine", "upload", "--repository-url", repository_url, "dist/*"]

    process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, bufsize=1)

    output_queue = queue.Queue()
    stdout_thread = threading.Thread(target=enqueue_output, args=(process.stdout, output_queue))
    stderr_thread = threading.Thread(target=enqueue_output, args=(process.stderr, output_queue))

    stdout_thread.start()
    stderr_thread.start()

    while process.poll() is None or not output_queue.empty():
        try:
            line = output_queue.get(timeout=0.1)  # Non-blocking with timeout
            if line is None:
                break  # Exit loop when both threads finish
            print(line)
        except queue.Empty:
            continue  # No output yet, keep waiting

    stdout_thread.join()
    stderr_thread.join()

    if process.returncode == 0:
        print("Upload completed successfully.")
    else:
        print(f"Upload failed with return code {process.returncode}.")

if __name__ == "__main__":
    artifact_registry_url = "https://your-artifact-registry-url"  # Replace with actual Google Artifact Registry URL
    upload_to_google_artifact_registry(artifact_registry_url)

