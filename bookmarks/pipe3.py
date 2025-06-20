import subprocess
import threading
import queue
import sys

def enqueue_output(pipe, output_queue):
    """Read output from a pipe and put it into a queue."""
    try:
        with pipe:
            for line in iter(pipe.readline, b''):
                output_queue.put(line.decode().strip())
    finally:
        output_queue.put(None)  # Signal that this thread is done

def run_subprocess():
    """Run a subprocess and continuously print its output."""
    cmd = ["ping", "-c", "5", "google.com"]  # Example command
    process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, bufsize=1, universal_newlines=True)

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
    print("Process completed.")

if __name__ == "__main__":
    run_subprocess()

