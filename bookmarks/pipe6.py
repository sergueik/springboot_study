import subprocess
import threading

def read_stream(stream):
    """Reads process output stream in real-time"""
    for line in stream:
        print(line.strip())

def run_command():
    with subprocess.Popen(["ping", "-c", "5", "google.com"], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True) as process:
        # Start separate threads to read stdout and stderr simultaneously
        stdout_thread = threading.Thread(target=read_stream, args=(process.stdout,))
        stderr_thread = threading.Thread(target=read_stream, args=(process.stderr,))

        stdout_thread.start()
        stderr_thread.start()

        stdout_thread.join()
        stderr_thread.join()

run_command()
