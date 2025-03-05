import subprocess
import threading
import queue

def read_output(pipe, output_queue):
    """Reads the process output line by line and puts it into a queue."""
    with pipe:
        for line in iter(pipe.readline, ''):
            output_queue.put(line.strip())

def main():
    command = "ls -l /nonexistent_directory"  # Replace with any command
    process = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)

    output_queue = queue.Queue()

    # Start a thread to read stdout in real-time
    stdout_thread = threading.Thread(target=read_output, args=(process.stdout, output_queue), daemon=True)
    stdout_thread.start()

    # Read stdout from the queue and print in real-time
    while True:
        try:
            line = output_queue.get(timeout=0.1)  # Non-blocking with timeout
            print(line)
        except queue.Empty:
            if process.poll() is not None:
                break  # Process has finished and queue is empty

    stdout_thread.join()  # Ensure the thread completes

    # Collect stderr after process completion
    stderr_output = process.stderr.read().strip()
    
    # Print stderr if any
    if stderr_output:
        print("\n[ERROR OUTPUT]:")
        print(stderr_output)

    # Report process exit status
    print(f"\n[PROCESS STATUS]: Exit Code {process.returncode}")

if __name__ == "__main__":
    main()
