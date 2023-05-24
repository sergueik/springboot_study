#!/usr/bin/env python3 $@
import threading
import time
import queue
import sys
import random
import asyncio
global num
q = queue.Queue()

# this worker subroutine will be runs 'as a thread'.
def worker():
  while True:
    item = q.get()
    cnt = item + 1
    delay = random.randint(0, num)
    print(f'Working on {cnt} item will take {delay}')
    time.sleep(delay)
    print(f'Finished.')
    q.task_done()

# Turn-on the worker thread.
threading.Thread(target = worker, daemon = True).start()
if len(sys.argv) > 1:
  num = int(sys.argv[1])
else:
  num = 3

print(f'Send {num} task requests to the worker')

for item in range(num):
  q.put(item)
# NOTE: not seeing concurrent execution because
# start a thread, then immediately wait on that thread to finish
# Block until all tasks are done.
q.join()
print('All work completed')
