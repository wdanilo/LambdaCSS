import sys
import time
import logging
from watchdog.observers import Observer
from watchdog.events import LoggingEventHandler

class Handler:
  def dispatch (h, evnt):
    if evnt.src_path == "./Test.hs":
      print(":r")
      print("main")
      sys.stdout.flush()

if __name__ == "__main__":
    print("main")
    sys.stdout.flush()
    
    event_handler = Handler()
    observer = Observer()
    observer.schedule(event_handler, ".", recursive=True)
    observer.start()
    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        observer.stop()
    observer.join()
