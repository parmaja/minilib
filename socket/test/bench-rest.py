import requests
from concurrent.futures import ThreadPoolExecutor, as_completed
import time
import threading

URL = "http://localhost:81/erpsample/cswb"
COUNT = 10000
MAX_WORKERS = 10000  # Number of parallel threads

# Thread-safe variables to track concurrent connections
current_connections = 0
max_connections = 0
connections_lock = threading.Lock()

def make_request(request_id):
    global current_connections, max_connections

    # 1. Increment connection counter and track the maximum
    with connections_lock:
        current_connections += 1
        if current_connections > max_connections:
            max_connections = current_connections

    try:
        # 2. Perform the HTTP request
        response = requests.get(URL, timeout=10)
        return request_id, response.status_code, "Success"
    except requests.exceptions.RequestException as e:
        return request_id, "Error", str(e)
    finally:
        # 3. Decrement connection counter when done (always runs, even on error)
        with connections_lock:
            current_connections -= 1

def main():
    global max_connections  # Reset in case function is called multiple times
    max_connections = 0

    print(f"Starting {COUNT} parallel requests to {URL}...\n")

    start_time = time.perf_counter()
    success_count = 0
    error_count = 0

    with ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
        futures = [executor.submit(make_request, i) for i in range(COUNT)]

        for future in as_completed(futures):
            req_id, status, msg = future.result()
            if status == "Error":
                error_count += 1
            else:
                success_count += 1

    end_time = time.perf_counter()
    total_time = end_time - start_time

    # Print Timing & Concurrency Summary
    print("=" * 45)
    print("           TIMING & CONCURRENCY SUMMARY")
    print("=" * 45)
    print(f"Total Requests:             {COUNT}")
    print(f"Successful:                 {success_count}")
    print(f"Failed:                     {error_count}")
    print(f"Max Concurrent Connections: {max_connections}")
    print(f"Total Time:                 {total_time:.4f} seconds")
    print(f"Avg Time/Request:           {(total_time / COUNT):.4f} seconds")
    print(f"Throughput:                 {COUNT / total_time:.2f} requests/second")
    print("=" * 45)

if __name__ == "__main__":
    main()