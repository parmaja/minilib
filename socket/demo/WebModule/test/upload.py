import requests
import gzip

def upload_file(url, file_path):
    # Open the file and compress it using gzip
    with open(file_path, 'rb') as file:
        #content_file = gzip.compress(file.read())
        content_file = file.read()

    # Set the headers for compressed and chunked transfer encoding
    headers = {
        'Accept-Encoding': '',
        'Content-Encoding': '',
        'Transfer-Encoding': 'chunked'
    }

    # Send the request with the compressed file and headers
    response = requests.post(url, data=content_file, headers=headers)

    print("Response status code:", response.status_code)
    print("Response content:", response.text)

    for header, value in response.headers.items():
        print(f"{header}: {value}")

    if response.status_code == requests.codes.ok:
        print("File upload successful!")
    else:
        print("File upload failed. Status code:", response.status_code)

# Example usage
upload_file('http://10.0.0.7:81/sony/csen/', 'file.txt')
