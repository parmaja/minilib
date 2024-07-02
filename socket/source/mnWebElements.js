function reloadElement() {
    name = 'test';
    url = '';
    fetch(url) // Replace with the actual server endpoint
      .then(response => response.text())
      .then(data => {
        document.getElementById(data).innerHTML = data;
      })
      .catch(error => {
        document.getElementById(data).innerHTML = error;
        console.error('Error fetching content:', error);
      });
}

setInterval(reload, 1000);