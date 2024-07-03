var reload_elements = [];

function reloadElements() {
  reload_elements.forEach(item => {
    fetch(item[1]) 
      .then(response => response.text())
      .then(data => {
        document.getElementById(item[0]).innerHTML = data;
      })
      .catch(error => {
        document.getElementById(item[0]).innerHTML = 'Error: ' + error.message;
        console.error('Error fetching content:', error);
      });
  });
}

function addReload(elmName, elmURL) {
  reload_elements.push([elmName, elmURL]);
  if (reload_elements.length === 1) {
    setInterval(reloadElements, 1000);
  }
}