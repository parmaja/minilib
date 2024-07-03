var reload_elements = [];

function reloadElements() {
  console.log('reloadElements');
  reload_elements.forEach(element => {
    const tagId = element.id;
    const tagUrl = element.getAttribute('data-refresh-url');
    fetch(tagUrl) 
      .then(response => response.text())
      .then(data => {
        element.innerHTML = data;
      })
      .catch(error => {
        element.innerHTML = 'Error: ' + error.message;
        console.error('Error fetching content:', error);
      });
  });
}

function init()
{
  reload_elements = document.querySelectorAll('[data-refresh-url]');
  if (reload_elements.length > 0) 
  {
    console.log('interval enabled');
    setInterval(reloadElements, 1000);
  }
  else
    console.log('interval is not enabled');
}

window.onload = init;