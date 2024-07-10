var ws = null;
var attached = false;

function ws_receive(msg) 
{
  if (msg.charAt(0) === '{')
  {    
    const json = JSON.parse(msg);
    if (json.type === 'text') 
    {      
      const element = document.getElementById(json.element);
      if (element)
        element.value = json.content;
    }
  }
}

function ws_send(msg) 
{

}

function send(id, command, content) 
{
  ws_send({"element": id, "command": command, "content": content})
}

function attach(url)
{
  console.log("connecting to: " + url);
  ws = new WebSocket(url)
  ws.onopen = function(ev) {
    console.log("connection established");
  }
  ws.onmessage = function(ev) {
    ws_receive(ev.data);
    console.log("Message from Server: " + ev.data);
  }
  ws.onclose  = function(ev) {
    console.log("Connection closed");
  }
  ws.onerror = function(ev) {
    console.log("error")
  }
}

var reload_elements = [];

function reloadElements() 
{
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

  attached = document.body.hasAttribute('data-attach');
  if (attached == true)  
    attach(window.location.href);
}

window.onload = init;
