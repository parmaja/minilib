let mnw = {};

mnw.ws = null;
mnw.attached = false;
mnw.url = "";

mnw.ws_receive = function(msg)
{
  if (msg.charAt(0) === '{')
  {    
    console.log(msg);
    const json = JSON.parse(msg);
    if (json.command === 'change') 
    {      
      const element = document.getElementById(json.element);
      if (element)
        element.value = json.content;
    }
  }
}

mnw.ws_send = function(msg) 
{
  this.ws.send(msg);
}

mnw.send = function(id, command, content) 
{
  this.ws_send(JSON.stringify({"element": id, "command": command, "content": content}));
}

mnw.connect = function()
{
  console.log("connecting to: " + this.url);

  this.ws = new WebSocket(this.url)

  this.ws.onopen = function(ev) 
  {
    console.log("Connection established");
  }

  this.ws.onmessage = function(ev) 
  {
    mnw.ws_receive(ev.data);    
  }

  this.ws.onclose  = function(ev) 
  {
    console.log("Connection closed");
  }

  this.ws.onerror = function(ev) 
  {
    console.log("Error, trying in 5s")
    setTimeout(this.connect, 5000);
  }
}

mnw.attach = function(url)
{
  this.url = url;  
  this.connect();
}

var reload_elements = [];

function reloadElements() 
{
  reload_elements.forEach(element => {
    const tagId = element.id;
    const tagUrl = element.getAttribute('data-mnw-refresh-url');
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
  reload_elements = document.querySelectorAll('[data-mnw-refresh-url]');
  if (reload_elements.length > 0) 
  {
    var interval = document.body.hasAttribute('data-mnw-refresh-interval');
    if (interval == 0)
    interval = 1000;
    setInterval(reloadElements, interval);
    console.log('interval enabled ' + interval);
  }

  mnw.attached = document.body.hasAttribute('data-mnw-attach');
  if (mnw.attached == true)  
    mnw.attach(window.location.href);
}

window.addEventListener('load', init);
