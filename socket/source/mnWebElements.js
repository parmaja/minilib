let mnw = {};

mnw.ws = null;
mnw.interactive = false;
mnw.attached = false;
mnw.url = "";
mnw.pool = [];

mnw.raw_receive = function(msg)
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
  else if (msg == "attached")
  {
    this.attached = true;
  } 
  else if (msg == "ping")
  {
    this.ws.send("pong");
  } 
  else if (msg == "close")
  {
    this.attached = false;
    this.ws.close()
  }
}

mnw.raw_send = function(msg) 
{
  this.ws.send(msg);
}

mnw.send = function(id, command, content) 
{
  this.raw_send(JSON.stringify({"element": id, "command": command, "content": content}));
}

mnw.connect = function()
{
  console.log("connecting to: " + this.url);

  this.ws = new WebSocket(this.url);  

  this.ws.onopen = function(ev) 
  {
    console.log("Connection established");
    mnw.raw_send('attach');
  }

  this.ws.onmessage = function(ev) 
  {
    mnw.raw_receive(ev.data);
  }

  this.ws.onclose  = function(ev) 
  {
    this.attached = false;
    console.log("Connection closed");
    if (this.interactive)
    {
      console.log("Error, trying in 5s")
      setTimeout(this.connect, 5000);
    }
  }  

  this.ws.onerror = function(ev) 
  {
    console.log("Error, trying in 5s")
    if (this.interactiveh)
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

  mnw.interactive = document.body.hasAttribute('data-mnw-interactive');
  if (mnw.interactive == true)  
    mnw.attach(window.location.href);
}

function finish()
{
  if (mnw.ws)
    mnw.ws.close();
}

window.addEventListener('load', init);

window.addEventListener("beforeunload", finish);