"use-strict";

let mnw = {};

mnw.ws = null;
mnw.interactive = false;
mnw.attached = false;
mnw.url = "";
mnw.pool = [];

mnw.raw_receive = function(msg)
{
  console.log(msg);
  if (msg.charAt(0) === '{')
  {    
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
  console.log(msg);
  this.ws.send(msg);
}

mnw.send = function(id, command, content) 
{
  this.raw_send(JSON.stringify({"element": id, "command": command, "content": content}));
}

mnw.connect = function()
{
  console.log("Connecting to: " + this.url);
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
    mnw.attached = false;
    console.log("Connection closed, deattached");
    if (mnw.interactive)
    {
      console.log("Error, trying in 5s")
      setTimeout(function() { mnw.connect(); }, 5000);
    }
  }  

  this.ws.onerror = function(ev) 
  {
    console.log("Connection error")
    if (mnw.interactive)
    {
      //mnw.ws.close();      
      //setTimeout(function() { mnw.connect(); }, 5000);
    }
  }
}

mnw.attach = function(url)
{
  this.url = url;  
  this.connect();
}

var interval = 1000;
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

mnw.click = function(event, sender) 
{
  const url = sender.getAttribute('href');
  fetch(url)
  .then(response => response.text())
  .then(data => console.log(data));  
  event.preventDefault();
  return false;
}

function createToastElement(type, content) 
{
  let toastContainer = document.createElement('div');
  toastContainer.classList.add('toast-container');

  let toast = document.createElement('div');
  toast.classList.add('toast');

  let toastBody = document.createElement('div');
  toastBody.classList.add('toast-body', type);
  toastBody.innerText = content;

  toast.appendChild(toastBody);
  toastContainer.appendChild(toast);

  if ($('.wrapping-element').length <= 0) {
      let wrappingElement = document.createElement('div');
      wrappingElement.classList.add('wrapping-element');
      wrappingElement.appendChild(toastContainer);
      document.body.appendChild(wrappingElement);
  }
  else {
      $('.wrapping-element').append(toastContainer);
  }
}

mnw.showToast = function(content, type = "warning")
{
  var delay = 15000;
  var toastContainer = document.querySelector("#toast-container");

 
  var element = document.createElement('div');
  element.className = `toast align-items-center bg-${type} text-black border-black shadow-thin`;
  element.setAttribute('role', 'alert');
  element.setAttribute('aria-live', 'assertive');  
  element.setAttribute('aria-atomic', 'true');
  element.innerHTML = `<div class="d-flex">
                        <div class="toast-body h6 p-3 m-0">${content}</div>
                        <button type="button" class="btn-close btn-close-black me-2 m-auto" data-bs-dismiss="toast" aria-label="Close"></button>
                      </div>`;

  toastContainer.appendChild(element);  
  
  var toast = new bootstrap.Toast(element, {delay: delay, autohide: true, animation: true});

  element.addEventListener('hidden.bs.toast', function () {
    element.remove();
  })

  toast.show();
}

mnw.switch_theme = function(event)
{
  if (document.body.getAttribute('data-bs-theme') == 'dark')
    document.body.setAttribute('data-bs-theme', 'light');
  else
    document.body.setAttribute('data-bs-theme', 'dark');    
}

function init()
{
  reload_elements = document.querySelectorAll('[data-mnw-refresh-url]');
  if (reload_elements.length > 0) 
  {
    console.log('Interval is ' + document.body.getAttribute('data-mnw-refresh-interval'));

    if (document.body.hasAttribute('data-mnw-refresh-interval'))
        interval = parseInt(document.body.getAttribute('data-mnw-refresh-interval')) * 1000;

    console.log('Interval is ' + interval.toString());

    if (interval > 0)
    {
        setInterval(reloadElements, interval);
        console.log('Interval enabled ' + interval.toString());
    }
  }

  mnw.interactive = document.body.hasAttribute('data-mnw-interactive');
  if (mnw.interactive)  
    mnw.attach(window.location.href);


  // JavaScript to toggle dark mode
}

function finish()
{
/*  if (mnw.ws)
    mnw.ws.close();*/
}

window.addEventListener('load', init);
window.addEventListener("beforeunload", finish);