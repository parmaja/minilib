/* version 1.6 */
"use strict";

let mnw = {};

mnw.ws = null;
mnw.interactive = false;
mnw.attached = false;
mnw.url = "";
mnw.pool = [];

/* WebSocket Protocol */

mnw.raw_receive = function(msg)
{
  if (msg.charAt(0) === '{')
  {
    try {
      const json = JSON.parse(msg);
      if (json.command === 'change')
      {
        const element = document.getElementById(json.element);
        if (element)
          element.value = json.content;
      }
    } catch (e) {
      console.error('Error parsing JSON:', e);
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
      // Reconnection is handled in onclose, so we just log the error
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
    const tagUrl = element.getAttribute('data-mnw-refresh-url');
    let tagStamp = element.getAttribute('data-mnw-stamp');

    if (element._mnwAbortController)
      element._mnwAbortController.abort();
    element._mnwAbortController = new AbortController();

    fetch(tagUrl, { headers:{"If-None-Match": tagStamp }, signal: element._mnwAbortController.signal })
      .then(response => {
          if (response.status === 304)
            return null;
          const etag = response.headers.get('ETag');
          const data = response.text();
          return Promise.all([etag, data]);
        }
      )
      .then(result => {
          if (!result) return;
          const [etag, data] = result;
          element.setAttribute('data-mnw-stamp', etag);
          element.innerHTML = data;
        }
      )
      .catch(error => {
        if (error.name === 'AbortError') return;
        element.innerHTML = 'Error: ' + error.message;
        console.error('Error fetching content:', error);
      });
  });
}

mnw.click = function(sender, event)
{
  const url = sender.getAttribute('href');
  fetch(url)
  .then(response => response.text())
  .then(data => console.log("Click response: " + data))
  .catch(error => {
    console.error('Error on click:', error);
    console.log("Error on click: " + error.message);
  });
  event.preventDefault();
  return false;
}

mnw.action = function(event, url, data)
{
  //console.log(JSON.stringify(data));
  fetch(url, {
    method: 'POST',
    body: JSON.stringify(data),
    headers: {
      'Content-Type': 'application/json'
    }
  })
  .then(response => response.text())
  .then(data => console.log("Action response: "+data))
  .catch(error => {
    console.error('Error in action:', error);
    console.log("Error in action: "+error.message);
  });
   return false;
}

/* Utils functions */

mnw.formPost = function(formElement, event) {
   if (event) {
     event.preventDefault();
   }
   const formData = new FormData(formElement);
   const data = {};
   formData.forEach((value, key) => {
     data[key] = value;
   });
   fetch(formElement.action, {
     method: 'POST',
     body: JSON.stringify(data),
     headers: {
       'Content-Type': 'application/json'
     }
   })
   .then(response => response.text())
   .then(text => {
     let json;
     try {
       json = JSON.parse(text);
     } catch (e) {
       mnw.showToast('Invalid JSON response', 'danger');
       return;
     }
     if (json.redirect) {
       window.location.href = json.redirect;
     } else if (json.message) {
       mnw.showToast(json.message, 'danger');
     } else {
       mnw.showToast('Unknown error', 'danger');
     }
   })
   .catch(error => {
     console.error('Error in formPost:', error);
     mnw.showToast('Network error', 'danger');
   });
   return false;
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
}

function finish()
{
/*  if (mnw.ws)
    mnw.ws.close();*/
}

window.addEventListener('load', init);
window.addEventListener("beforeunload", finish);

/* UI functions */

mnw.switch_zoom = function(sender, event)
{
  let mnw_zoom = sender.getAttribute('data-mnw-zoom') || '';
  if (mnw_zoom === 'normal')
    mnw_zoom = '';

  if (mnw_zoom)
  {
    document.documentElement.setAttribute('data-mnw-zoom', mnw_zoom);
    localStorage.setItem('mnw-zoom', mnw_zoom);
  }
  else
  {
    document.documentElement.removeAttribute('data-mnw-zoom');
    localStorage.removeItem('mnw-zoom');
  }
}

/* Bootstrap Functions */

mnw.showToast = function(content, type = "warning")
{
  var delay = 15000;
  var toastContainer = document.querySelector("#toast-container");
  if (!toastContainer) {
    toastContainer = document.createElement('div');
    toastContainer.id = 'toast-container';
    document.body.appendChild(toastContainer);
  }

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

mnw.switch_theme = function(sender, event)
{
  let bs_theme = 'dark';
  if (document.body.getAttribute('data-bs-theme') == 'dark')
    bs_theme = 'light';
  document.body.setAttribute('data-bs-theme', bs_theme);
  localStorage.setItem('mnw-theme', bs_theme);
}

mnw.init_zoom = function()
{
  if (!document.body.getAttribute('data-mnw-zoom'))
  {
    let bs_zoom = localStorage.getItem('mnw-zoom');
    if (bs_zoom)
      document.documentElement.setAttribute('data-mnw-zoom', bs_zoom);
  }
}

mnw.init_accordions = function()
{
  document.querySelectorAll('[data-mnw-save-state]').forEach(function(accordion) {
    const accordionId = accordion.id;
    if (!accordionId) return;

    accordion.querySelectorAll('[data-mnw-section]').forEach(function(section) {
      const sectionId = section.getAttribute('data-mnw-section');
      const key = 'mnw-accordion-' + accordionId + '-' + sectionId;
      if (localStorage.getItem(key) === '1') {
        section.classList.add('show');
        const header = document.querySelector('[data-bs-target="#' + sectionId + '"]');
        if (header) {
          header.classList.remove('collapsed');
          header.setAttribute('aria-expanded', 'true');
        }
      }
    });

    accordion.addEventListener('shown.bs.collapse', function(ev) {
      const section = ev.target;
      const sectionId = section.getAttribute('data-mnw-section');
      if (sectionId) {
        const key = 'mnw-accordion-' + accordionId + '-' + sectionId;
        localStorage.setItem(key, '1');
      }
    });

    accordion.addEventListener('hidden.bs.collapse', function(ev) {
      const section = ev.target;
      const sectionId = section.getAttribute('data-mnw-section');
      if (sectionId) {
        const key = 'mnw-accordion-' + accordionId + '-' + sectionId;
        localStorage.removeItem(key);
      }
    });
  });
}

document.addEventListener('DOMContentLoaded', function() {
  mnw.init_zoom();
  mnw.init_accordions();
});
