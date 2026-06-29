"use strict";
const version = "1.82";

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
    console.log("Connection closed, detached");
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
          if (etag) element.setAttribute('data-mnw-stamp', etag);
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
  //if (event) event.preventDefault();
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
      'Content-Type': 'application/json',
      'X-Form-Submit': 'json'
    },
    redirect: 'manual'
  })
  .then(response => {
    if (response.status === 0 || response.type === 'opaque') {
      console.warn('Opaque response: browser blocked cross-origin redirect header');
      return Promise.reject(new Error('CORS/Network block'));
    }
    else if (response.status === 302 || response.status === 301 || response.status === 307 || response.status === 308) {
      const location = response.headers.get('Location');
      if (location) {
        window.location.href = location;
      } else {
        window.location.href = formElement.action || window.location.href;
      }
      return Promise.resolve(null);
    }
    return response.text().then(text => ({ type: response.type, status: response.status, message: text }));
  })
  .then(result => {
    if (!result) return;
    let json;
    try {
      json = JSON.parse(result.message);
    } catch (e) {
      if (result.type === 'error' || !result.type) {
        mnw.showToast('Error ' + result.status, 'danger');
      } else {
        mnw.showToast('Invalid JSON response', 'danger');
      }
      return;
    }
    if (json.redirect) {
      window.location.href = json.redirect;
    } else if (json.type === 'error' || !result.type) {
      mnw.showToast(json.message || 'Request failed', 'danger');
    } else if (json.message) {
      mnw.showToast(json.message, json.type || 'info');
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
  document.querySelectorAll('form').forEach(form => {
    form.addEventListener('reset', e => {
      // setTimeout ensures the DOM values are actually reset before triggering
      setTimeout(() => {
        e.target.querySelectorAll('input, select, textarea').forEach(el => {
          el.dispatchEvent(new Event('input', { bubbles: true }));
        });
      }, 0);
    });
  });

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

/* Bootstrap Functions */

mnw.showToast = function(content, type = "warning")
{
  var delay = 15000;
  var safeContent = String(content).replace(/[&<>"']/g, function(m) {
    return ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' })[m];
  });
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
                         <div class="toast-body h6 p-3 m-0">${safeContent}</div>
                         <button type="button" class="btn-close btn-close-black me-2 m-auto" data-bs-dismiss="toast" aria-label="Close"></button>
                       </div>`;

  toastContainer.appendChild(element);

  var toast = new bootstrap.Toast(element, {delay: delay, autohide: true, animation: true});

  element.addEventListener('hidden.bs.toast', function () {
    element.remove();
  })

  toast.show();
}

/* UI functions */

mnw.switch_theme = function(sender, event)
{
  let bs_theme = 'dark';
  if (document.body.getAttribute('data-bs-theme') == 'dark')
    bs_theme = 'light';
  document.body.setAttribute('data-bs-theme', bs_theme);
  localStorage.setItem('mnw-theme', bs_theme);
}

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
  document.querySelectorAll('[data-mnw-savestate]').forEach(function(accordion) {
    const accordionId = accordion.id;
    if (!accordionId) return;

    const targetId = accordion.getAttribute('data-bs-target');
    const collapseElement = document.querySelector(targetId);
    if (collapseElement)
    {
      const savedState = localStorage.getItem('mnw-accordion-' + accordionId + '-' + collapseElement.id);
      if (savedState === 'show') {
        const bsCollapse = new bootstrap.Collapse(collapseElement, { toggle: false });
        bsCollapse.show();
      } else if (savedState === 'hide') {
        const bsCollapse = new bootstrap.Collapse(collapseElement, { toggle: false });
        bsCollapse.hide();
      }

      collapseElement.addEventListener('show.bs.collapse', function(event) {
        const collapseId = event.target.id;
        localStorage.setItem('mnw-accordion-' + accordionId + '-' + collapseId, 'show');
      });

      collapseElement.addEventListener('hide.bs.collapse', function(event) {
        const collapseId = event.target.id;
        localStorage.setItem('mnw-accordion-' + accordionId + '-' + collapseId, 'hide');
      });
    }
  });
}

document.addEventListener('DOMContentLoaded', function()
{
  const el = document.querySelector('.version');
  if (el) el.textContent += ' js: ' + version+'';

  mnw.init_zoom();
  mnw.init_accordions();
});
