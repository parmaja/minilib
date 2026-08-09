"use strict";
const version = "1.84";
//used <script src="https://cdn.jsdelivr.net/npm/js-sha256@0.11.0/src/sha256.min.js"></script>

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

mnw.formPost = async function(e, extraJson) {
  if (e) e.preventDefault();
  const formElement = e.target;

  // Collect all native inputs as JSON (handles checkboxes, radios, files automatically)
  const data = Object.fromEntries(new FormData(formElement));

  if (extraJson)
    Object.assign(data, extraJson);

  const action = e.submitter.getAttribute('data-action') || "";
  if (action)
    data['action'] = action;

  // Hash password fields before submission
  for (const el of formElement.querySelectorAll('input[type="password"]')) {
    const token = el.getAttribute('data-token') || "";
    const name = el.getAttribute('name') || "";

    if (token && name) {
      data[name] = await sha256(el.value + '-' + token);
    }
  }

  /* use form.addEventListener("formdata", function(e) .... */

  formElement.querySelectorAll('[name], [data-name], [data-field-name]').forEach(el => {
    if (typeof el.setJSON === 'function') {
        el.setJSON(data);
    }
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
  element.className = `toast align-items-center bg-${type.toLowerCase()} text-black border-black shadow-thin`;
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

mnw.switch_theme = function(e)
{
  let bs_theme = 'dark';
  if (document.body.getAttribute('data-bs-theme') == 'dark')
    bs_theme = 'light';
  document.body.setAttribute('data-bs-theme', bs_theme);
  document.body.setAttribute('data-theme', bs_theme); //* Some Addons/Controls may use it like Marked.js
  localStorage.setItem('mnw-theme', bs_theme);
}

mnw.switch_zoom = function(e)
{
  let mnw_zoom = e.currentTarget.getAttribute('data-mnw-value') || '';
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
    let mnw_zoom = localStorage.getItem('mnw-zoom');
    if (mnw_zoom)
      document.documentElement.setAttribute('data-mnw-zoom', mnw_zoom);
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

/* Binding (Bind property in TmnwElement) */

//A TCheckBox or TSelect with data-bind-group toggles the visibility (d-none)
//of every other element that has the same data-bind-group, itself is never changed.
mnw.apply_binding = function(trigger)
{
  const group = trigger.getAttribute('data-bind-group');
  if (!group) return;
  const action = trigger.getAttribute('data-bind-action') || 'visible';

  //The attribute may sit on a wrapper element (i.e. checkbox outer div)
  const checkbox = trigger.matches('input[type="checkbox"]')
    ? trigger
    : trigger.querySelector('input[type="checkbox"]');
  const select = trigger.matches('select')
    ? trigger
    : trigger.querySelector('select');

  const checked = checkbox ? checkbox.checked : null;
  const value = select ? select.value : null;

  document.querySelectorAll('[data-bind-group="' + group + '"]').forEach(target => {
    if (target === trigger) return;

    let visible = false;
    if (checkbox)
      visible = checked;
    else if (select)
      visible = (target.getAttribute('data-bind-name') === value);

    if (action === 'enabled')
    {
      target.querySelectorAll('input, select, textarea, button').forEach(c => c.disabled = !visible);
    }
    else
    {
      target.classList.toggle('d-none', !visible);
    }
  });
};

mnw.init_bindings = function()
{
  document.addEventListener('change', function(e) {
    const el = e.target;
    if (!el.matches('input[type="checkbox"], select')) return;
    const trigger = el.closest('[data-bind-group]');
    if (trigger)
      mnw.apply_binding(trigger);
  });

  //Apply the initial state of every trigger
  document.querySelectorAll('[data-bind-group]').forEach(el => {
    if (el.matches('input[type="checkbox"], select') || el.querySelector('input[type="checkbox"], select'))
      mnw.apply_binding(el);
  });
};

document.addEventListener('DOMContentLoaded', function()
{
  const el = document.querySelector('.version');
  if (el) el.textContent += ' js: ' + version+'';

  //mnw.init_zoom(); moved to html
  mnw.init_accordions();
  mnw.init_bindings();
  mnw.init_masks();
});

/* Masked Inputs (data-mask attribute) */

//'9' required digit, '0' optional digit, '#' optional digit, 'A' required letter,
//'*' required alphanumeric, '.' and ',' optional decimal separator slots
mnw.maskChars = {
  '9': /[0-9]/,
  '0': /[0-9]/,
  '#': /[0-9]/,
  'A': /[A-Za-z]/,
  '*': /[A-Za-z0-9]/,
  '.': /[.,]/,
  ',': /[.,]/
};

mnw.maskPresets = {
  date:     '99/99/9999',
  time:     '00:00',
  datetime: '99/99/9999 00:00',
  phone:    '(999) 999-9999',
  number:   '999999.99',
  zip:      '99999-9999'
};

mnw.mask_slots = function(pattern)
{
  const slots = [];
  for (const ch of pattern)
  {
    if (mnw.maskChars[ch])
      slots.push({ char: ch, optional: (ch === '0' || ch === '#' || ch === '.' || ch === ',') });
    else
      slots.push({ literal: ch });
  }
  return slots;
};

mnw.mask_pattern = function(input)
{
  let pattern = input.getAttribute('data-mask');
  if (!pattern) return '';
  if (mnw.maskPresets[pattern])
    pattern = mnw.maskPresets[pattern];
  return pattern;
};

//The index of the decimal separator slot ('.' or ','), -1 when the pattern has none
mnw.mask_sep_slot = function(pattern)
{
  const slots = mnw.mask_slots(pattern);
  for (let i = 0; i < slots.length; i++)
    if (slots[i].char === '.' || slots[i].char === ',') return i;
  return -1;
};

//Strip the literals and return the raw typed value
mnw.unmask_value = function(value, pattern)
{
  const slots = mnw.mask_slots(pattern);
  const sepSi = mnw.mask_sep_slot(pattern);
  let raw = '';
  let si = 0;
  for (const ch of value)
  {
    while (si < slots.length && slots[si].literal !== undefined) si++;
    if (si >= slots.length) break;
    if (sepSi >= 0 && si < sepSi && (ch === '.' || ch === ',') &&
        raw.indexOf('.') < 0 && raw.indexOf(',') < 0)
    {
      //a separator typed before its position jumps to the separator slot
      raw += ch;
      si = sepSi + 1;
      continue;
    }
    if (mnw.maskChars[slots[si].char].test(ch))
    {
      raw += ch;
      si++;
    }
  }
  return raw;
};

//Build the formatted value from a raw value
mnw.mask_value = function(raw, pattern)
{
  const slots = mnw.mask_slots(pattern);
  const sepSi = mnw.mask_sep_slot(pattern);

  //split the raw value at its separator, the integer part fills the slots
  //before the separator slot and the fraction part the slots after it
  let intRaw = raw;
  let fracRaw = '';
  let sepChar = '';
  if (sepSi >= 0)
  {
    const sepIdx = raw.search(/[.,]/);
    if (sepIdx >= 0)
    {
      intRaw = raw.substring(0, sepIdx);
      fracRaw = raw.substring(sepIdx + 1);
      sepChar = raw[sepIdx];
    }
  }

  let out = '';
  let ri = 0;
  let part = intRaw;
  let si = 0;
  while (si < slots.length)
  {
    const slot = slots[si];
    if (slot.literal !== undefined)
    {
      //only show the literal when there are still chars to type after it
      if (ri < part.length)
        out += slot.literal;
      si++;
      continue;
    }
    if (si === sepSi)
    {
      if (sepChar)
      {
        out += sepChar;
        ri = 0;
        part = fracRaw;
      }
      si++;
      continue;
    }
    if (ri >= part.length)
    {
      si++;
      continue;
    }
    const ch = part[ri];
    if (mnw.maskChars[slot.char].test(ch))
    {
      out += ch;
      ri++;
      si++;
    }
    else if (slot.optional)
      si++; //skip the slot, keep the char for the next slot
    else
      ri++; //drop the char, retry the slot
  }
  return out;
};

mnw.mask_caret = function(pattern, oldValue, start, masked)
{
  //caret was at the end, keep it at the end
  if (start >= oldValue.length)
    return masked.length;
  const rawBefore = mnw.unmask_value(oldValue.substring(0, start), pattern);
  const slots = mnw.mask_slots(pattern);
  let ri = 0;
  let pos = 0;
  for (let si = 0; si < slots.length; si++)
  {
    const slot = slots[si];
    if (slot.literal !== undefined)
    {
      pos++;
      continue;
    }
    if (ri >= rawBefore.length) break;
    ri++;
    pos++;
  }
  //skip the literals that follow the last typed char
  while (pos < masked.length && slots[pos] && slots[pos].literal !== undefined)
    pos++;
  return Math.min(pos, masked.length);
};

mnw.apply_mask = function(input)
{
  if (input._mnwMasked) return;
  input._mnwMasked = true;
  const pattern = mnw.mask_pattern(input);
  if (pattern)
  {
    //format the initial value, digits-only masks get a numeric keyboard
    const masked = mnw.mask_value(mnw.unmask_value(input.value, pattern), pattern);
    if (masked !== input.value)
      input.value = masked;
    if (!/[A*]/.test(pattern) && !input.hasAttribute('inputmode'))
      input.setAttribute('inputmode', 'numeric');
  }
  input.addEventListener('input', function(e)
  {
    const el = e.target;
    const pattern = mnw.mask_pattern(el);
    if (!pattern) return;
    const start = el.selectionStart || 0;
    const oldValue = el.value;
    const masked = mnw.mask_value(mnw.unmask_value(oldValue, pattern), pattern);
    if (masked === oldValue) return;
    el.value = masked;
    const pos = mnw.mask_caret(pattern, oldValue, start, masked);
    try { el.setSelectionRange(pos, pos); } catch (err) { /* ignore */ }
  });
};

mnw.init_masks = function()
{
  document.querySelectorAll('input[data-mask]').forEach(mnw.apply_mask);
};

/* Confirm Modal */

let _confirming = false;

document.addEventListener('click', async function(e) {
  if (_confirming) return;

  const confirmEl = e.target.closest('[data-mnw-confirm]');
  if (!confirmEl) return;

  const message = confirmEl.getAttribute('data-mnw-confirm');
  if (!message) return;

  e.preventDefault();
  e.stopPropagation();
  e.stopImmediatePropagation();

  const confirmed = await mnw.confirm(message);
  if (confirmed) {
    _confirming = true;
    confirmEl.click();
    _confirming = false;
  }
}, true);

mnw.confirm = function(message) {
  return new Promise(function(resolve) {
    var modalEl = document.getElementById('mnw-confirm-modal');
    if (!modalEl) {
      modalEl = document.createElement('div');
      modalEl.id = 'mnw-confirm-modal';
      modalEl.className = 'modal fade';
      modalEl.setAttribute('tabindex', '-1');
      modalEl.setAttribute('aria-hidden', 'true');
      modalEl.innerHTML =
        '<div class="modal-dialog modal-dialog-centered modal-sm">' +
          '<div class="modal-content">' +
            '<div class="modal-body h6 p-4 pb-0"></div>' +
            '<div class="modal-footer border-0 pt-2">' +
              '<button type="button" class="btn btn-secondary" data-mnw-confirm-no>No</button>' +
              '<button type="button" class="btn btn-primary" data-mnw-confirm-yes>Yes</button>' +
            '</div>' +
          '</div>' +
        '</div>';
      document.body.appendChild(modalEl);
    }

    modalEl.querySelector('.modal-body').textContent = message;

    var modal = new bootstrap.Modal(modalEl, { backdrop: 'static', keyboard: false });

    function onDone(result) {
      modalEl.removeEventListener('keydown', onKeyDown);
      modal.hide();
      resolve(result);
    }

    function onKeyDown(e) {
      if (e.key === 'Enter') {
        e.preventDefault();
        onDone(true);
      } else if (e.key === 'Escape') {
        e.preventDefault();
        onDone(false);
      }
    }

    modalEl.addEventListener('keydown', onKeyDown);

    modalEl.querySelector('[data-mnw-confirm-yes]').onclick = function() {
      onDone(true);
    };

    modalEl.querySelector('[data-mnw-confirm-no]').onclick = function() {
      onDone(false);
    };

    modal.show();
  });
};
