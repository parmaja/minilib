/*
  mnwAPIDoc - OpenAPI/Swagger documentation viewer.

  Part of the "Mini Library"

  @license   modifiedLGPL (modified of mod://www.gnu.org/licenses/lgpl.html)
             See the file COPYING.MLGPL, included in this distribution,
  @author    Zaher Dirkey <zaher, zaherdirkey>

  Pure vanilla JavaScript, no external dependencies. The companion Pascal
  control (mnwAPIDoc.pas) compiles this file into resources and serves it
  from the assets schema.

  Usage:
    mnwAPIDoc.render(document.getElementById('api'), {
      url: '/assets/spec/openapi.json',          // fetch the spec from URL
      content: specObjectOrJsonString,           // or give the spec directly
      contentElementId: 'api_spec',              // or read it from a <script> template
      labels: {},                                // UI strings (default English)
      theme: 'auto',                             // auto | light | dark
      bootstrapTheme: true,                      // follow Bootstrap CSS variables
      serverUrl: '',                             // override the spec servers
      proxyUrl: '',                              // CORS proxy for TryIt, use {url} placeholder
      headingText: '',                           // small text above the title
      height: '',                                // CSS height (default fills the viewport)
      credentials: 'same-origin',                // fetch credentials for TryIt
      search: true, tryIt: true, download: true, sidebar: true,
      expandSections: false, expandOperations: false, expandSchemas: false,
      hideModels: false, sortTags: false, sortOperations: false,
      fillBody: true, showCurl: false,
      customCss: ''
    });

  Supports OpenAPI 3.x and Swagger 2.0 JSON documents (inline or by URL).
*/
var mnwAPIDoc = (function () {
  'use strict';

  var METHOD_ORDER = ['get', 'post', 'put', 'delete', 'patch', 'head', 'options', 'trace'];

  var defaultLabels = {
    overview: 'Overview',
    servers: 'Servers',
    authentication: 'Authentication',
    download: 'Download spec',
    models: 'Models',
    search: 'Search...',
    parameters: 'Parameters',
    requestBody: 'Request body',
    responses: 'Responses',
    tryit: 'Try it out',
    send: 'Send',
    clear: 'Clear',
    status: 'Status',
    time: 'Time',
    headers: 'Headers',
    body: 'Body',
    example: 'Example',
    required: 'required',
    deprecated: 'deprecated',
    noresult: 'No matching operations',
    loaderror: 'Failed to load the specification',
    name: 'Name',
    type: 'Type',
    in: 'In',
    description: 'Description',
    nocontent: 'No content'
  };

  /* ---------------------------------------------------------------- CSS */

  var STYLE_CSS = `
.mnw-apidoc {
  --ap-bg: #ffffff;
  --ap-bg2: #f6f8fa;
  --ap-fg: #24292f;
  --ap-muted: #57606a;
  --ap-border: #d0d7de;
  --ap-hover: #eaeef2;
  --ap-primary: #3b82f6;
  --ap-code-bg: #f0f2f5;
  --ap-get: #2f9de4;
  --ap-post: #16a085;
  --ap-put: #e67e22;
  --ap-delete: #e74c3c;
  --ap-patch: #8e44ad;
  --ap-other: #7f8c8d;
  display: flex;
  flex-direction: row;
  /*height: calc(100vh - 96px);*/
  min-height: 420px;
  border: 1px solid var(--ap-border);
  /*border-radius: 8px;*/
  overflow: hidden;
  background: var(--ap-bg);
  color: var(--ap-fg);
  font-size: 14px;
  line-height: 1.55;
}
.mnw-apidoc[data-mnw-theme="dark"] {
  --ap-bg: #161b22;
  --ap-bg2: #1c2128;
  --ap-fg: #e6edf3;
  --ap-muted: #8b949e;
  --ap-border: #30363d;
  --ap-hover: #21262d;
  --ap-primary: #58a6ff;
  --ap-code-bg: #1c2128;
}
.mnw-apidoc[data-mnw-bs="1"] {
  --ap-bg: var(--bs-body-bg, #ffffff);
  --ap-bg2: var(--bs-tertiary-bg, #f6f8fa);
  --ap-fg: var(--bs-body-color, #24292f);
  --ap-muted: var(--bs-secondary-color, #57606a);
  --ap-border: var(--bs-border-color, #d0d7de);
  --ap-hover: rgba(127, 127, 127, 0.14);
  --ap-primary: var(--bs-primary, #3b82f6);
  --ap-code-bg: var(--bs-tertiary-bg, #f0f2f5);
}
.mnw-apidoc * { box-sizing: border-box; }
.mnw-apidoc a { color: var(--ap-primary); text-decoration: none; }
.mnw-apidoc a:hover { text-decoration: underline; }
.mnw-apidoc code, .mnw-apidoc pre {
  font-family: ui-monospace, SFMono-Regular, "JetBrains Mono", Consolas, monospace;
  font-size: 12.5px;
}
.mnw-apidoc code { background: var(--ap-code-bg); padding: 1px 5px; border-radius: 4px; }
.mnw-apidoc pre {
  background: var(--ap-code-bg);
  border: 1px solid var(--ap-border);
  border-radius: 6px;
  padding: 10px 12px;
  overflow: auto;
  margin: 8px 0;
}
.mnw-apidoc pre code { background: none; padding: 0; }

/* --- side navigation --- */
.ap-nav {
  width: 292px;
  min-width: 250px;
  display: flex;
  flex-direction: column;
  border-inline-end: 1px solid var(--ap-border);
  background: var(--ap-bg2);
}
.ap-nav-head {
  padding: 14px 16px 10px;
  border-bottom: 1px solid var(--ap-border);
}
.ap-nav-title { font-weight: 700; font-size: 15px; }
.ap-nav-version {
  display: inline-block;
  margin-inline-start: 6px;
  padding: 0 7px;
  font-size: 11px;
  font-weight: 600;
  border: 1px solid var(--ap-border);
  border-radius: 10px;
  color: var(--ap-muted);
  vertical-align: middle;
}
.ap-nav-search { padding: 10px 12px; }
.ap-nav-search input {
  width: 100%;
  padding: 6px 10px;
  border: 1px solid var(--ap-border);
  border-radius: 6px;
  background: var(--ap-bg);
  color: var(--ap-fg);
  outline: none;
}
.ap-nav-search input:focus { border-color: var(--ap-primary); }
.ap-nav-list { flex: 1; overflow-y: auto; padding: 4px 0 24px; }
.ap-nav-group { margin: 2px 0; }
.ap-nav-group > summary {
  padding: 7px 16px;
  font-weight: 600;
  font-size: 12px;
  text-transform: uppercase;
  letter-spacing: 0.04em;
  color: var(--ap-muted);
  cursor: pointer;
  list-style: none;
  display: flex;
  align-items: center;
  gap: 6px;
}
.ap-nav-group > summary::before { content: "\\25B8"; transition: transform 0.12s; }
.ap-nav-group[open] > summary::before { transform: rotate(90deg); }
.ap-nav-group > summary::-webkit-details-marker { display: none; }
.ap-nav-op {
  display: flex;
  align-items: baseline;
  gap: 8px;
  padding: 5px 16px 5px 26px;
  cursor: pointer;
  border-inline-start: 2px solid transparent;
}
.ap-nav-op:hover { background: var(--ap-hover); }
.ap-nav-op.ap-active { background: var(--ap-hover); border-inline-start-color: var(--ap-primary); }
.ap-nav-op .ap-nav-text {
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  color: var(--ap-fg);
}
.ap-nav-op.ap-deprecated .ap-nav-text { text-decoration: line-through; opacity: 0.6; }
.ap-nav-empty { padding: 8px 16px; color: var(--ap-muted); font-style: italic; }

/* --- method badges --- */
.ap-method {
  flex: none;
  font-size: 10.5px;
  font-weight: 700;
  letter-spacing: 0.03em;
  padding: 1px 6px;
  border-radius: 4px;
  color: #fff;
  text-transform: uppercase;
  min-width: 52px;
  text-align: center;
}
.ap-method-get { background: var(--ap-get); }
.ap-method-post { background: var(--ap-post); }
.ap-method-put { background: var(--ap-put); }
.ap-method-delete { background: var(--ap-delete); }
.ap-method-patch { background: var(--ap-patch); }
.ap-method-head, .ap-method-options, .ap-method-trace { background: var(--ap-other); }

/* --- main content --- */
.ap-main { flex: 1; overflow-y: auto; padding: 20px 28px 80px; scroll-behavior: smooth; }
.ap-main h1 { font-size: 26px; margin: 0 0 4px; }
.ap-main h2 {
  font-size: 19px;
  margin: 34px 0 10px;
  padding-bottom: 6px;
  border-bottom: 1px solid var(--ap-border);
}
.ap-main h3 { font-size: 15.5px; margin: 18px 0 6px; }
.ap-main p { margin: 6px 0; }
.ap-heading-text { color: var(--ap-muted); font-size: 13px; text-transform: uppercase; letter-spacing: 0.05em; }
.ap-version-badge {
  display: inline-block;
  padding: 1px 10px;
  font-size: 12px;
  font-weight: 600;
  border: 1px solid var(--ap-primary);
  color: var(--ap-primary);
  border-radius: 12px;
  vertical-align: middle;
}
.ap-doc-meta { color: var(--ap-muted); font-size: 13px; margin: 4px 0 12px; }
.ap-toolbar { display: flex; gap: 8px; flex-wrap: wrap; margin: 10px 0 4px; }
.ap-chip {
  display: inline-block;
  padding: 2px 10px;
  font-size: 12px;
  border: 1px solid var(--ap-border);
  border-radius: 12px;
  background: var(--ap-bg2);
  color: var(--ap-fg);
  cursor: pointer;
}
.ap-chip:hover { border-color: var(--ap-primary); text-decoration: none; }
.ap-tag-desc { color: var(--ap-muted); margin: 2px 0 12px; }

/* --- sections --- */
.ap-section { margin: 22px 0 12px; }
.ap-section > summary {
  cursor: pointer;
  list-style: none;
  display: flex;
  align-items: center;
  gap: 8px;
  padding: 2px 14px;
  background: var(--ap-bg2);
  border: 1px solid var(--ap-border);
  border-radius: 8px;
  /*line-height: 2rem;*/
}
.ap-section > summary::-webkit-details-marker { display: none; }
.ap-section > summary::before { content: "\\25B8"; flex: none; color: var(--ap-muted); transition: transform 0.12s; font-size: 2.0em; }
.ap-section[open] > summary::before { transform: rotate(90deg); }
.ap-section > summary:hover { background: var(--ap-hover); }
.ap-section > summary h2 { margin: 0; border: none; padding: 0; }
.ap-section-body { padding: 2px 2px 0; }

/* --- operation card --- */
.ap-op {
  border: 1px solid var(--ap-border);
  border-radius: 8px;
  margin: 12px 0;
  background: var(--ap-bg);
  overflow: hidden;
}
.ap-op > summary {
  display: flex;
  align-items: center;
  gap: 10px;
  padding: 10px 14px;
  cursor: pointer;
  list-style: none;
  background: var(--ap-bg2);
}
.ap-op > summary::-webkit-details-marker { display: none; }
.ap-op > summary::before { content: "\\25B8"; flex: none; color: var(--ap-muted); transition: transform 0.12s; }
.ap-op[open] > summary::before { transform: rotate(90deg); }
.ap-op > summary:hover { background: var(--ap-hover); }
.ap-op-path {
  font-family: ui-monospace, SFMono-Regular, Consolas, monospace;
  font-size: 13px;
  word-break: break-all;
}
.ap-op-params {
  color: var(--ap-muted);
  font-family: ui-monospace, SFMono-Regular, Consolas, monospace;
  font-size: 11.5px;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  min-width: 0;
}
.ap-op-summary { color: var(--ap-muted); font-size: 12.5px; margin-inline-start: auto; text-align: end; }
.ap-op-body { padding: 6px 16px 16px; border-top: 1px solid var(--ap-border); }
.ap-badge-deprecated {
  font-size: 11px;
  font-weight: 700;
  color: #b91c1c;
  border: 1px solid #b91c1c;
  border-radius: 4px;
  padding: 0 5px;
  text-transform: uppercase;
}
.ap-desc { margin: 8px 0; }
.ap-section-title {
  font-size: 12.5px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.04em;
  color: var(--ap-muted);
  margin: 16px 0 6px;
}

/* --- tables --- */
.ap-table { width: 100%; border-collapse: collapse; margin: 6px 0; }
.ap-table th, .ap-table td {
  text-align: start;
  padding: 6px 10px;
  border: 1px solid var(--ap-border);
  vertical-align: top;
  font-size: 13px;
}
.ap-table th { background: var(--ap-bg2); font-weight: 600; }
.ap-cell-name { font-family: ui-monospace, SFMono-Regular, Consolas, monospace; font-weight: 600; }
.ap-req-star { color: #dc2626; font-weight: 700; }
.ap-type { color: var(--ap-muted); font-family: ui-monospace, SFMono-Regular, Consolas, monospace; }

/* --- schema tree --- */
.ap-schema { margin-inline-start: 0; background: var(--ap-bg2); }
.ap-schema ul { list-style: none; margin: 0; padding-inline-start: 18px; }
.ap-schema > ul { padding-inline-start: 4px; }
.ap-row { padding: 2.5px 0; border-bottom: 1px dashed transparent; }
.ap-row > summary { cursor: pointer; list-style: none; display: flex; align-items: baseline; gap: 8px; flex-wrap: wrap; }
.ap-row > summary::-webkit-details-marker { display: none; }
.ap-row > summary::before { content: ""; width: 12px; flex: none; }
details.ap-row > summary::before { content: "\\25B8"; transition: transform 0.12s; }
details.ap-row[open] > summary::before { transform: rotate(90deg); }
.ap-prop-name { font-family: ui-monospace, SFMono-Regular, Consolas, monospace; font-weight: 600; }
.ap-row-summary { display: flex; align-items: baseline; gap: 8px; flex-wrap: wrap; }
.ap-prop-meta { color: var(--ap-muted); font-size: 12px; }
.ap-prop-desc { color: var(--ap-muted); font-size: 12.5px; flex-basis: 100%; margin-inline-start: 20px; }
.ap-ref-link { font-size: 12px; }

/* --- responses --- */
.ap-resp { border: 1px solid var(--ap-border); border-radius: 6px; margin: 8px 0; overflow: hidden; }
.ap-resp > summary {
  cursor: pointer;
  padding: 7px 12px;
  display: flex;
  align-items: center;
  gap: 10px;
  background: var(--ap-bg2);
  list-style: none;
}
.ap-resp > summary::-webkit-details-marker { display: none; }
.ap-resp > summary::before { content: "\\25B8"; flex: none; color: var(--ap-muted); transition: transform 0.12s; }
.ap-resp[open] > summary::before { transform: rotate(90deg); }
.ap-status {
  font-weight: 700;
  font-family: ui-monospace, SFMono-Regular, Consolas, monospace;
  padding: 0 8px;
  border-radius: 4px;
  color: #fff;
}
.ap-status-2 { background: #16a085; }
.ap-status-3 { background: #2f9de4; }
.ap-status-4 { background: #e67e22; }
.ap-status-5 { background: #e74c3c; }
.ap-status-x { background: #7f8c8d; }
.ap-resp-body { padding: 8px 14px 14px; border-top: 1px solid var(--ap-border); }

/* --- try it --- */
.ap-try { border: 1px solid var(--ap-primary); border-radius: 6px; margin: 12px 0; overflow: hidden; }
.ap-try > summary {
  cursor: pointer;
  padding: 8px 12px;
  font-weight: 600;
  background: var(--ap-bg2);
  list-style: none;
  color: var(--ap-primary);
  display: flex;
  align-items: center;
  gap: 8px;
}
.ap-try > summary::-webkit-details-marker { display: none; }
.ap-try > summary::before { content: "\\25B8"; flex: none; transition: transform 0.12s; }
.ap-try[open] > summary::before { transform: rotate(90deg); }
.ap-try-body { padding: 10px 14px 14px; border-top: 1px solid var(--ap-border); }
.ap-field { margin: 6px 0; display: flex; flex-direction: column; gap: 2px; }
.ap-field label { font-size: 12px; font-weight: 600; color: var(--ap-muted); }
.ap-field label .ap-f-in { font-weight: 400; }
.ap-field input, .ap-field select, .ap-field textarea {
  padding: 5px 9px;
  border: 1px solid var(--ap-border);
  border-radius: 6px;
  background: var(--ap-bg);
  color: var(--ap-fg);
  font: inherit;
  outline: none;
}
.ap-field textarea {
  font-family: ui-monospace, SFMono-Regular, Consolas, monospace;
  font-size: 12.5px;
  min-height: 110px;
  resize: vertical;
  width: 100%;
}
.ap-field input:focus, .ap-field select:focus, .ap-field textarea:focus { border-color: var(--ap-primary); }
.ap-try-actions { display: flex; gap: 6px; align-items: center; margin-inline-start: auto; flex: none; flex-wrap: wrap; }
.ap-try-title { flex: 1 1 auto; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.ap-btn {
  padding: 5px 16px;
  border: 1px solid var(--ap-primary);
  background: var(--ap-primary);
  color: #fff;
  border-radius: 6px;
  cursor: pointer;
  font-weight: 600;
  font-size: 13px;
}
.ap-btn:disabled { opacity: 0.55; cursor: default; }
.ap-btn.ap-btn-plain { background: none; color: var(--ap-fg); border-color: var(--ap-border); }
.ap-try-result { margin-top: 12px; }
.ap-try-result .ap-resp-body { border-top: 1px solid var(--ap-border); }

/* --- markdown --- */
.ap-md h1, .ap-md h2, .ap-md h3, .ap-md h4 { margin: 14px 0 6px; }
.ap-md h1 { font-size: 20px; }
.ap-md h2 { font-size: 17px; border: none; padding: 0; }
.ap-md ul, .ap-md ol { margin: 6px 0; padding-inline-start: 24px; }
.ap-md blockquote { margin: 6px 0; padding-inline-start: 12px; border-inline-start: 3px solid var(--ap-border); color: var(--ap-muted); }
.ap-md table { border-collapse: collapse; }
.ap-md td, .ap-md th { border: 1px solid var(--ap-border); padding: 4px 8px; }

.ap-error {
  padding: 14px 18px;
  border: 1px solid var(--ap-delete);
  color: var(--ap-delete);
  border-radius: 8px;
  background: var(--ap-bg);
}
@media (max-width: 760px) {
  .mnw-apidoc { flex-direction: column; height: auto; }
  .ap-nav { width: 100%; max-height: 300px; border-inline-end: none; border-bottom: 1px solid var(--ap-border); }
  .ap-main { overflow: visible; }
}
`;

  /* ------------------------------------------------------------ helpers */

  function esc(s) {
    return String(s === null || s === undefined ? '' : s)
      .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
  }

  function el(tag, cls, html) {
    var e = document.createElement(tag);
    if (cls) { e.className = cls; }
    if (html !== undefined && html !== null) { e.innerHTML = html; }
    return e;
  }

  function isPlainObject(v) {
    return v !== null && typeof v === 'object' && !Array.isArray(v);
  }

  function formatJSON(v) {
    try { return JSON.stringify(v, null, 2); } catch (e) { return String(v); }
  }

  function sanitizeId(s) {
    return String(s).replace(/[^A-Za-z0-9_\-]+/g, '-');
  }

  var styleInjected = false;
  function ensureStyle() {
    if (styleInjected || document.getElementById('mnw-apidoc-style')) {
      styleInjected = true;
      return;
    }
    var s = document.createElement('style');
    s.id = 'mnw-apidoc-style';
    s.textContent = STYLE_CSS;
    document.head.appendChild(s);
    styleInjected = true;
  }

  /* ------------------------------------------------------- spec helpers */

  function resolveRef(root, ref) {
    if (!ref || ref.charAt(0) !== '#') { return null; }
    var parts = ref.substring(1).split('/');
    var cur = root;
    for (var i = 1; i < parts.length; i++) {
      var p = parts[i].replace(/~1/g, '/').replace(/~0/g, '~');
      if (cur === null || cur === undefined) { return null; }
      cur = cur[p];
    }
    return cur === undefined ? null : cur;
  }

  // Follow $ref (with sibling overrides) up to a few levels
  function deref(root, node) {
    var guard = 0;
    while (node && node.$ref && guard < 10) {
      guard++;
      var target = resolveRef(root, node.$ref);
      if (!target) { return node; }
      if (Object.keys(node).length === 1) { return target; }
      var merged = {};
      var k;
      for (k in target) { if (Object.prototype.hasOwnProperty.call(target, k)) { merged[k] = target[k]; } }
      for (k in node) {
        if (Object.prototype.hasOwnProperty.call(node, k) && k !== '$ref') { merged[k] = node[k]; }
      }
      node = merged;
    }
    return node;
  }

  function refName(ref) {
    if (!ref) { return ''; }
    var p = String(ref).split('/');
    return decodeURIComponent(p[p.length - 1]);
  }

  function isSwagger2(spec) {
    return String(spec.swagger || '').charAt(0) === '2';
  }

  function getSchemas(spec) {
    return (spec.components && spec.components.schemas) || spec.definitions || {};
  }

  function getServers(spec) {
    if (Array.isArray(spec.servers) && spec.servers.length) {
      return spec.servers.filter(function (s) { return s && s.url; });
    }
    if (spec.host) {
      var schemes = spec.schemes || ['http'];
      return schemes.map(function (sc) {
        return { url: sc + '://' + spec.host + (spec.basePath || '') };
      });
    }
    return [];
  }

  function getSecuritySchemes(spec) {
    return (spec.components && spec.components.securitySchemes) || spec.securityDefinitions || {};
  }

  // Swagger 2.0 parameter -> OpenAPI 3 style parameter (keeps name/in/description/required/schema)
  function normalizeParameter(spec, p) {
    p = deref(spec, p);
    if (!p) { return null; }
    if (!p.schema) {
      var schema = {};
      ['type', 'format', 'items', 'enum', 'default', 'collectionFormat'].forEach(function (k) {
        if (p[k] !== undefined) { schema[k] = p[k]; }
      });
      if (schema.type === 'array' && schema.collectionFormat) { delete schema.collectionFormat; }
      p = Object.assign({}, p, { schema: schema });
      delete p.type; delete p.format; delete p.items; delete p.enum; delete p.default;
    }
    return p;
  }

  // Swagger 2.0 response -> OpenAPI 3 style
  function normalizeResponse(spec, r) {
    r = deref(spec, r) || {};
    if (r.content) { return r; }
    var out = { description: r.description || '' };
    if (r.schema) {
      out.content = { 'application/json': { schema: deref(spec, r.schema) } };
      if (r.examples) { out.content['application/json'].example = r.examples; }
    }
    return out;
  }

  // Collect operations as [{name, description, operations:[...]}]
  function collectGroups(state) {
    var spec = state.spec;
    var groups = {};
    var order = [];

    function groupFor(name) {
      if (!groups[name]) {
        groups[name] = { name: name, description: '', operations: [] };
        order.push(groups[name]);
      }
      return groups[name];
    }

    (spec.tags || []).forEach(function (t) {
      t = deref(spec, t);
      if (t && t.name) {
        var g = groupFor(t.name);
        g.description = t.description || '';
      }
    });

    var paths = spec.paths || {};
    Object.keys(paths).forEach(function (path) {
      var item = deref(spec, paths[path]) || {};
      var common = (item.parameters || []).map(function (p) { return normalizeParameter(spec, p); })
        .filter(function (p) { return p; });
      METHOD_ORDER.forEach(function (method) {
        var op = item[method];
        if (!op) { return; }
        op = deref(spec, op);
        var tag = (op.tags && op.tags.length) ? op.tags[0] : 'default';
        var params = {};
        common.concat((op.parameters || []).map(function (p) { return normalizeParameter(spec, p); })
          .filter(function (p) { return p; }))
          .forEach(function (p) {
            if (p.in === 'body' || p.in === 'formData') { return; }
            params[p.in + '|' + p.name] = p;
          });
        var opInfo = {
          op: op,
          path: path,
          method: method,
          params: Object.keys(params).map(function (k) { return params[k]; }),
          id: 'op-' + sanitizeId(tag + '-' + (op.operationId || (method + ' ' + path)))
        };
        // requestBody: OpenAPI 3 or Swagger 2 body/formData parameter
        if (op.requestBody) {
          opInfo.body = deref(spec, op.requestBody);
        } else if (isSwagger2(spec)) {
          var bodyP = null, formPs = [];
          (op.parameters || []).forEach(function (raw) {
            var p = normalizeParameter(spec, raw);
            if (!p) { return; }
            if (p.in === 'body') { bodyP = p; }
            else if (p.in === 'formData') { formPs.push(p); }
          });
          if (bodyP) {
            var consumes = op.consumes || spec.consumes || ['application/json'];
            var content = {};
            consumes.forEach(function (ct) { content[ct] = { schema: bodyP.schema, description: bodyP.description || '' }; });
            opInfo.body = { content: content, required: !!bodyP.required, description: bodyP.description || '' };
          } else if (formPs.length) {
            var props = {}, req = [];
            formPs.forEach(function (p) {
              props[p.name] = Object.assign({}, p.schema, { description: p.description || '' });
              if (p.required) { req.push(p.name); }
            });
            opInfo.body = {
              content: { 'application/x-www-form-urlencoded': { schema: { type: 'object', properties: props, required: req } } },
              required: false, description: ''
            };
          }
        }
        var responses = {};
        var rawResp = op.responses || {};
        Object.keys(rawResp).forEach(function (code) {
          responses[code] = normalizeResponse(spec, rawResp[code]);
        });
        opInfo.responses = responses;
        groupFor(tag).operations.push(opInfo);
      });
    });

    var list = order.filter(function (g) { return g.operations.length > 0; });
    if (state.options.sortOperations) {
      list.forEach(function (g) {
        g.operations.sort(function (a, b) {
          return (a.path + a.method).localeCompare(b.path + b.method);
        });
      });
    }
    if (state.options.sortTags) {
      list.sort(function (a, b) { return a.name.localeCompare(b.name); });
    }
    return list;
  }

  /* ---------------------------------------------------- schema helpers */

  function schemaTypeName(schema) {
    if (!schema) { return 'any'; }
    if (schema.$ref) { return refName(schema.$ref); }
    var t = schema.type || (schema.properties ? 'object' : (schema.items ? 'array' : ''));
    if (schema.nullable && t) { t += ' | null'; }
    if (schema.format) { t += ' <' + schema.format + '>'; }
    if (schema.enum) { t = (t || 'string') + ' = ' + schema.enum.join(', '); }
    return t || (schema.allOf ? 'all of' : (schema.oneOf ? 'one of' : (schema.anyOf ? 'any of' : 'any')));
  }

  // Build an example JSON value from a schema. 'seen' tracks $refs along the
  // current branch so recursive models stop instead of nesting forever.
  function buildExample(schema, spec, depth, seen) {
    if (!isPlainObject(schema) || depth > 8) { return undefined; }
    seen = seen || [];
    if (schema.example !== undefined) { return schema.example; }
    if (schema.default !== undefined) { return schema.default; }
    if (Array.isArray(schema['x-examples']) && schema['x-examples'].length) { return schema['x-examples'][0]; }

    var s = schema;
    if (s.$ref) {
      var target = resolveRef(spec, s.$ref);
      if (!target) { return undefined; }
      if (seen.indexOf(s.$ref) >= 0) {
        var ty = target.type || (target.properties ? 'object' : (target.items ? 'array' : ''));
        return ty === 'array' ? [] : (ty === 'object' ? {} : undefined);
      }
      seen = seen.concat([s.$ref]);
      s = deref(spec, s);
    }
    var type = s.type || (s.properties ? 'object' : (s.items ? 'array' : (s.enum ? 'string' : '')));
    if (!type) {
      var comb = s.allOf || s.oneOf || s.anyOf;
      if (Array.isArray(comb) && comb.length) {
        if (s.allOf) {
          var merged = {};
          comb.forEach(function (c) {
            var v = buildExample(deref(spec, c), spec, depth + 1, seen);
            if (isPlainObject(v)) { Object.assign(merged, v); }
          });
          return merged;
        }
        return buildExample(deref(spec, comb[0]), spec, depth + 1, seen);
      }
      return undefined;
    }
    if (type === 'object') {
      var out = {};
      var props = s.properties || {};
      Object.keys(props).forEach(function (name) {
        var v = buildExample(props[name], spec, depth + 1, seen);
        if (v !== undefined) { out[name] = v; }
      });
      return out;
    }
    if (type === 'array') {
      var item = s.items ? buildExample(s.items, spec, depth + 1, seen) : undefined;
      return item === undefined ? [] : [item];
    }
    if (type === 'integer' || type === 'number') {
      if (s.enum && s.enum.length) { return s.enum[0]; }
      return type === 'integer' ? 1 : 1.5;
    }
    if (type === 'boolean') { return true; }
    if (type === 'string') {
      if (s.enum && s.enum.length) { return s.enum[0]; }
      switch (s.format) {
        case 'date-time': return new Date().toISOString();
        case 'date': return new Date().toISOString().substring(0, 10);
        case 'email': return 'user@example.com';
        case 'uuid': return '3fa85f64-5717-4562-b3fc-2c963f66afa6';
        case 'uri': case 'url': return 'https://example.com';
        case 'password': return '********';
        default: return 'string';
      }
    }
    return undefined;
  }

  /* ------------------------------------------------ schema tree renderer */

  // Renders one schema as a collapsible tree of rows
  function schemaTree(schema, state) {
    var box = el('div', 'ap-schema');
    box.appendChild(schemaRow(null, schema, state, 0, []));
    return box;
  }

  function schemaRow(name, rawSchema, state, level, stack, isRequired) {
    var spec = state.spec;
    var refPath = rawSchema && rawSchema.$ref;
    var schema = deref(spec, rawSchema) || {};
    var frag;

    var summary = el('div', 'ap-row-summary');
    if (name !== null) {
      var nameSpan = el('span', 'ap-prop-name', esc(name));
      summary.appendChild(nameSpan);
    }
    var meta = el('span', 'ap-prop-meta', esc(schemaTypeName(schema)));
    summary.appendChild(meta);
    if (isRequired) {
      summary.appendChild(el('span', 'ap-req-star', '*'));
    }
    if (schema.deprecated) {
      summary.appendChild(el('span', 'ap-badge-deprecated', esc(state.labels.deprecated)));
    }
    if (refPath) {
      var mId = 'model-' + sanitizeId(refName(refPath));
      var link = el('a', 'ap-ref-link', esc(refName(refPath)));
      link.href = '#' + mId;
      link.dataset.apScroll = mId;
      summary.appendChild(link);
    }
    var note = [];
    if (schema.default !== undefined) { note.push('default: ' + formatJSON(schema.default)); }
    if (schema.minimum !== undefined) { note.push('min: ' + schema.minimum); }
    if (schema.maximum !== undefined) { note.push('max: ' + schema.maximum); }
    if (schema.minLength !== undefined) { note.push('minLength: ' + schema.minLength); }
    if (schema.pattern) { note.push('pattern: ' + schema.pattern); }
    if (note.length) {
      summary.appendChild(el('span', 'ap-prop-meta', esc(note.join(' \u00b7 '))));
    }
    if (schema.description) {
      summary.appendChild(el('span', 'ap-prop-desc', mdInline(schema.description)));
    }

    // Children: object props / array items / combined subschemas.
    // A schema repeated up the ancestor chain is a cycle, stop there.
    var cycle = stack.indexOf(schema) >= 0 || level > 10;
    var requiredList = Array.isArray(schema.required) ? schema.required : [];
    var children = el('ul');
    var hasChildren = false;

    if (!cycle) {
      if (schema.properties) {
        Object.keys(schema.properties).forEach(function (prop) {
          var child = schemaRow(prop, deref(spec, schema.properties[prop]), state, level + 1,
            stack.concat([schema]), requiredList.indexOf(prop) >= 0);
          children.appendChild(child);
          hasChildren = true;
        });
      }
      if (schema.items) {
        children.appendChild(schemaRow('items', schema.items, state, level + 1, stack.concat([schema])));
        hasChildren = true;
      }
      ['allOf', 'oneOf', 'anyOf'].forEach(function (kw) {
        if (Array.isArray(schema[kw])) {
          schema[kw].forEach(function (sub, i) {
            children.appendChild(schemaRow(kw + '[' + i + ']', sub, state, level + 1, stack.concat([schema])));
          });
          hasChildren = true;
        }
      });
    }

    if (hasChildren) {
      frag = el('details', 'ap-row');
      var sum = el('summary');
      sum.appendChild(summary);
      frag.appendChild(sum);
      frag.appendChild(children);
      if (state.options.expandSchemas && level < 1) { frag.open = true; }
    } else {
      frag = el('div', 'ap-row');
      frag.appendChild(summary);
    }
    return frag;
  }

  /* ----------------------------------------------------- markdown lite */

  function mdInline(text) {
    // Minimal inline markdown: `code`, **bold**, *italic*, [text](url)
    var out = esc(text);
    out = out.replace(/`([^`]+)`/g, '<code>$1</code>');
    out = out.replace(/\*\*([^*]+)\*\*/g, '<strong>$1</strong>');
    out = out.replace(/(^|[^*])\*([^*\n]+)\*/g, '$1<em>$2</em>');
    out = out.replace(/\[([^\]]+)\]\(([^)\s]+)\)/g, function (m, t, u) {
      return '<a href="' + esc(u) + '" target="_blank" rel="noopener">' + t + '</a>';
    });
    return out;
  }

  function mdToHtml(text) {
    if (!text) { return ''; }
    var lines = String(text).replace(/\r\n/g, '\n').split('\n');
    var out = [];
    var inCode = false, inList = null, codeBuf = [];
    function closeList() { if (inList) { out.push('</' + inList + '>'); inList = null; } }
    lines.forEach(function (line) {
      var fence = line.match(/^```\s*(\w*)\s*$/);
      if (fence) {
        if (inCode) {
          out.push('<pre><code>' + esc(codeBuf.join('\n')) + '</code></pre>');
          codeBuf = [];
          inCode = false;
        } else {
          closeList();
          inCode = true;
        }
        return;
      }
      if (inCode) { codeBuf.push(line); return; }
      var h = line.match(/^(#{1,4})\s+(.*)$/);
      if (h) {
        closeList();
        var lvl = h[1].length + 2; // h3..h6 in markdown -> inside our h1/h2 layout
        out.push('<h' + lvl + '>' + mdInline(h[2]) + '</h' + lvl + '>');
        return;
      }
      var li = line.match(/^\s*[-*+]\s+(.*)$/);
      var oli = line.match(/^\s*\d+[.)]\s+(.*)$/);
      if (li || oli) {
        var want = li ? 'ul' : 'ol';
        if (inList !== want) { closeList(); inList = want; out.push('<' + want + '>'); }
        out.push('<li>' + mdInline((li || oli)[1]) + '</li>');
        return;
      }
      closeList();
      var bq = line.match(/^>\s?(.*)$/);
      if (bq) { out.push('<blockquote>' + mdInline(bq[1]) + '</blockquote>'); return; }
      if (line.trim() === '') { return; }
      out.push('<p>' + mdInline(line) + '</p>');
    });
    if (inCode) { out.push('<pre><code>' + esc(codeBuf.join('\n')) + '</code></pre>'); }
    closeList();
    return out.join('\n');
  }

  /* ---------------------------------------------------------- theme --- */

  function applyTheme(state) {
    var root = state.root;
    var theme = state.options.theme || 'auto';
    var useBs = state.options.bootstrapTheme !== false;

    if (useBs && theme === 'auto') {
      root.setAttribute('data-mnw-bs', '1');
      return null; // Bootstrap CSS variables follow data-bs-theme automatically
    }
    root.removeAttribute('data-mnw-bs');

    function dark() {
      if (theme === 'dark') { return true; }
      if (theme === 'light') { return false; }
      var host = root.closest('[data-bs-theme]');
      var t = (host && host.getAttribute('data-bs-theme')) ||
        document.documentElement.getAttribute('data-bs-theme');
      if (t === 'dark') { return true; }
      if (t === 'light') { return false; }
      return !!(window.matchMedia && matchMedia('(prefers-color-scheme: dark)').matches);
    }
    function sync() { root.setAttribute('data-mnw-theme', dark() ? 'dark' : 'light'); }
    sync();
    if (theme !== 'auto') { return null; }
    var cleanupFns = [];
    if (window.MutationObserver) {
      var mo = new MutationObserver(sync);
      mo.observe(document.documentElement, { attributes: true, attributeFilter: ['data-bs-theme', 'class'] });
      cleanupFns.push(function () { mo.disconnect(); });
    }
    if (window.matchMedia) {
      var mq = matchMedia('(prefers-color-scheme: dark)');
      if (mq.addEventListener) {
        mq.addEventListener('change', sync);
        cleanupFns.push(function () { mq.removeEventListener('change', sync); });
      }
    }
    return function () { cleanupFns.forEach(function (f) { f(); }); };
  }

  /* ---------------------------------------------------------- try it --- */

  function tryItSection(state, group, opInfo) {
    var spec = state.spec;
    var op = opInfo.op;
    var labels = state.labels;
    var wrap = el('details', 'ap-try');
    var sum = el('summary');
    sum.appendChild(el('span', 'ap-try-title', esc(labels.tryit)));
    wrap.appendChild(sum);
    var body = el('div', 'ap-try-body');
    wrap.appendChild(body);

    // Server selector
    var serverSel = null;
    var servers = state.servers;
    if (servers.length > 1 || (servers.length === 1 && /^https?:/.test(servers[0].url))) {
      var fServer = el('div', 'ap-field');
      fServer.appendChild(el('label', null, esc(labels.servers)));
      serverSel = document.createElement('select');
      servers.forEach(function (s, i) {
        var opt = document.createElement('option');
        opt.value = s.url;
        opt.textContent = (s.description ? s.description + ' - ' : '') + s.url;
        if (i === 0) { opt.selected = true; }
        serverSel.appendChild(opt);
      });
      fServer.appendChild(serverSel);
      body.appendChild(fServer);
    }

    var inputs = [];
    function addField(param) {
      var schema = deref(spec, param.schema) || {};
      var f = el('div', 'ap-field');
      var lab = el('label');
      lab.innerHTML = esc(param.name) +
        (param.required ? ' <span class="ap-req-star">*</span>' : '') +
        ' <span class="ap-f-in">(' + esc(param.in) + ')</span>';
      f.appendChild(lab);
      var input = document.createElement('input');
      input.type = schema.type === 'integer' || schema.type === 'number' ? 'number' : 'text';
      input.placeholder = (schema.enum ? schema.enum.join(', ') : schemaTypeName(schema)) +
        (param.description ? ' - ' + param.description : '');
      if (param.required) { input.dataset.apRequired = '1'; }
      f.appendChild(input);
      body.appendChild(f);
      inputs.push({ param: param, input: input });
    }
    opInfo.params.forEach(addField);

    // Body editor
    var bodyTextarea = null;
    var contentTypeSel = null;
    if (opInfo.body && opInfo.body.content) {
      var types = Object.keys(opInfo.body.content);
      var fBody = el('div', 'ap-field');
      var labBody = el('label');
      labBody.innerHTML = esc(labels.requestBody) + (opInfo.body.required ? ' <span class="ap-req-star">*</span>' : '');
      fBody.appendChild(labBody);
      if (types.length > 1) {
        contentTypeSel = document.createElement('select');
        types.forEach(function (t, i) {
          var opt = document.createElement('option');
          opt.value = t; opt.textContent = t; opt.selected = i === 0;
          contentTypeSel.appendChild(opt);
        });
        fBody.appendChild(contentTypeSel);
      }
      bodyTextarea = document.createElement('textarea');
      if (state.options.fillBody !== false) {
        var firstType = types[0];
        var media = opInfo.body.content[firstType] || {};
        var example = media.example !== undefined ? media.example :
          (media.schema ? buildExample(media.schema, spec, 0) : undefined);
        if (example !== undefined) { bodyTextarea.value = formatJSON(example); }
      }
      fBody.appendChild(bodyTextarea);
      body.appendChild(fBody);
    }

    // Actions
    var actions = el('div', 'ap-try-actions');
    var sendBtn = el('button', 'ap-btn', esc(labels.send));
    var clearBtn = el('button', 'ap-btn ap-btn-plain', esc(labels.clear));
    actions.appendChild(sendBtn);
    actions.appendChild(clearBtn);
    if (state.options.showCurl) {
      var curlBtn = el('button', 'ap-btn ap-btn-plain', 'cURL');
      actions.appendChild(curlBtn);
    }
    sum.appendChild(actions);
    //The action buttons live in the header row: their clicks must not
    //toggle (close) the panel, and pressing one opens it.
    actions.addEventListener('click', function (e) {
      e.preventDefault();
      wrap.open = true;
    });
    var resultBox = el('div', 'ap-try-result');
    body.appendChild(resultBox);

    function currentContentType() {
      return contentTypeSel ? contentTypeSel.value : Object.keys(opInfo.body ? opInfo.body.content : {})[0] || 'application/json';
    }

    function buildRequest() {
      var base = serverSel ? serverSel.value : (servers[0] ? servers[0].url : '');
      if (state.options.serverUrl) { base = state.options.serverUrl; }
      var path = opInfo.path;
      var query = [];
      var headers = {};
      inputs.forEach(function (it) {
        var v = it.input.value;
        if (v === '') { return; }
        if (it.param.in === 'path') {
          path = path.split('{' + it.param.name + '}').join(encodeURIComponent(v));
        } else if (it.param.in === 'query') {
          query.push(encodeURIComponent(it.param.name) + '=' + encodeURIComponent(v));
        } else if (it.param.in === 'header') {
          headers[it.param.name] = v;
        }
      });
      // Always build a full URL: a missing or relative server in the spec
      // means the documentation page's own origin, so the cURL example
      // shows a complete URL. Same-origin fetch() behaves identically.
      if (!/^[a-z][a-z0-9+.-]*:\/\//i.test(base)) {
        base = window.location.origin + (base === '/' ? '' : base);
      }
      var url = base.replace(/\/+$/, '') + path;
      if (query.length) { url += (url.indexOf('?') >= 0 ? '&' : '?') + query.join('&'); }
      var hasBody = bodyTextarea && bodyTextarea.value !== '';
      if (hasBody) { headers['Content-Type'] = currentContentType(); }
      return { url: url, method: opInfo.method.toUpperCase(), headers: headers, body: hasBody ? bodyTextarea.value : null };
    }

    function curlCommand(req) {
      var parts = ['curl -X ' + req.method, "'" + req.url.replace(/'/g, "'\\''") + "'"];
      Object.keys(req.headers).forEach(function (h) {
        parts.push("-H '" + h + ': ' + req.headers[h] + "'");
      });
      if (req.body) { parts.push("--data-raw '" + req.body.replace(/'/g, "'\\''") + "'"); }
      return parts.join(' \\\n  ');
    }

    function showText(text, cls) {
      resultBox.innerHTML = '';
      resultBox.appendChild(el('div', cls || 'ap-error', esc(text)));
    }

    function showResponse(status, statusText, elapsed, respHeaders, respBody, req) {
      resultBox.innerHTML = '';
      var box = el('div', 'ap-resp');
      box.open = true;
      var s = el('summary');
      var statusClass = 'ap-status-' + (Math.floor(status / 100) || 'x');
      s.appendChild(el('span', 'ap-status ' + statusClass, esc(String(status))));
      s.appendChild(el('span', 'ap-prop-meta', esc(statusText + ' \u00b7 ' + elapsed + ' ms')));
      box.appendChild(s);
      var rbody = el('div', 'ap-resp-body');
      box.appendChild(rbody);
      rbody.appendChild(el('div', 'ap-section-title', esc(labels.headers)));
      var table = el('table', 'ap-table');
      table.innerHTML = '<tr><th>' + esc(labels.name) + '</th><th>' + esc(labels.body) + '</th></tr>';
      Object.keys(respHeaders).forEach(function (h) {
        var tr = document.createElement('tr');
        tr.innerHTML = '<td class="ap-cell-name">' + esc(h) + '</td><td>' + esc(respHeaders[h]) + '</td>';
        table.appendChild(tr);
      });
      rbody.appendChild(table);
      rbody.appendChild(el('div', 'ap-section-title', esc(labels.body)));
      if (respBody) {
        var pretty = respBody;
        try { pretty = formatJSON(JSON.parse(respBody)); } catch (e) { /* keep raw */ }
        rbody.appendChild(el('pre', null, '<code>' + esc(pretty) + '</code>'));
      } else {
        rbody.appendChild(el('div', 'ap-prop-meta', esc(labels.nocontent)));
      }
      if (state.options.showCurl) {
        rbody.appendChild(el('div', 'ap-section-title', 'cURL'));
        rbody.appendChild(el('pre', null, '<code>' + esc(curlCommand(req)) + '</code>'));
      }
      resultBox.appendChild(box);
    }

    sendBtn.addEventListener('click', function () {
      var req = buildRequest();
      var missing = [];
      inputs.forEach(function (it) {
        if (it.input.dataset.apRequired && it.input.value === '') { missing.push(it.param.name); }
      });
      if (missing.length) {
        showText(labels.required + ': ' + missing.join(', '));
        return;
      }
      var url = req.url;
      if (state.options.proxyUrl) {
        if (state.options.proxyUrl.indexOf('{url}') >= 0) {
          url = state.options.proxyUrl.replace('{url}', encodeURIComponent(url));
        } else {
          url = state.options.proxyUrl.replace(/\/+$/, '') + '/' + url;
        }
      }
      sendBtn.disabled = true;
      sendBtn.textContent = '\u2026';
      var t0 = (window.performance && performance.now) ? performance.now() : Date.now();
      fetch(url, {
        method: req.method,
        headers: req.headers,
        body: req.body,
        credentials: state.options.credentials || 'same-origin'
      }).then(function (resp) {
        return resp.text().then(function (text) {
          var h = {};
          resp.headers.forEach(function (v, k) { h[k] = v; });
          var t1 = (window.performance && performance.now) ? performance.now() : Date.now();
          showResponse(resp.status, resp.statusText || '', Math.round(t1 - t0), h, text, req);
        });
      }).catch(function (err) {
        showText(labels.loaderror + ': ' + err);
      }).then(function () {
        sendBtn.disabled = false;
        sendBtn.textContent = esc(labels.send);
      });
    });

    clearBtn.addEventListener('click', function () {
      inputs.forEach(function (it) { it.input.value = ''; });
      if (bodyTextarea && state.options.fillBody !== false) {
        var firstType = Object.keys(opInfo.body.content)[0];
        var media = opInfo.body.content[firstType] || {};
        var example = media.schema ? buildExample(media.schema, spec, 0) : undefined;
        bodyTextarea.value = example === undefined ? '' : formatJSON(example);
      } else if (bodyTextarea) {
        bodyTextarea.value = '';
      }
      resultBox.innerHTML = '';
    });

    if (state.options.showCurl) {
      curlBtn.addEventListener('click', function () {
        var req = buildRequest();
        resultBox.innerHTML = '';
        resultBox.appendChild(el('pre', null, '<code>' + esc(curlCommand(req)) + '</code>'));
      });
    }

    return wrap;
  }

  /* ------------------------------------------------------------ render */

  function statusClass(code) {
    var c = String(code).charAt(0);
    return 'ap-status-' + ('2345'.indexOf(c) >= 0 ? c : 'x');
  }

  function mediaSchemas(content) {
    var out = [];
    if (!content) { return out; }
    Object.keys(content).forEach(function (ct) {
      out.push({ type: ct, media: content[ct] || {} });
    });
    return out;
  }

  function renderOperation(state, group, opInfo) {
    var spec = state.spec;
    var labels = state.labels;
    var op = opInfo.op;
    var card = el('details', 'ap-op');
    card.id = opInfo.id;

    var sum = el('summary');
    sum.appendChild(el('span', 'ap-method ap-method-' + opInfo.method, esc(opInfo.method)));
    sum.appendChild(el('span', 'ap-op-path', esc(opInfo.path)));
    //Short parameters hint: query/header/cookie params (path params are
    //already visible in the path itself)
    var hintParts = [];
    var hintText = [];
    opInfo.params.forEach(function (p) {
      if (p.in === 'query') { hintParts.push('?' + p.name); }
      else if (p.in === 'header' || p.in === 'cookie') { hintParts.push(p.name); }
      else { return; }
      hintText.push(p.name + ' (' + p.in + (p.required ? ', required)' : ')'));
    });
    if (opInfo.body && opInfo.body.content) {
      hintParts.push('{body}');
      hintText.push('request body (' + Object.keys(opInfo.body.content).join(', ') + ')');
    }
    if (hintParts.length) {
      var hints = el('span', 'ap-op-params', esc(hintParts.join(' ')));
      hints.title = hintText.join('\n');
      sum.appendChild(hints);
    }
    if (op.deprecated) { sum.appendChild(el('span', 'ap-badge-deprecated', esc(labels.deprecated))); }
    if (op.summary) { sum.appendChild(el('span', 'ap-op-summary', esc(op.summary))); }
    card.appendChild(sum);

    var body = el('div', 'ap-op-body');
    card.appendChild(body);

    if (op.operationId) { body.appendChild(el('div', 'ap-prop-meta', 'operationId: ' + esc(op.operationId))); }
    if (op.description && op.description !== op.summary) {
      body.appendChild(el('div', 'ap-desc ap-md', mdToHtml(op.description)));
    } else if (op.summary) {
      body.appendChild(el('div', 'ap-desc', esc(op.summary)));
    }

    if (opInfo.params.length) {
      body.appendChild(el('div', 'ap-section-title', esc(labels.parameters)));
      var table = el('table', 'ap-table');
      table.innerHTML = '<tr><th>' + esc(labels.name) + '</th><th>' + esc(labels.in) + '</th><th>' +
        esc(labels.type) + '</th><th></th><th>' + esc(labels.description) + '</th></tr>';
      opInfo.params.forEach(function (p) {
        var schema = deref(spec, p.schema) || {};
        var tr = document.createElement('tr');
        tr.innerHTML =
          '<td class="ap-cell-name">' + esc(p.name) + '</td>' +
          '<td>' + esc(p.in) + '</td>' +
          '<td class="ap-type">' + esc(schemaTypeName(schema)) + '</td>' +
          '<td>' + (p.required ? '<span class="ap-req-star" title="' + esc(labels.required) + '">*</span>' : '') + '</td>' +
          '<td>' + (p.description ? mdInline(p.description) : '') + '</td>';
        table.appendChild(tr);
      });
      body.appendChild(table);
    }

    if (opInfo.body && opInfo.body.content) {
      body.appendChild(el('div', 'ap-section-title', esc(labels.requestBody) +
        (opInfo.body.required ? ' *' : '')));
      if (opInfo.body.description) {
        body.appendChild(el('div', 'ap-desc', mdInline(opInfo.body.description)));
      }
      mediaSchemas(opInfo.body.content).forEach(function (m) {
        body.appendChild(el('div', 'ap-prop-meta', esc(m.type)));
        if (m.media.schema) {
          body.appendChild(schemaTree(m.media.schema, state));
        }
        if (m.media.example !== undefined) {
          body.appendChild(el('div', 'ap-section-title', esc(labels.example)));
          body.appendChild(el('pre', null, '<code>' + esc(formatJSON(m.media.example)) + '</code>'));
        }
      });
    }

    var codes = Object.keys(opInfo.responses);
    if (codes.length) {
      body.appendChild(el('div', 'ap-section-title', esc(labels.responses)));
      codes.sort().forEach(function (code) {
        var r = opInfo.responses[code];
        var box = el('details', 'ap-resp');
        var s = el('summary');
        s.appendChild(el('span', 'ap-status ' + statusClass(code), esc(code)));
        s.appendChild(el('span', 'ap-prop-meta', esc(r.description || '')));
        box.appendChild(s);
        var rbody = el('div', 'ap-resp-body');
        box.appendChild(rbody);
        mediaSchemas(r.content).forEach(function (m) {
          rbody.appendChild(el('div', 'ap-prop-meta', esc(m.type)));
          if (m.media.schema) { rbody.appendChild(schemaTree(m.media.schema, state)); }
          if (m.media.example !== undefined) {
            rbody.appendChild(el('div', 'ap-section-title', esc(labels.example)));
            rbody.appendChild(el('pre', null, '<code>' + esc(formatJSON(m.media.example)) + '</code>'));
          }
        });
        if (!r.content) { rbody.appendChild(el('div', 'ap-prop-meta', esc(labels.nocontent))); }
        if (r.headers) {
          rbody.appendChild(el('div', 'ap-section-title', esc(labels.headers)));
          var ht = el('table', 'ap-table');
          ht.innerHTML = '<tr><th>' + esc(labels.name) + '</th><th>' + esc(labels.type) + '</th><th>' + esc(labels.description) + '</th></tr>';
          Object.keys(r.headers).forEach(function (hn) {
            var hd = deref(spec, r.headers[hn]) || {};
            var tr = document.createElement('tr');
            tr.innerHTML = '<td class="ap-cell-name">' + esc(hn) + '</td><td class="ap-type">' +
              esc(schemaTypeName(deref(spec, hd.schema) || {})) + '</td><td>' + esc(hd.description || '') + '</td>';
            ht.appendChild(tr);
          });
          rbody.appendChild(ht);
        }
        box.open = codes.length === 1;
        body.appendChild(box);
      });
    }

    if (state.options.tryIt !== false) {
      body.appendChild(tryItSection(state, group, opInfo));
    }

    if (state.options.expandOperations) { card.open = true; }
    state.sections.push({ id: opInfo.id, group: group.name });
    return card;
  }

  function renderModels(state) {
    var labels = state.labels;
    var schemas = getSchemas(state.spec);
    var names = Object.keys(schemas).sort();
    if (!names.length) { return null; }

    var section = el('details', 'ap-section');
    section.id = 'models';
    section.open = !!state.options.expandSections;
    var sum = el('summary');
    sum.appendChild(el('h2', null, esc(labels.models)));
    section.appendChild(sum);
    var sbody = el('div', 'ap-section-body');
    section.appendChild(sbody);
    names.forEach(function (name) {
      var card = el('details', 'ap-op');
      card.id = 'model-' + sanitizeId(name);
      var sum = el('summary');
      sum.appendChild(el('span', 'ap-op-path', esc(name)));
      var schema = schemas[name];
      if (schema && schema.description) {
        sum.appendChild(el('span', 'ap-op-summary', esc(schema.description)));
      }
      card.appendChild(sum);
      var body = el('div', 'ap-op-body');
      card.appendChild(body);
      var full = deref(state.spec, schema);
      if (full && full.description) { body.appendChild(el('div', 'ap-desc', mdInline(full.description))); }
      body.appendChild(schemaTree(full || {}, state));
      body.appendChild(el('div', 'ap-section-title', esc(labels.example)));
      var modelsBase = state.spec.components && state.spec.components.schemas
        ? '#/components/schemas/' : '#/definitions/';
      var example = buildExample(schemas[name], state.spec, 0, [modelsBase + name]);
      body.appendChild(el('pre', null, '<code>' + esc(formatJSON(example === undefined ? {} : example)) + '</code>'));
      if (state.options.expandSchemas) { card.open = true; }
      sbody.appendChild(card);
      state.sections.push({ id: card.id, group: labels.models });
    });
    return section;
  }

  function renderSidebar(state) {
    var labels = state.labels;
    var nav = el('nav', 'ap-nav');

    var head = el('div', 'ap-nav-head');
    var title = el('div', 'ap-nav-title');
    title.textContent = (state.spec.info && state.spec.info.title) || 'API';
    head.appendChild(title);
    if (state.spec.info && state.spec.info.version) {
      head.appendChild(el('span', 'ap-nav-version', esc(state.spec.info.version)));
    }
    nav.appendChild(head);

    var listBox = el('div', 'ap-nav-list');
    nav.appendChild(listBox);

    // Groups of operations
    state.groups.forEach(function (group) {
      var g = el('details', 'ap-nav-group');
      g.open = !!state.options.expandSections;
      var s = el('summary', null, esc(group.name));
      g.appendChild(s);
      group.operations.forEach(function (opInfo) {
        var item = el('div', 'ap-nav-op' + (opInfo.op.deprecated ? ' ap-deprecated' : ''));
        item.dataset.apTarget = opInfo.id;
        item.dataset.apSearch = (opInfo.path + ' ' + (opInfo.op.summary || '') + ' ' +
          (opInfo.op.operationId || '') + ' ' + group.name).toLowerCase();
        item.appendChild(el('span', 'ap-method ap-method-' + opInfo.method, esc(opInfo.method)));
        var text = el('span', 'ap-nav-text');
        text.textContent = opInfo.op.summary || opInfo.path;
        item.appendChild(text);
        g.appendChild(item);
      });
      listBox.appendChild(g);
    });

    // Models link
    if (state.modelsSection) {
      var mg = el('details', 'ap-nav-group');
      mg.open = !!state.options.expandSections;
      mg.appendChild(el('summary', null, esc(labels.models)));
      state.modelsSection.querySelectorAll('.ap-op').forEach(function (card) {
        var item = el('div', 'ap-nav-op');
        item.dataset.apTarget = card.id;
        var opPath = card.querySelector('.ap-op-path');
        var modelText = opPath ? opPath.textContent : card.id;
        item.dataset.apSearch = modelText.toLowerCase();
        var text = el('span', 'ap-nav-text');
        text.textContent = modelText;
        item.appendChild(text);
        mg.appendChild(item);
      });
      listBox.appendChild(mg);
    }

    // Search box
    if (state.options.search !== false) {
      var search = el('div', 'ap-nav-search');
      var input = document.createElement('input');
      input.type = 'search';
      input.placeholder = labels.search;
      search.appendChild(input);
      nav.insertBefore(search, listBox);
      input.addEventListener('input', function () {
        var q = input.value.trim().toLowerCase();
        var anyVisible = false;
        listBox.querySelectorAll('.ap-nav-group').forEach(function (grp) {
          var visibleInGroup = 0;
          grp.querySelectorAll('.ap-nav-op').forEach(function (item) {
            var match = !q || (item.dataset.apSearch || '').indexOf(q) >= 0 ||
              grp.querySelector('summary').textContent.toLowerCase().indexOf(q) >= 0;
            item.style.display = match ? '' : 'none';
            if (match) { visibleInGroup++; }
          });
          grp.style.display = visibleInGroup ? '' : 'none';
          if (visibleInGroup) { grp.open = true; anyVisible = true; }
        });
        var emptyMsg = listBox.querySelector('.ap-nav-empty');
        if (!anyVisible) {
          if (!emptyMsg) {
            emptyMsg = el('div', 'ap-nav-empty', esc(labels.noresult));
            listBox.appendChild(emptyMsg);
          }
        } else if (emptyMsg) {
          emptyMsg.remove();
        }
      });
    }

    return nav;
  }

  function buildMain(state) {
    var labels = state.labels;
    var spec = state.spec;
    var main = el('main', 'ap-main');

    // Overview
    var overview = el('section');
    overview.id = 'overview';
    if (state.options.headingText) {
      main.appendChild(el('div', 'ap-heading-text', esc(state.options.headingText)));
    }
    var info = spec.info || {};
    var h1 = el('h1');
    h1.textContent = info.title || 'API';
    overview.appendChild(h1);
    if (info.version) { overview.appendChild(el('span', 'ap-version-badge', 'v ' + esc(info.version))); }
    var meta = el('div', 'ap-doc-meta');
    if (spec.openapi) { meta.textContent = 'OpenAPI ' + esc(spec.openapi); }
    else if (spec.swagger) { meta.textContent = 'Swagger ' + esc(spec.swagger); }
    overview.appendChild(meta);
    if (info.contact && (info.contact.url || info.contact.email || info.contact.name)) {
      var c = info.contact;
      var link = c.url ? c.url : (c.email ? 'mailto:' + c.email : '');
      overview.appendChild(el('div', 'ap-doc-meta',
        (link ? '<a href="' + esc(link) + '">' + esc(c.name || link) + '</a>' : esc(c.name || ''))));
    }
    if (info.license && (info.license.name || info.license.url)) {
      overview.appendChild(el('div', 'ap-doc-meta',
        (info.license.url ? '<a href="' + esc(info.license.url) + '">' : '') +
        esc(info.license.name || info.license.url) + (info.license.url ? '</a>' : '')));
    }
    if (info.termsOfService) {
      overview.appendChild(el('div', 'ap-doc-meta',
        '<a href="' + esc(info.termsOfService) + '">' + esc(info.termsOfService) + '</a>'));
    }
    if (info.description) {
      overview.appendChild(el('div', 'ap-md', mdToHtml(info.description)));
    }

    // Servers
    if (state.servers.length) {
      overview.appendChild(el('div', 'ap-section-title', esc(labels.servers)));
      state.servers.forEach(function (s) {
        var line = el('div');
        line.appendChild(el('code', null, esc(s.url)));
        if (s.description) { line.appendChild(el('span', 'ap-prop-meta', ' ' + esc(s.description))); }
        overview.appendChild(line);
      });
    }

    // Authentication
    var secSchemes = getSecuritySchemes(spec);
    var schemeNames = Object.keys(secSchemes);
    if (schemeNames.length) {
      overview.appendChild(el('div', 'ap-section-title', esc(labels.authentication)));
      var st = el('table', 'ap-table');
      st.innerHTML = '<tr><th>' + esc(labels.name) + '</th><th>' + esc(labels.type) + '</th><th>' + esc(labels.description) + '</th></tr>';
      schemeNames.forEach(function (n) {
        var sc = secSchemes[n] || {};
        var tr = document.createElement('tr');
        tr.innerHTML = '<td class="ap-cell-name">' + esc(n) + '</td><td class="ap-type">' +
          esc((sc.type || '') + (sc.in ? ' (' + sc.in + ')' : '') + (sc.scheme ? ' ' + sc.scheme : '')) +
          '</td><td>' + (sc.description ? mdInline(sc.description) : '') + '</td>';
        st.appendChild(tr);
      });
      overview.appendChild(st);
    }

    // Toolbar: download spec
    var toolbar = el('div', 'ap-toolbar');
    if (state.options.download !== false && (state.options.url || state.spec)) {
      var dl = el('a', 'ap-chip', esc(labels.download));
      if (state.options.url) {
        dl.href = state.options.url;
        dl.setAttribute('download', '');
      } else {
        dl.href = URL.createObjectURL(new Blob([formatJSON(spec)], { type: 'application/json' }));
        dl.setAttribute('download', 'openapi.json');
      }
      toolbar.appendChild(dl);
    }
    if (toolbar.childNodes.length) { overview.appendChild(toolbar); }

    main.appendChild(overview);
    state.sections.push({ id: 'overview', group: labels.overview });

    // Groups
    state.groups.forEach(function (group) {
      var section = el('details', 'ap-section');
      section.id = 'tag-' + sanitizeId(group.name);
      section.open = !!state.options.expandSections;
      var sum = el('summary');
      sum.appendChild(el('h2', null, esc(group.name)));
      section.appendChild(sum);
      var sbody = el('div', 'ap-section-body');
      section.appendChild(sbody);
      if (group.description) {
        sbody.appendChild(el('div', 'ap-tag-desc ap-md', mdToHtml(group.description)));
      }
      group.operations.forEach(function (opInfo) {
        sbody.appendChild(renderOperation(state, group, opInfo));
      });
      main.appendChild(section);
    });

    // Models
    if (!state.options.hideModels) {
      var models = renderModels(state);
      if (models) {
        main.appendChild(models);
        state.modelsSection = models;
      }
    }
    return main;
  }

  // Find a section/operation by id and open its parent collapsible
  // sections (tag groups and models), also opens its group in the sidebar.
  // The target itself stays as-is: methods are not auto-opened.
  function revealTarget(state, id) {
    var target = state.main.querySelector('#' + CSS.escape(id));
    if (!target) { return null; }
    var box = target.parentElement ? target.parentElement.closest('details') : null;
    while (box) {
      box.open = true;
      box = box.parentElement ? box.parentElement.closest('details') : null;
    }
    if (state.nav) {
      var item = state.nav.querySelector('.ap-nav-op[data-ap-target="' + id + '"]');
      if (item) {
        var grp = item.closest('details');
        if (grp) { grp.open = true; }
      }
    }
    return target;
  }

  function bindNavigation(state) {
    if (!state.nav) { return; }
    // Click on a nav item: open the target and scroll to it
    state.nav.addEventListener('click', function (ev) {
      var item = ev.target.closest('.ap-nav-op');
      if (!item) { return; }
      ev.preventDefault();
      var target = revealTarget(state, item.dataset.apTarget);
      if (!target) { return; }
      target.scrollIntoView({ behavior: 'smooth', block: 'start' });
    });

    // Reference links inside schema trees
    state.main.addEventListener('click', function (ev) {
      var a = ev.target.closest('a[data-ap-scroll]');
      if (!a) { return; }
      ev.preventDefault();
      var target = revealTarget(state, a.dataset.apScroll);
      if (target) {
        target.scrollIntoView({ behavior: 'smooth', block: 'start' });
      }
    });

    // Scroll spy: mark the active operation in the sidebar
    // (throttled with setTimeout instead of requestAnimationFrame, which
    // never fires while the tab is hidden and would stall the flag)
    var timer = null;
    state.main.addEventListener('scroll', function () {
      if (timer) { return; }
      timer = setTimeout(function () {
        timer = null;
        var top = state.main.getBoundingClientRect().top;
        var current = null;
        var lastVisible = null;
        state.sections.forEach(function (sec) {
          var node = state.main.querySelector('#' + CSS.escape(sec.id));
          if (!node) { return; }
          //skip nodes inside a collapsed section header
          var box = node.closest('details:not([open])');
          if (box && box !== node) { return; }
          var rel = node.getBoundingClientRect().top - top;
          if (rel <= 60) {
            current = sec.id;
          }
          if (rel >= 0 && rel < state.main.clientHeight - 20) {
            lastVisible = sec.id;
          }
        });
        //at the end of the content the band rule finds nothing below,
        //use the last section whose header is visible instead
        var atBottom = state.main.scrollTop + state.main.clientHeight >= state.main.scrollHeight - 4;
        if (atBottom && lastVisible) {
          current = lastVisible;
        }
        state.nav.querySelectorAll('.ap-nav-op.ap-active').forEach(function (n) { n.classList.remove('ap-active'); });
        if (current) {
          var active = state.nav.querySelector('.ap-nav-op[data-ap-target="' + current + '"]');
          if (active) { active.classList.add('ap-active'); }
        }
      }, 60);
    });
  }

  function showError(state, message) {
    if (!state.main) {
      state.main = el('main', 'ap-main');
      state.root.appendChild(state.main);
    }
    state.main.innerHTML = '';
    state.main.appendChild(el('div', 'ap-error', esc(state.labels.loaderror) + '<br>' + esc(message)));
  }

  function renderSpec(state, spec) {
    if (!isPlainObject(spec)) { throw new Error('not an object'); }
    state.spec = spec;
    state.servers = getServers(spec);
    if (state.options.serverUrl) {
      state.servers = [{ url: state.options.serverUrl }].concat(state.servers);
    }
    state.groups = collectGroups(state);
    state.sections = [];

    //Build main first, the sidebar lists models found in it
    state.main = buildMain(state);
    if (state.options.sidebar !== false) {
      state.nav = renderSidebar(state);
      state.root.appendChild(state.nav);
    }
    state.root.appendChild(state.main);
    bindNavigation(state);

    if (location.hash) {
      var target = revealTarget(state, location.hash.substring(1));
      if (target) {
        setTimeout(function () { target.scrollIntoView({ block: 'start' }); }, 0);
      }
    }
  }

  function loadSpec(state, then, onError) {
    var opt = state.options;
    var content = opt.content;
    if (content === undefined && opt.contentElementId) {
      var src = document.getElementById(opt.contentElementId);
      if (src) { content = src.textContent; }
    }
    if (typeof content === 'string') {
      try { content = JSON.parse(content); }
      catch (e) { onError('JSON: ' + e.message); return; }
    }
    if (isPlainObject(content)) { then(content); return; }
    if (opt.url) {
      fetch(opt.url, { credentials: 'same-origin' })
        .then(function (r) {
          if (!r.ok) { throw new Error('HTTP ' + r.status); }
          return r.text();
        })
        .then(function (text) { then(JSON.parse(text)); })
        .catch(function (e) { onError(String(e)); });
      return;
    }
    onError('no url or content in options');
  }

  /**
   * Render an API documentation viewer inside target element.
   * target: element or CSS selector. options: see the file header.
   */
  function render(target, options) {
    var root = typeof target === 'string' ? document.querySelector(target) : target;
    if (!root) { return null; }
    ensureStyle();

    // Cleanup a previous viewer instance on the same element
    if (root.__mnwApiDoc) {
      if (root.__mnwApiDoc.cleanup) {
        try { root.__mnwApiDoc.cleanup(); } catch (e) { /* ignore */ }
      }
    }
    root.innerHTML = '';
    root.classList.add('mnw-apidoc');

    options = options || {};
    var labels = {};
    Object.keys(defaultLabels).forEach(function (k) { labels[k] = defaultLabels[k]; });
    if (isPlainObject(options.labels)) {
      Object.keys(options.labels).forEach(function (k) {
        if (options.labels[k]) { labels[k] = options.labels[k]; }
      });
    }

    var state = {
      root: root,
      options: options,
      labels: labels,
      spec: null,
      servers: [],
      groups: [],
      sections: [],
      nav: null,
      main: null,
      modelsSection: null
    };
    var cleanupTheme = applyTheme(state);

    if (options.customCss) {
      var cs = document.createElement('style');
      cs.textContent = options.customCss;
      root.appendChild(cs);
    }
    if (options.height) {
      root.style.height = options.height;
    }

    loadSpec(state, function (spec) {
      try {
        renderSpec(state, spec);
      } catch (e) {
        showError(state, String(e));
      }
    }, function (message) {
      showError(state, message);
    });

    root.__mnwApiDoc = {
      cleanup: function () {
        if (cleanupTheme) {
          try { cleanupTheme(); } catch (e) { /* ignore */ }
        }
      }
    };
    return state;
  }

  return {
    version: '1.7',
    render: render,
    labels: defaultLabels
  };
})();
