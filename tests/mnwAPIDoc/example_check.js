'use strict';
// Quick check that the Responses section can build a readable JSON example from
// a real spec, run against the file server OpenAPI document.
// Run: node tests/mnwAPIDoc/example_check.js [path-to-spec]
var fs = require('fs');
var path = require('path');

var src = fs.readFileSync(path.join(__dirname, '..', '..', 'web', 'source', 'mnwAPIDoc.js'), 'utf8');

function grab(name) {
  var start = src.indexOf('function ' + name + '(');
  if (start < 0) { throw new Error('not found: ' + name); }
  var i = src.indexOf('{', start), depth = 0;
  for (; i < src.length; i++) {
    if (src[i] === '{') { depth++; }
    else if (src[i] === '}') { depth--; if (depth === 0) { i++; break; } }
  }
  return src.slice(start, i);
}

var code = ['isPlainObject', 'resolveRef', 'deref', 'buildExample'].map(grab).join('\n');
var api = new Function(code + '\nreturn { buildExample: buildExample };')();

var specPath = process.argv[2] || 'W:\\source\\projects\\cs_utils\\fileserver.json';
var spec = JSON.parse(fs.readFileSync(specPath, 'utf8'));

var ok = true;
Object.keys(spec.paths).forEach(function (p) {
  ['get', 'post', 'put', 'delete', 'patch'].forEach(function (m) {
    var op = spec.paths[p][m];
    if (!op || !op.responses) { return; }
    Object.keys(op.responses).forEach(function (code) {
      var r = op.responses[code];
      if (!r.content) { return; }
      Object.keys(r.content).forEach(function (t) {
        var media = r.content[t];
        if (!media.schema) { return; }
        var ex = api.buildExample(media.schema, spec, 0, []);
        var shown = ex !== undefined;
        if (!shown) { ok = false; }
        console.log((shown ? 'OK  ' : 'NONE') + ' ' + m.toUpperCase() + ' ' + p +
          ' [' + code + ' ' + t + ']');
        if (t.indexOf('json') >= 0) { console.log(JSON.stringify(ex, null, 2)); }
      });
    });
  });
});
console.log(ok ? 'ALL BODIES HAVE AN EXAMPLE' : 'SOME BODIES HAVE NO EXAMPLE');
process.exit(ok ? 0 : 1);
