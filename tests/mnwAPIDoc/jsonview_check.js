// Checks of the JSON table view helpers built into mnwAPIDoc.js.
// Run: node tests/mnwAPIDoc/jsonview_check.js
'use strict';

var fs = require('fs');
var path = require('path');

var file = path.join(__dirname, '..', '..', 'web', 'source', 'mnwAPIDoc.js');
var src = fs.readFileSync(file, 'utf8');

// the helpers are plain functions, take them out of the module as they are
// (esc comes first, the table view helpers live in the same block)
var start = src.indexOf('function esc(');
var end = src.indexOf('function sanitizeId(');
if (start < 0 || end < 0) { throw new Error('helpers not found in ' + file); }
var block = src.slice(start, end);
if (block.indexOf('function jsonList(') < 0) {
  throw new Error('the block does not hold every helper');
}
var names = ['isPlainObject', 'parseJSONText', 'jsonKind', 'jsonType', 'jsonCell',
  'jsonFlattenRows', 'jsonList'];
var api = new Function(block + '\nreturn {' + names.map(function (n) {
  return n + ':' + n;
}).join(',') + '};')();

var rows = [];
function out(title, ok) { rows.push([title, !!ok]); }

out('array body is its own list', (function () {
  var l = api.jsonList([{ a: 1 }, { a: 2 }]);
  return l.name === '' && l.meta.length === 0 && l.rows.length === 2;
})());

out('object takes the first property holding objects', (function () {
  var l = api.jsonList({
    state: 'ok', total: 8, count: 8, page: 1, limit: 50,
    notes: [{ id: 'n-1', user: 'admin' }, { id: 'n-2', user: 'bob' }]
  });
  return l.name === 'notes' && l.rows.length === 2 &&
    JSON.stringify(l.meta) === JSON.stringify([['state', 'ok'], ['total', '8'],
      ['count', '8'], ['page', '1'], ['limit', '50']]);
})());

out('an array of scalars is not a list', api.jsonList({ tags: ['a', 'b'], n: 2 }) === null);
out('an empty array is not a list', api.jsonList({ notes: [], n: 2 }) === null);
out('a list inside a nested object is not picked', api.jsonList({ page: { items: [{ id: 1 }] } }) === null);
out('items of mixed shape are not a list', api.jsonList({ rows: [{ id: 1 }, 5] }) === null);

out('the object itself is flattened with dotted names', (function () {
  var rows2 = [];
  api.jsonFlattenRows({ book: { id: 'bk-1', tags: ['a'] }, n: 1 }, '', rows2, 0);
  return JSON.stringify(rows2) === JSON.stringify([
    ['book', 'object', '{2 keys}'],
    ['book.id', 'string', 'bk-1'],
    ['book.tags', 'array of string', 'a'],
    ['n', 'number', '1']
  ]);
})());

out('text is only parsed when it is an object or an array', (function () {
  return api.jsonKind(api.parseJSONText('{"a":1}')) === 'object' &&
    api.jsonKind(api.parseJSONText('[1]')) === 'array' &&
    api.parseJSONText('<html>') === null && api.parseJSONText('') === null &&
    api.parseJSONText('null') === null && api.parseJSONText('{"a":') === null &&
    api.jsonKind(api.parseJSONText('{"a":1}')) === 'object';
})());

out('cells say what is inside them', api.jsonCell([1, 2, 3]) === '1, 2, 3' &&
  api.jsonCell([{ a: 1 }, { b: 2 }]) === '{2 objects}' && api.jsonCell({ a: 1 }) === '{1 keys}' &&
  api.jsonCell([]) === '[]' && api.jsonCell(null) === 'null');

out('an array type says what it holds', api.jsonType(['a']) === 'array of string' &&
  api.jsonType([1, 'a']) === 'array of number / string' && api.jsonType([]) === 'array');

var failed = rows.filter(function (r) { return !r[1]; });
rows.forEach(function (r) { console.log((r[1] ? 'PASS ' : 'FAIL ') + r[0]); });
console.log(failed.length ? failed.length + ' FAILURES' : 'ALL PASS');
process.exitCode = failed.length ? 1 : 0;
