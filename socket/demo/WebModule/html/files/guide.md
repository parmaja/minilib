# Markdown file handler demo

Open this file as `/files/guide.md` and the server renders it as HTML instead of
sending the raw text. The file itself still lives in the public folder.

## What is rendered

- **Bold**, *italic* and `inline code`
- task lists are enabled
- fenced code blocks use highlight.js

> The original file is unchanged. Only the HTTP response is transformed.

```pascal
Web.RegisterSchema('files', TFilesSchema);
RegisterMarkdownFiles(Web); // .md and .markdown -> text/html
```
