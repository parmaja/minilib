// Tests for the file-extension handler registry in WebElements.
program web_ext_handle;
{$ifdef fpc}
{$mode delphi}
{$endif}
uses
  SysUtils, Classes, mnMIME, mnWebElements, mnwMarkdownIt;

type
  TTestSchema = class(THTML)
  end;

var
  Failed: Integer = 0;

procedure Check(ACondition: Boolean; const AMessage: string);
begin
  if ACondition then
    WriteLn('ok   - ', AMessage)
  else
  begin
    WriteLn('FAIL - ', AMessage);
    Inc(Failed);
  end;
end;

var
  Ext: TmnwFileExtensions;
  Item: TmnwFileExtension;
  Web: TmnwWeb;
  Schema: TMarkdownFileSchema;
  Ctx: TmnwContext;
  Markdown: TMarkdownIt;
  FileName: string;
  Raised: Boolean;
begin
  Ext := TmnwFileExtensions.Create;
  try
    Check(TmnwFileExtensions.Normalize('md') = 'md', 'Normalize: md');
    Check(TmnwFileExtensions.Normalize('.MD') = 'md', 'Normalize: leading dot and case');
    Check(TmnwFileExtensions.Normalize('guide.md') = 'md', 'Normalize: file name');
    Check(TmnwFileExtensions.Normalize('C:\dir\GUIDE.MD') = 'md', 'Normalize: full path');

    Ext.Register('md', TTestSchema);
    Check(Ext.Count = 1, 'Register adds one item');
    Item := Ext.Find('md');
    Check(Item <> nil, 'Find by normalized extension');
    if Item <> nil then
    begin
      Check(Item.SchemaClass = TTestSchema, 'Handler schema stored');
      Check(Item.ContentType = 'text/html', 'Default content type is text/html');
    end;

    Ext.Register('.MD', TTestSchema, 'application/x-test');
    Check(Ext.Count = 1, 'Re-register replaces instead of duplicating');
    Item := Ext.Find('md');
    if Item <> nil then
      Check(Item.ContentType = 'application/x-test', 'Re-register updates content type');

    Ext.Register('markdown', TTestSchema);
    Check(Ext.Count = 2, 'Alias extension added');
    Check(Ext.FindFile('notes.MD') <> nil, 'FindFile: case-insensitive extension');
    Check(Ext.FindFile('notes.markdown') <> nil, 'FindFile: alias extension');
    Check(Ext.FindFile('notes.txt') = nil, 'FindFile: unknown extension returns nil');
    Check(Ext.FindFile('') = nil, 'FindFile: empty name returns nil');

    Raised := False;
    try
      Ext.Register('', TTestSchema);
    except
      on E: Exception do
        Raised := True;
    end;
    Check(Raised, 'Register rejects empty extension');

    Raised := False;
    try
      Ext.Register('md', nil);
    except
      on E: Exception do
        Raised := True;
    end;
    Check(Raised, 'Register rejects nil schema class');

    RegisterMarkdownFiles(Ext);
    Item := Ext.Find('md');
    Check((Item <> nil) and (Item.SchemaClass = TMarkdownFileSchema),
      'RegisterMarkdownFiles registers the markdown Schema');
    Check((Item <> nil) and (Item.ContentType = 'text/html'),
      'Markdown handler responds with text/html');
    Check(Ext.Find('markdown') <> nil, 'RegisterMarkdownFiles registers .markdown alias');

    Check(DocumentToContentType('guide.md') = 'text/markdown',
      'MIME: .md maps to text/markdown for raw serving');
    Check(DocumentToContentType('guide.txt') = 'text/plain',
      'MIME: existing mapping still works');
  finally
    Ext.Free;
  end;

  //* Composition smoke test: the handler Schema must turn the file into a
  //* TMarkdownIt element under Document.Body.Main.
  FileName := IncludeTrailingPathDelimiter(GetTempDir) + 'web_ext_handle_sample.md';
  try
    with TStringList.Create do
    try
      Add('# Sample heading');
      Add('Some **bold** text.');
      SaveToFile(FileName);
    finally
      Free;
    end;

    Web := TmnwWeb.Create(nil);
    try
      Ctx.Web := Web;
      Ctx.HandlerFile := FileName;
      Schema := TMarkdownFileSchema.Create(Web, 'md', 'md');
      try
        Schema.Compose(Ctx);
        Check(Schema.Document.Body.Main.Count > 0, 'Handler Schema composes children');
        if Schema.Document.Body.Main.Count > 0 then
        begin
          Markdown := Schema.Document.Body.Main.Items[0] as TMarkdownIt;
          Check(Pos('# Sample heading', Markdown.Source) > 0,
            'File content becomes markdown Source');
          Check(mioSanitize in Markdown.Options, 'Sanitization enabled by default');
        end;
      finally
        Schema.Free;
      end;
    finally
      Web.Free;
    end;
  finally
    DeleteFile(FileName);
  end;

  if Failed > 0 then
  begin
    WriteLn(Failed, ' test(s) failed');
    Halt(1);
  end;
  WriteLn('All tests passed');
end.
