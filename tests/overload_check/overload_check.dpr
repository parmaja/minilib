// Checks which TLibrarySources.Add overload a set constant binds to as 4th arg,
// e.g. Sources.Add(stScript, url, 'file.js', [libDefer]).
program overload_check;
{$ifdef fpc}
{$mode delphi}
{$endif}
uses
  SysUtils, mnUtils, mnClasses, mnWebElements;
var
  Libs: TmnwLibraries;
  Src: TmnwLibrarySource;
  i: Integer;
begin
  Libs := Libraries;
  for i := 0 to Libs.Count - 1 do
    if Libs.Items[i] is THighlightJS_Library then
    begin
      for Src in Libs.Items[i].Sources do
        WriteLn(Src.Name,
          ' defer=', libDefer in Src.Options,
          ' cross=', libCross in Src.Options,
          ' dir=', Ord(Src.Direction));
    end;
end.
