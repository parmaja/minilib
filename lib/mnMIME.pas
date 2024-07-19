unit mnMIME;
{$M+}{$H+}
{$ifdef FPC}{$mode delphi}{$endif}
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 * @author    Belal Hamed <belal, belalhamed@gmail.com>
 *
}
interface

uses
  Classes, SysUtils,
  mnUtils, mnClasses;

type
  TMIMEFeatures =
  (
    Text,
    Executable
  );

  TmnMIMEItem = Class(TmnNamedObject)
  public
    Features: TMIMEFeatures;
    ContentType: string;
    Description: string
  end;

  { TmnMIME }

  TmnMIME = class(TmnNamedObjectList<TmnMIMEItem>)
  private
    procedure Register;
  protected
    procedure Created; override;
  public
    function Add(Extension, ContentType: string; Description: string = ''): TmnMIMEItem;
  end;

//* You can call txt or .txt filename.txt or c:\temp\filename.txt
function DocumentToContentType(const Extension: string): string;

function MIME: TmnMIME;

implementation

var
  FMIME: TmnMIME = nil;

function MIME: TmnMIME;
begin
  if FMIME = nil then
  begin
    FMIME := TmnMIME.Create;
    FMIME.Register;
  end;
  Result := FMIME;
end;

function DocumentToContentType(const Extension: string): string;
var
  Ext: string;
  item: TmnMIMEItem;
begin
  if Pos('.', Extension) = 0 then
    Ext := Extension
  else
  begin
    Ext := LowerCase(ExtractFileExt(Extension));
    if Length(Ext) > 1 then
      Ext := Copy(Ext, 2, Length(Ext));
  end;
  item := MIME.Find(Ext);
  if item = nil then
    Result := ''
  else
    Result := item.ContentType;
end;

{ TmnMIME }

function TmnMIME.Add(Extension, ContentType: string; Description: string): TmnMIMEItem;
begin
  Result := TmnMIMEItem.Create;
  Result.Name := Extension;
  Result.ContentType := ContentType;
  Result.Description := Description;
  inherited Add(Result);
end;

procedure TmnMIME.Created;
begin
  inherited;
end;

procedure TmnMIME.Register;
begin
  Add('', 'application/binary');
  Add('aac', 'audio/aac', 'AAC audio file');
  Add('apng', 'image/apng', 'Animated Portable Network Graphics (APNG) image');
  Add('arc', 'application/octet-stream', 'Archive document (multiple files embedded)');
  Add('avi', 'video/x-msvideo', 'AVI: Audio Video Interleave');
  Add('azw', 'application/vnd.amazon.ebook', 'Amazon Kindle eBook format');
  Add('bin', 'application/octet-stream', 'Any kind of binary data');
  Add('bmp', 'image/bmp', 'Windows OS/2 Bitmap Graphics');
  Add('bz', 'application/x-bzip', 'BZip archive');
  Add('bz2', 'application/x-bzip2', 'BZip2 archive');
  Add('css', 'text/css', 'Cascading Style Sheets (CSS)');
  Add('csv', 'text/csv', 'Comma-separated values (CSV)');
  Add('doc', 'application/msword', 'Microsoft Word');
  Add('docx', 'application/vnd.openxmlformats-officedocument.wordprocessingml.document', 'Microsoft Word (OpenXML)');
  Add('epub', 'application/epub+zip', 'Electronic publication (EPUB)');
  Add('eot', 'application/vnd.ms-fontobject', 'MS Embedded OpenType fonts');
  Add('gif', 'image/gif', 'Graphics Interchange Format (GIF)');
  Add('gz', 'application/gzip', 'GZip Compressed Archive');
  Add('htm',  'text/html', 'HyperText Markup Language (HTML)');
  Add('html', 'text/html', 'HyperText Markup Language (HTML)');
  Add('ico', 'image/x-icon', 'Icon format');
  Add('ics', 'text/calendar', 'iCalendar format');
  Add('jar', 'application/java-archive', 'Java Archive (JAR)');
  Add('jpeg', 'image/jpeg', 'JPEG images');
  Add('jpg', 'image/jpeg', 'JPEG images');
  Add('js', 'text/javascript', 'JavaScript (ECMAScript)');
  Add('json', 'application/json', 'JSON format');
  Add('jsonld', 'application/ld+json', 'JSON-LD format');
  Add('mid', 'audio/midi', 'Musical Instrument Digital Interface (MIDI)');
  Add('midi', 'audio/midi', 'Musical Instrument Digital Interface (MIDI)');
  Add('mp3', 'audio/mpeg', 'MP3 audio');
  Add('mp4', 'video/mp4', 'MP4 video');
  Add('mpeg', 'video/mpeg', 'MPEG Video');
  Add('mpkg', 'application/vnd.apple.installer+xml', 'Apple Installer Package');
  Add('odp', 'application/vnd.oasis.opendocument.presentation', 'OpenDocuemnt presentation document');
  Add('ods', 'application/vnd.oasis.opendocument.spreadsheet', 'OpenDocuemnt spreadsheet document');
  Add('odt', 'application/vnd.oasis.opendocument.text', 'OpenDocument text document');
  Add('oga', 'audio/ogg', 'OGG audio');
  Add('ogv', 'video/ogg', 'OGG video');
  Add('ogx', 'application/ogg', 'OGG');
  Add('otf', 'font/otf','OpenType font');
  Add('png', 'image/png', 'Portable Network Graphics');
  Add('pdf', 'application/pdf', 'Adobe Portable Document Format (PDF)');
  Add('ppt', 'application/vnd.ms-powerpoint', 'Microsoft PowerPoint');
  Add('rar', 'application/x-rar-compressed', 'RAR archive');
  Add('rtf', 'application/rtf', 'Rich Text Format (RTF)');
  Add('sh', 'application/x-sh', 'Bourne shell script');
  Add('svg', 'image/svg+xml', 'Scalable Vector Graphics (SVG)');
  Add('swf', 'application/x-shockwave-flash', 'Small web format (SWF) or Adobe Flash document');
  Add('tar', 'application/x-tar', 'Tape Archive (TAR)');
  Add('tif', 'image/tiff', 'Tagged Image File Format (TIFF)');
  Add('tiff', 'image/tiff', 'Tagged Image File Format (TIFF)');
  Add('ttf', 'font/ttf', 'TrueType Font');
	Add('txt', 'text/plain', 'Text');
  Add('vsd', 'application/vnd.visio', 'Microsft Visio');
  Add('wav', 'audio/x-wav', 'Waveform Audio Format');
  Add('weba', 'audio/webm', 'WEBM audio');
  Add('webm', 'video/webm', 'WEBM video');
  Add('webp', 'image/webp', 'WEBP image');
  Add('woff', 'font/woff', 'Web Open Font Format (WOFF)');
  Add('woff2', 'font/woff2', 'Web Open Font Format (WOFF)');
  Add('xhtml', 'application/xhtml+xml', 'XHTML');
  Add('xls', 'application/vnd.ms-excel', 'Microsoft Excel');
  Add('xlsx', 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', 'Microsoft Excel (OpenXML)');
  Add('xml', 'application/xml', 'XML');
  Add('xul', 'application/vnd.mozilla.xul+xml', 'XUL');
  Add('zip', 'application/zip', 'ZIP archive');
  Add('3gp', 'video/3gpp', '3GPP audio/video container');
  Add('3g2', 'video/3gpp2', '3GPP2 audio/video container');
  Add('7z', 'application/x-7z-compressed', '7-zip archive');
end;

initialization
finalization
  FreeAndNil(FMIME);
end.
