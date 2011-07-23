unit TSVN_SCM;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{
  Link to SCM manager like TSVN and TGIT
}
interface

uses
  SysUtils, Forms, StrUtils, Variants, Classes, Controls, Graphics, Contnrs,
  SynEdit, EditorEngine;

type

  { TTSVN_SCM }

  TTSVN_SCM = class(TEditorSCM)
  private
  protected
    function GetTortoiseProc: string;
    function GetTortoiseMerge: string;
  public
    constructor Create; override;
    procedure CommitDirectory(Directory: string); override;
    procedure CommitFile(FileName: string); override;
    procedure UpdateDirectory(Directory: string); override;
    procedure UpdateFile(FileName: string); override;
    procedure RevertDirectory(Directory: string); override;
    procedure RevertFile(FileName: string); override;
    procedure DiffFile(FileName: string); override;
    procedure DiffToFile(FileName, ToFileName: string); override;
    property TortoiseProc: string read GetTortoiseProc;
    property TortoiseMerge: string read GetTortoiseMerge;
  end;

implementation

{ TTSVN_SCM }

procedure TTSVN_SCM.CommitDirectory(Directory: string);
begin
  ExecuteProcess(TortoiseProc, '/command:commit /path:"' + Directory + '" /notempfile /closeonend');
end;

procedure TTSVN_SCM.CommitFile(FileName: string);
begin
  ExecuteProcess(TortoiseProc, '/command:commit /path:"' + FileName + '" /notempfile /closeonend');
end;

procedure TTSVN_SCM.UpdateDirectory(Directory: string);
begin
  ExecuteProcess(TortoiseProc, '/command:update /path:"' + Directory + '" /notempfile /closeonend');
end;

procedure TTSVN_SCM.UpdateFile(FileName: string);
begin
  ExecuteProcess(TortoiseProc, '/command:update /path:"' + FileName + '" /notempfile /closeonend');
end;

procedure TTSVN_SCM.RevertDirectory(Directory: string);
begin
  ExecuteProcess(TortoiseProc, '/command:revert /path:"' + Directory + '" /notempfile /closeonend');
end;

procedure TTSVN_SCM.RevertFile(FileName: string);
begin
end;

procedure TTSVN_SCM.DiffFile(FileName: string);
begin
  ExecuteProcess(TortoiseProc, '/command:diff /path:"' + FileName + '" /notempfile /closeonend');
end;

procedure TTSVN_SCM.DiffToFile(FileName, ToFileName: string);
begin
  ExecuteProcess(TortoiseMerge, '/base:"' + FileName + '" /mine:' + ToFileName);
end;

function TTSVN_SCM.GetTortoiseProc: string;
var
  s: string;
begin
  s := '';
  if (s = '') and DirectoryExists('C:\Program Files\TortoiseSVN') then
    s := 'C:\Program Files\TortoiseSVN';
  if s <> '' then
    s := IncludeTrailingPathDelimiter(s);
  if s = '' then
    Result := 'TortoiseProc.exe'
  else if (s <> '') and SameText(RightStr(s, 4), 'bin\') then
    Result := '"' + s + 'TortoiseProc.exe"'
  else
    Result := '"' + s + 'bin\TortoiseProc.exe"';
end;

function TTSVN_SCM.GetTortoiseMerge: string;
var
  s: string;
begin
  s := '';
  if (s = '') and DirectoryExists('C:\Program Files\TortoiseSVN') then
    s := 'C:\Program Files\TortoiseSVN';
  if s <> '' then
    s := IncludeTrailingPathDelimiter(s);
  if s = '' then
    Result := 'TortoiseMerge.exe'
  else if (s <> '') and SameText(RightStr(s, 4), 'bin\') then
    Result := '"' + s + 'TortoiseMerge.exe"'
  else
    Result := '"' + s + 'bin\TortoiseMerge.exe"';
end;

constructor TTSVN_SCM.Create;
begin
  inherited Create;
  FName := 'TSVN';
  FTitle := 'Tortoise Subversion';
  FDescription := 'Tortoise Subversion for windows';
end;

initialization
  with Engine do
  begin
    SourceManagements.Add(TTSVN_SCM);
  end;
end.

