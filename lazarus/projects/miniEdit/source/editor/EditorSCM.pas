unit EditorSCM;
{$mode delphi}
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
  SysUtils, Forms, StrUtils, Variants, Classes, Controls, Graphics, Contnrs, IAddons, SynEdit;

type

  { TEditorSCM }

  TEditorSCM = class(TAddon)
  private
  protected
  public
    procedure CommitDirectory(Directory: string); virtual;
    procedure CommitFile(FileName: string); virtual;
    procedure UpdateDirectory(Directory: string); virtual;
    procedure UpdateFile(FileName: string); virtual;
    procedure RevertDirectory(Directory: string); virtual;
    procedure RevertFile(FileName: string); virtual;
    procedure DiffFile(FileName: string); virtual;
    procedure DiffToFile(FileName, ToFileName: string); virtual;
  end;

implementation

{ TEditorSCM }

procedure TEditorSCM.CommitDirectory(Directory: string);
begin

end;

procedure TEditorSCM.CommitFile(FileName: string);
begin

end;

procedure TEditorSCM.UpdateDirectory(Directory: string);
begin

end;

procedure TEditorSCM.UpdateFile(FileName: string);
begin

end;

procedure TEditorSCM.RevertDirectory(Directory: string);
begin

end;

procedure TEditorSCM.RevertFile(FileName: string);
begin

end;

procedure TEditorSCM.DiffFile(FileName: string);
begin

end;

procedure TEditorSCM.DiffToFile(FileName, ToFileName: string);
begin

end;

end.

