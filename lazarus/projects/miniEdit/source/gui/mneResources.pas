unit mneResources;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  SysUtils, Classes, ImgList, Controls;

type

  { TEditorResource }

  TEditorResource = class(TDataModule)
    SmallImages: TImageList;
    BookmarkImages: TImageList;
    DebugImages: TImageList;
    ToolbarImageList: TImageList;
    procedure DataModuleCreate(Sender: TObject);
  private
  public
  end;

const
  DEBUG_IMAGE_EXECUTE = 0;
  DEBUG_IMAGE_BREAKPOINT = 1;
  DEBUG_IMAGE_MARGINES = 0;

var
  EditorResource: TEditorResource = nil;

implementation

{$R *.lfm}

procedure TEditorResource.DataModuleCreate(Sender: TObject);
begin
end;

end.
