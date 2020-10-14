unit ChatRoomFrames;

{$mode objfpc}{$H+}
{$define use_webbrowser}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, Menus,
  Graphics,
  {$ifdef use_webbrowser}
  IpHtml,
  //HtmlView, HTMLSubs,
  {$endif}
  SynEdit, SynHighlighterMulti;

type

  { TChatRoomFrame }

  TChatRoomFrame = class(TFrame)
    ChangeTopicBtn: TButton;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    TopicEdit: TEdit;
    WhoIsMnu: TMenuItem;
    OpMnu: TMenuItem;
    MenuItem2: TMenuItem;
    UsersPopupMenu: TPopupMenu;
    Splitter2: TSplitter;
    UserListBox: TListView;
    procedure ChangeTopicBtnClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure OpMnuClick(Sender: TObject);
    procedure WhoIsMnuClick(Sender: TObject);
  private
    function GetCurrentUser: string;
  protected
    {$ifdef use_webbrowser}
    //Viewer: THtmlViewer;
    Body: TIpHtmlNodeBODY;
    Viewer: TIpHtmlPanel;
    procedure HtmlEnumerator(Document: TIpHtml);
    {$else}
    MsgEdit: TSynEdit;
    {$endif}
  public
    RoomName: string;
    IsRoom: Boolean;
    constructor Create(TheOwner: TComponent); override;
    procedure AddMessage(aMsg: string);
  end;

implementation

uses
  MainForm;

{$R *.lfm}

{ TChatRoomFrame }

procedure TChatRoomFrame.MenuItem1Click(Sender: TObject);
var
  aUser: string;
begin
  aUser := GetCurrentUser;
  if aUser <> '' then
    IRC.OpUser(RoomName, aUser);
end;

procedure TChatRoomFrame.OpMnuClick(Sender: TObject);
var
  aUser: string;
begin
  aUser := GetCurrentUser;
  if aUser <> '' then
    IRC.OpUser(RoomName, aUser);
end;

procedure TChatRoomFrame.WhoIsMnuClick(Sender: TObject);
var
  aUser: string;
begin
  aUser := GetCurrentUser;
  if aUser <> '' then
    IRC.WhoIs(aUser);
end;

function TChatRoomFrame.GetCurrentUser: string;
begin
  Result := '';
  if UserListBox.Items.Count >0 then
  begin
    Result := UserListBox.Selected.Caption;
  end;
end;

{$ifdef use_webbrowser}
procedure TChatRoomFrame.HtmlEnumerator(Document: TIpHtml);
var
   n: TIpHtmlNode;
   nb: TIpHtmlNodeBODY;
   i: Integer;
begin
   if not Assigned(Document.HtmlNode) then begin
      Exit;
   end;
   if Document.HtmlNode.ChildCount < 1 then begin
      Exit;
   end;
   for i := 0 to Document.HtmlNode.ChildCount -1 do
   begin
     n := Document.HtmlNode.ChildNode[i];
     if (n is TIpHtmlNodeBODY) then
     begin
       Body := TIpHtmlNodeBODY(n);
       exit;
     end;
   end;
end;
{$endif}

procedure TChatRoomFrame.ChangeTopicBtnClick(Sender: TObject);
begin
  Viewer.Scroll(hsaEnd);
end;

constructor TChatRoomFrame.Create(TheOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(TheOwner);
  {$ifdef use_webbrowser}
  HandleAllocated;
  //Viewer := THtmlViewer.Create(Self);
  Viewer := TIpHtmlPanel.Create(Self);
  with Viewer do
  begin
    Parent := Self;
    Align := alClient;
    BorderStyle := bsNone;
    MarginHeight := 10;
    MarginWidth := 10;
    Visible := True;
    //ScrollBars := ssAutoVertical;
  end;
  //Viewer.LoadFromFile(Application.Location + 'chat.html');
  Viewer.SetHtmlFromFile(Application.Location + 'chat.html');
  //Find Body
  //Viewer.EnumDocuments(@HtmlEnumerator);
  for i :=0 to Viewer.MasterFrame.Html.HtmlNode.ChildCount - 1 do
    if Viewer.MasterFrame.Html.HtmlNode.ChildNode[i] is TIpHtmlNodeBODY then
    begin
      Body := TIpHtmlNodeBODY(Viewer.MasterFrame.Html.HtmlNode.ChildNode[i]);
      break;
    end;
  {$else}
  MsgEdit := TSynEdit.Create(TheOwner);
  with MsgEdit do
  begin
    Parent := Self;
    //ParentWindow := Handle;
    Align := alClient;
    ScrollBars := ssAutoVertical;
    ReadOnly := True;
    Gutter.Visible := False;
    Options := Options + [eoHideRightMargin];
  end;
  {$endif}
end;

procedure TChatRoomFrame.AddMessage(aMsg: string);
{$ifdef use_webbrowser}
var
  aNode: TIpHtmlNodeText;
{$endif}
begin
  {$ifdef use_webbrowser}
  aNode := TIpHtmlNodeText.Create(Body);
  with aNode do
  begin
    aNode.EscapedText := aMsg;
  end;

  with TIpHtmlNodeBR.Create(Body) do
  begin
  end;

  Viewer.Update;
  Viewer.Scroll(hsaEnd);
  {$else}
  MsgEdit.Lines.Add(aMSG);
  MsgEdit.CaretY := MsgEdit.Lines.Count;
  //MsgEdit.ScrollBy(0, 1);
  {$endif}
end;

end.

