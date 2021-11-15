unit MainForms;

{
 this example how to Read/Write an object include properties using mnXMLProfile  
}
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, LR_Class, SynEdit, SynHighlighterXML,
  mnXMLRttiProfile;

type
  TMyItems = class;

  TMyItem = Class(TmnXMLItem)
  private
    FName: string;
    FChilds: TMyItems;
    FSingle: Boolean;
    function GetChildsStored: Boolean;
  public
    constructor Create;
    destructor Destroy; override; 
  published
    property Name:string read FName write FName;
    property Childs:TMyItems read FChilds write FChilds stored GetChildsStored;
    property Single: Boolean read FSingle write FSingle default True; 
  end;

  TMyItems = class(TmnXMLItems)
  private
    FName: string;
    function GetItem(Index: Integer): TMyItem;
    procedure SetItem(Index: Integer; const Value: TMyItem);
  public
    function DoCreateItem(AClass: TmnXMLItemClass):TmnXMLItem; override;
    property Items[Index: Integer]: TMyItem read GetItem write SetItem; default;
    property Name:string read FName write FName;
  published
  end;

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Panel1: TPanel;
    Splitter1: TSplitter;
    SynEdit1: TSynEdit;
    SynEdit2: TSynEdit;
    SynXMLSyn1: TSynXMLSyn;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMyItems }

function TMyItems.DoCreateItem(AClass: TmnXMLItemClass): TmnXMLItem;
begin
  Result := TMyItem.Create;
end;

function TMyItems.GetItem(Index: Integer): TMyItem;
begin
  Result := inherited Items[Index] as TMyItem;
end;

procedure TMyItems.SetItem(Index: Integer; const Value: TMyItem);
begin
  Items[Index] := Value;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  aItems:TMyItems;
  aItem:TMyItem;
  aSubItem:TMyItem;
  s:string;
begin
  aItems := TMyItems.Create;
  try
    aItems.Name := 'Address';

    aItem:=TMyItem.Create;
    aItem.Name := 'Zaher'#13#10' Dirkey';
    aItems.Add(aItem);

    aSubItem:=TMyItem.Create;
    aSubItem.Name := 'Lina';
    aItem.Childs.Add(aSubItem);

    aSubItem:=TMyItem.Create;
    aSubItem.Name := 'Aya';
    aItem.Childs.Add(aSubItem);

    aSubItem:=TMyItem.Create;
    aSubItem.Name := 'Omar';
    aItem.Childs.Add(aSubItem);

    aItem:=TMyItem.Create;
    aItem.Name := 'Jhon Smith';
    aItems.Add(aItem);

    aItems.SaveToString(s);
    aItems.SaveToFile('c:\333.txt');
    SynEdit1.Lines.Text := s;
  finally
    aItems.Free;
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  aItems:TMyItems;
  s:string;
begin
  aItems := TMyItems.Create;
  try
    aItems.LoadFromString(SynEdit1.Lines.Text);
    if aItems.Count > 0 then
    begin
      aItems.SaveToString(s);
      SynEdit2.Lines.Text := s;
    end
    else
      SynEdit2.Lines.Clear;
  finally
    aItems.Free;
  end;
end;

procedure TMainForm.Button3Click(Sender: TObject);
var
  aItems:TMyItems;
  s:string;
begin
  aItems := TMyItems.Create;
  try
    aItems.LoadFromFile('c:\333.txt');
    aItems.SaveToFile('c:\444.txt');
    if aItems.Count > 0 then
    begin
      aItems.SaveToString(s);
      SynEdit2.Lines.Text := s;
    end
    else
      SynEdit2.Lines.Clear;
  finally
    aItems.Free;
  end;
end;

{ TMyItem }

constructor TMyItem.Create;
begin
  inherited;
  FChilds := TMyItems.Create;
  FSingle := True;
end;

destructor TMyItem.Destroy;
begin
  FChilds.Free;
  inherited;
end;

function TMyItem.GetChildsStored: Boolean;
begin
  Result := Childs.Count > 0;
end;

end.
