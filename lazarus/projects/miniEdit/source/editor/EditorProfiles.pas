unit EditorProfiles;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{/$I SynEdit.inc}

interface

uses
  Messages, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls, Registry, ExtCtrls, Buttons, ImgList,
  Contnrs, Menus, SynEdit, SynEditHighlighter, SynEditMiscClasses, SynEditPointClasses, SynGutter, SynEditKeyCmds, Classes, SysUtils;

const
  cDefaultOptions = [eoAltSetsColumnMode, eoAutoIndent, eoDragDropEditing, eoDropFiles, eoScrollPastEol,
    eoShowScrollHint, eoRightMouseMovesCursor, eoTabsToSpaces, eoTabIndent, eoTrimTrailingSpaces, eoKeepCaretX];

type
  TAttributeProfile = class(TCollectionItem)
  private
    FBackground: TColor;
    FForeground: TColor;
    FStyle: TFontStyles;
    FName: string;
  public
    constructor Create(ACollection: TCollection); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Name: string read FName write FName;
    property Background: TColor read FBackground write FBackground default clNone;
    property Foreground: TColor read FForeground write FForeground default clNone;
    property Style: TFontStyles read FStyle write FStyle default [];
  end;

  { TAttributesProfile }

  TAttributesProfile = class(TCollection)
  private
    function GetItem(Index: Integer): TAttributeProfile;
    procedure SetItem(Index: Integer; const Value: TAttributeProfile);
    function GetAttribute(Index: string): TAttributeProfile;
  public
    property Items[Index: Integer]: TAttributeProfile read GetItem write SetItem;
    property Attribute[Index: string]: TAttributeProfile read GetAttribute; default;
  end;

  THighlighterProfile = class(TCollectionItem)
  private
    FName: string;
    FAttributes: TAttributesProfile;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Name: string read FName write FName;
    property Attributes: TAttributesProfile read FAttributes;
  end;

  THighlightersProfile = class(TCollection)
  private
    function GetItem(Index: Integer): THighlighterProfile;
    procedure SetItem(Index: Integer; const Value: THighlighterProfile);
    function GetHighlighter(Index: string): THighlighterProfile;
  public
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: THighlighterProfile read GetItem write SetItem;
    property Highlighter[Index: string]: THighlighterProfile read GetHighlighter; default;
  end;

  { TGutterOptions }

  TGutterOptions = class(TPersistent)
  private
    FAutoSize: boolean;
    FBackcolor: TColor;
    FForecolor: TColor;
    FSavedColor: TColor;
    FSeparatorColor: TColor;
    FLeftOffset: integer;
    FRightOffset: integer;
    FShowLineNumbers: Boolean;
    FShowModifiedLines: Boolean;
    FUnsavedColor: TColor;
    FVisible: Boolean;
    FLeadingZeros: Boolean;
    FZeroStart: Boolean;
    FShowSeparator: Boolean;
    FWidth: Integer;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    constructor AssignFrom(SynGutter: TSynGutter);
    procedure Reset; virtual;
  published
    property AutoSize: boolean read FAutoSize write FAutoSize default True;
    property Backcolor: TColor read FBackcolor write FBackcolor default clBtnFace;
    property Forecolor: TColor read FForecolor write FForecolor default clBtnText;
    property SeparatorColor: TColor read FSeparatorColor write FSeparatorColor default clBtnText;
    property ShowSeparator: Boolean read FShowSeparator write FShowSeparator default True;

    property SavedColor: TColor read FSavedColor write FSavedColor default clGreen;
    property UnsavedColor: TColor read FUnsavedColor write FUnsavedColor default clYellow;
    property ShowModifiedLines: Boolean read FShowModifiedLines write FShowModifiedLines default True;

    property LeftOffset: integer read FLeftOffset write FLeftOffset default 0;
    property RightOffset: integer read FRightOffset write FRightOffset default 0;
    property Visible: boolean read FVisible write FVisible default True;
    property Width: integer read FWidth write FWidth default 30;
    property ShowLineNumbers: Boolean read FShowLineNumbers write FShowLineNumbers default True;
    property LeadingZeros: Boolean read FLeadingZeros write FLeadingZeros default False;
    property ZeroStart: Boolean read FZeroStart write FZeroStart default False;
  end;

  //This class is assignable to a SynEdit without modifying key properties that affect function

  { TEditorProfile }

  TEditorProfile = class(TComponent) //make it as object
  private
    FBackgroundColor: TColor;
    FExtOptions: TSynEditorOptions2;
    FForegroundColor: TColor;
    FMaxUndo: Integer;
    FExtraLineSpacing: Integer;
    FTabWidth: Integer;
    FRightEdge: Integer;
    FSelectedColor: TSynSelectedColor;
    FRightEdgeColor: TColor;
    FFont: TFont;
    FBookmarks: TSynBookMarkOpt;
    FOverwriteCaret: TSynEditCaretType;
    FInsertCaret: TSynEditCaretType;
    FOptions: TSynEditorOptions;
    FGutterOptions: TGutterOptions;
    FInsertMode: Boolean;
    FHighlighters: THighlightersProfile;
    procedure SetExtOptions(const AValue: TSynEditorOptions2);
    procedure SetFont(const Value: TFont);
    procedure SetOptions(const Value: TSynEditorOptions);
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetChildOwner: TComponent; override;
    function GetChildParent: TComponent; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Reset;
  published
    property Highlighters: THighlightersProfile read FHighlighters;
    property Options: TSynEditorOptions read FOptions write SetOptions default cDefaultOptions;
    property ExtOptions: TSynEditorOptions2 read FExtOptions write SetExtOptions default [];
    property Font: TFont read FFont write SetFont;
    property Gutter: TGutterOptions read FGutterOptions write FGutterOptions;
    property SelectedColor: TSynSelectedColor read FSelectedColor write FSelectedColor;
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor default clWindow;
    property ForegroundColor: TColor read FForegroundColor write FForegroundColor default clWindowText;
    property ExtraLineSpacing: Integer read FExtraLineSpacing write FExtraLineSpacing default 0;
    property RightEdge: Integer read FRightEdge write FRightEdge default 80;
    property RightEdgeColor: TColor read FRightEdgeColor write FRightEdgeColor default clSilver;
    property InsertMode: Boolean read FInsertMode write FInsertMode default True;
    property InsertCaret: TSynEditCaretType read FInsertCaret write FInsertCaret default ctVerticalLine;
    property OverwriteCaret: TSynEditCaretType read FOverwriteCaret write FOverwriteCaret default ctBlock;
    property MaxUndo: Integer read FMaxUndo write FMaxUndo default 1024;
    property TabWidth: Integer read FTabWidth write FTabWidth default 2;
  end;

implementation

uses
  SynGutterBase, SynGutterLineNumber, SynGutterChanges;

{ TEditorProfile }

type
  THackCustomSynEdit = class(TCustomSynEdit);

procedure TEditorProfile.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TCustomSynEdit) then
  begin
    Self.Font.Assign(TCustomSynEdit(Source).Font);
    Self.Gutter.Assign(THackCustomSynEdit(Source).Gutter);
    Self.SelectedColor.Assign(THackCustomSynEdit(Source).SelectedColor);

    Self.Options := THackCustomSynEdit(Source).Options;
    Self.ExtraLineSpacing := THackCustomSynEdit(Source).ExtraLineSpacing;
    Self.InsertCaret := THackCustomSynEdit(Source).InsertCaret;
    Self.OverwriteCaret := THackCustomSynEdit(Source).OverwriteCaret;
    Self.MaxUndo := THackCustomSynEdit(Source).MaxUndo;
    Self.RightEdge := THackCustomSynEdit(Source).RightEdge;
    Self.RightEdgeColor := THackCustomSynEdit(Source).RightEdgeColor;
    Self.TabWidth := THackCustomSynEdit(Source).TabWidth;
  end
  else
    inherited;
end;

procedure TEditorProfile.AssignTo(Dest: TPersistent);
begin
  if Assigned(Dest) and (Dest is TCustomSynEdit) then
  begin
    THackCustomSynEdit(Dest).Font.Assign(Self.Font);
    if TCustomSynEdit(Dest).Highlighter = nil then
    begin
      TCustomSynEdit(Dest).Font.Color := ForegroundColor;
      TCustomSynEdit(Dest).Color := BackgroundColor;
    end
    else
    begin
      TCustomSynEdit(Dest).Font.Color := TCustomSynEdit(Dest).Highlighter.WhitespaceAttribute.Foreground;
      TCustomSynEdit(Dest).Color := TCustomSynEdit(Dest).Highlighter.WhitespaceAttribute.Background;//BUG: bad to be here
    end;

    THackCustomSynEdit(Dest).Options := THackCustomSynEdit(Dest).Options - [eoDropFiles]; //make main window accept the files
    THackCustomSynEdit(Dest).Gutter.Assign(Self.Gutter);
    THackCustomSynEdit(Dest).SelectedColor.Assign(Self.SelectedColor);

    THackCustomSynEdit(Dest).Font.Quality := fqNonAntialiased; //not work

    THackCustomSynEdit(Dest).Options := Self.Options;
    THackCustomSynEdit(Dest).ExtraLineSpacing := Self.ExtraLineSpacing;
    THackCustomSynEdit(Dest).InsertCaret := Self.InsertCaret;
    THackCustomSynEdit(Dest).OverwriteCaret := Self.OverwriteCaret;
    THackCustomSynEdit(Dest).MaxUndo := Self.MaxUndo;
    THackCustomSynEdit(Dest).RightEdge := Self.RightEdge;
    THackCustomSynEdit(Dest).RightEdgeColor := Self.RightEdgeColor;
    THackCustomSynEdit(Dest).TabWidth := Self.TabWidth;
  end
  else
    inherited;
end;

constructor TEditorProfile.Create(AOwner: TComponent);
begin
  inherited;
  FComponentStyle := FComponentStyle + [csSubComponent];
  FBookmarks := TSynBookMarkOpt.Create(Self);
  FGutterOptions := TGutterOptions.Create;//ToDO check the Create params
  FSelectedColor := TSynSelectedColor.Create;
  FFont := TFont.Create;
  FHighlighters := THighlightersProfile.Create(THighlighterProfile);
  FBackgroundColor := clWindow;
  FForegroundColor := clWindowText;
  Reset;
end;

destructor TEditorProfile.Destroy;
begin
  FBookMarks.Free;
  FGutterOptions.Free;
  FSelectedColor.Free;
  FHighlighters.Free;
  FFont.Free;
  inherited;
end;

function TEditorProfile.GetChildOwner: TComponent;
begin
  Result := Self;
end;

function TEditorProfile.GetChildParent: TComponent;
begin
  Result := Self;
end;

procedure TEditorProfile.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
var
  i: Integer;
begin
  inherited;
  for i := 0 to ComponentCount - 1 do
    Proc(Components[i]);
end;

procedure TEditorProfile.Reset;
begin
  ForegroundColor := clWindowText;
  BackgroundColor := clWindow;
  Gutter.Reset;
  FSelectedColor.Foreground := clHighlightText;
  FSelectedColor.Background := clHighlight;
  FFont.Name := 'Courier New';
  FFont.Size := 10;
  Options := cDefaultOptions;
  ExtraLineSpacing := 0;
  InsertCaret := ctVerticalLine;
  OverwriteCaret := ctBlock;
  MaxUndo := 1024;
  RightEdge := 80;
  RightEdgeColor := clSilver;
  FInsertMode := True;
  TabWidth := 2;
end;

procedure TEditorProfile.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TEditorProfile.SetExtOptions(const AValue: TSynEditorOptions2);
begin
  if FExtOptions =AValue then exit;
  FExtOptions :=AValue;
end;

procedure TEditorProfile.SetOptions(
  const Value: TSynEditorOptions);
begin
  FOptions := Value;
end;

{ THighlightersProfile }

procedure THighlightersProfile.Assign(Source: TPersistent);
var
  Profile: THighlighterProfile;
begin
  if Source is TSynCustomHighlighter then
  begin
    Profile := Highlighter[TSynCustomHighlighter(Source).GetLanguageName];
    if Profile = nil then
    begin
      Profile := THighlighterProfile.Create(Self);
      Profile.Name := TSynCustomHighlighter(Source).GetLanguageName;
    end;
    Profile.Assign(TSynCustomHighlighter(Source));
  end
  else
    inherited;
end;

procedure THighlightersProfile.AssignTo(Dest: TPersistent);
var
  Profile: THighlighterProfile;
begin
  if Dest is TSynCustomHighlighter then
  begin
    Profile := Highlighter[TSynCustomHighlighter(Dest).GetLanguageName];
    if Profile <> nil then
    begin
      Profile.AssignTo(TSynCustomHighlighter(Dest));
    end;
  end
  else
    inherited;
end;

function THighlightersProfile.GetHighlighter(
  Index: string): THighlighterProfile;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, Index) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function THighlightersProfile.GetItem(Index: Integer): THighlighterProfile;
begin
  Result := inherited Items[Index] as THighlighterProfile;
end;

procedure THighlightersProfile.SetItem(Index: Integer;
  const Value: THighlighterProfile);
begin
  inherited Items[Index] := Value;
end;

{ THighlighterProfile }

procedure THighlighterProfile.Assign(Source: TPersistent);
var
  i: Integer;
  aItem: TAttributeProfile;
begin
  if Source is TSynCustomHighlighter then
  begin
    Attributes.Clear;
    for i := 0 to TSynCustomHighlighter(Source).AttrCount - 1 do
    begin
      aItem := TAttributeProfile.Create(Attributes);
      aItem.Assign(TSynCustomHighlighter(Source).Attribute[i]);
    end;
  end
  else
    inherited;
end;

procedure THighlighterProfile.AssignTo(Dest: TPersistent);
var
  i: Integer;
  aItem: TAttributeProfile;
begin
  if Dest is TSynCustomHighlighter then
  begin
    for i := 0 to TSynCustomHighlighter(Dest).AttrCount - 1 do
    begin
      aItem := Attributes[TSynCustomHighlighter(Dest).Attribute[i].Name];
      if aItem <> nil then
        aItem.AssignTo(TSynCustomHighlighter(Dest).Attribute[i]);
    end;
  end
  else
    inherited;
end;

constructor THighlighterProfile.Create(ACollection: TCollection);
begin
  inherited;
  FAttributes := TAttributesProfile.Create(TAttributeProfile);
end;

destructor THighlighterProfile.Destroy;
begin
  FAttributes.Free;
  inherited;
end;

{ TAttributesProfile }

function TAttributesProfile.GetAttribute(Index: string): TAttributeProfile;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, Index) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TAttributesProfile.GetItem(Index: Integer): TAttributeProfile;
begin
  Result := inherited Items[Index] as TAttributeProfile;
end;

procedure TAttributesProfile.SetItem(Index: Integer;
  const Value: TAttributeProfile);
begin
  inherited Items[Index] := Value;
end;

{ TAttributeProfile }

procedure TAttributeProfile.Assign(Source: TPersistent);
begin
  if Source is TSynHighlighterAttributes then
  begin
    Name := TSynHighlighterAttributes(Source).Name;
    Background := TSynHighlighterAttributes(Source).Background;
    Foreground := TSynHighlighterAttributes(Source).Foreground;
    Style := TSynHighlighterAttributes(Source).Style;
  end
  else
    inherited;
end;

procedure TAttributeProfile.AssignTo(Dest: TPersistent);
begin
  if Dest is TSynHighlighterAttributes then
  begin
  //  TSynHighlighterAttributes(Source).Name := Name;
    TSynHighlighterAttributes(Dest).Background := Background;
    TSynHighlighterAttributes(Dest).Foreground := Foreground;
    TSynHighlighterAttributes(Dest).Style := Style;
  end
  else
    inherited;
end;

constructor TAttributeProfile.Create(ACollection: TCollection);
begin
  inherited;
  FBackground := clNone;
  FForeground := clNone;
end;

{ TGutterOptions }

constructor TGutterOptions.Create;
begin
  inherited;
  Reset;
end;

procedure TGutterOptions.Assign(Source: TPersistent);
begin
  if Source is TSynGutter then
    AssignFrom(Source as TSynGutter)
  else
    inherited Assign(Source);
end;

procedure TGutterOptions.AssignTo(Dest: TPersistent);
var
  SynGutter: TSynGutter;
  i: Integer;
  gp: TSynGutterLineNumber;
  sp: TSynGutterSeparator;
  ch: TSynGutterChanges;
begin
  if Dest is TSynGutter then
  begin
    SynGutter := Dest as TSynGutter;

    SynGutter.AutoSize := FAutoSize;
    SynGutter.Color := FBackcolor;
    for i := 0 to SynGutter.Parts.Count -1 do
    begin
      SynGutter.Parts[i].MarkupInfo.Foreground := FForecolor;
      SynGutter.Parts[i].MarkupInfo.Background := FBackcolor;
    end;
    gp := SynGutter.Parts.ByClass[TSynGutterLineNumber, 0] as TSynGutterLineNumber;
    if gp <> nil then
    begin
      gp.Visible := FShowLineNumbers;
      gp.LeadingZeros := FLeadingZeros;
      gp.ZeroStart := FZeroStart;
    end;
    sp := SynGutter.Parts.ByClass[TSynGutterSeparator, 0] as TSynGutterSeparator;
    if sp <> nil then
    begin
      sp.Visible := FShowSeparator;
      sp.MarkupInfo.Foreground := FSeparatorColor;
      sp.MarkupInfo.Background := FSeparatorColor;
    end;
    ch := SynGutter.Parts.ByClass[TSynGutterChanges, 0] as TSynGutterChanges;
    if ch <> nil then
    begin
      ch.Visible := FShowModifiedLines;
      ch.SavedColor := FSavedColor;
      ch.ModifiedColor := FUnsavedColor;
    end;
    SynGutter.LeftOffset := FLeftOffset;
    SynGutter.RightOffset := FRightOffset;
    SynGutter.Visible := FVisible;
    SynGutter.Width := FWidth;
  end
  else
    inherited AssignTo(Dest);
end;

constructor TGutterOptions.AssignFrom(SynGutter: TSynGutter);
var
  gp: TSynGutterLineNumber;
  sp: TSynGutterSeparator;
  ch: TSynGutterChanges;
begin
  FAutoSize := SynGutter.AutoSize;
  FBackcolor := SynGutter.Color;
  FLeftOffset := SynGutter.LeftOffset;
  FRightOffset := SynGutter.RightOffset;
  FVisible := SynGutter.Visible;
  FWidth := SynGutter.Width;
  gp := SynGutter.Parts.ByClass[TSynGutterLineNumber, 0] as TSynGutterLineNumber;
  if gp <> nil then
  begin
    FShowLineNumbers := gp.Visible;
    FLeadingZeros := gp.LeadingZeros;
    FZeroStart := gp.ZeroStart;
  end;
  sp := SynGutter.Parts.ByClass[TSynGutterSeparator, 0] as TSynGutterSeparator;
  if sp <> nil then
  begin
    FShowSeparator := sp.Visible;
    FSeparatorColor := sp.MarkupInfo.Foreground;
  end;
  ch := SynGutter.Parts.ByClass[TSynGutterChanges, 0] as TSynGutterChanges;
  if ch <> nil then
  begin
    FShowModifiedLines := ch.Visible;
    FSavedColor := ch.SavedColor;
    FUnsavedColor := ch.ModifiedColor;
  end;
end;

procedure TGutterOptions.Reset;
begin
  FAutoSize := True;
  FBackcolor := clBtnFace;
  FForecolor := clBtnText;
  FSeparatorColor := clBtnFace;
  FShowSeparator := True;
  FLeftOffset := 0;
  FRightOffset := 0;
  FVisible := True;
  FWidth := 30;
  FShowLineNumbers := True;
  FLeadingZeros := False;
  FZeroStart := False;
  FShowModifiedLines := True;
  FSavedColor := clGreen;
  FUnsavedColor := clYellow;
end;

end.
