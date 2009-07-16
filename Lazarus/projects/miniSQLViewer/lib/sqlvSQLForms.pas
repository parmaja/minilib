unit fbvSQLForms;
{-----------------------------------------------------------------------------
 Author:    zaher
 Purpose:
 History:
-----------------------------------------------------------------------------}
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  fbvSessions, SynCompletionProposal, SynEdit, SynEditHighlighter, SynHighlighterFirebird;

type
  TfbvSQLForm = class(TForm)
  private
    Completion: TSynCompletionProposal;
  protected
    procedure LoadCompletion(SQLEdit: TSynEdit);
    procedure DoAddKeyword(AKeyword: string; AKind: integer);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  FBExtract, SynHighlighterHashEntries, TypInfo;

constructor TfbvSQLForm.Create(AOwner: TComponent);
var
  aRect: TRect;
begin
  inherited;
  if (WindowState = wsNormal) and (Application.MainForm <> nil) and (Application.MainForm.ClientHandle <> 0) then
  begin
    Windows.GetClientRect(Application.MainForm.ClientHandle, aRect);
    BoundsRect := aRect;
  end;
  Completion := TSynCompletionProposal.Create(Self);
  Completion.Options := [scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter];
  Completion.EndOfTokenChr := '()[]. ';
  Completion.DefaultType := ctCode;
  Completion.ShortCut := scCtrl + VK_SPACE;
end;

procedure TfbvSQLForm.DoAddKeyword(AKeyword: string; AKind: integer);
begin
  AKeyword := LowerCase(AKeyword);
  Completion.InsertList.Add(AKeyword);
  case AKind of
    Ord(tkDatatype): Completion.ItemList.Add('Datatype \column{}\style{+B}' + AKeyword + '\style{-B}');
    Ord(tkFunction): Completion.ItemList.Add('Function \column{}\style{+B}' + AKeyword + '\style{-B}');
    Ord(tkKey): Completion.ItemList.Add('Keyword \column{}\style{+B}' + AKeyword + '\style{-B}');
  end;
end;

procedure TfbvSQLForm.LoadCompletion(SQLEdit: TSynEdit);
  procedure FillNow(Name: string; MetaItems: TMetaItems);
  var
    i: Integer;
  begin
    for i := 0 to MetaItems.Count - 1 do
    begin
      Completion.InsertList.Add('"' + MetaItems[i].Name + '"');
      Completion.ItemList.Add(Name + ' \column{}\style{+B}' + MetaItems[i].Name + '\style{-B}');
//      Completion.ItemList.Add('Table \column{}\style{+B}'+FSession.Tables+'\style{-B}(var Message: TMessage); message WM_PASTE');
    end;
  end;
begin
  Completion.AddEditor(SQLEdit);
  EnumerateKeywords(Ord(tkDatatype), FirebirdTypes, SQLEdit.IdentChars, DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), FirebirdFunctions, SQLEdit.IdentChars, DoAddKeyword);
  EnumerateKeywords(Ord(tkKey), FirebirdKeywords, SQLEdit.IdentChars, DoAddKeyword);
  FillNow('Table', (Owner as TfbvSession).Tables);
  FillNow('Procedure', (Owner as TfbvSession).Proceduers);
  FillNow('View', (Owner as TfbvSession).Views);
  FillNow('Generator', (Owner as TfbvSession).Generators);
  FillNow('Functions', (Owner as TfbvSession).Functions);
  FillNow('Exceptions', (Owner as TfbvSession).Exceptions);
  FillNow('Domains', (Owner as TfbvSession).Domains);
  FillNow('Fields', (Owner as TfbvSession).Fields);
//  Completion.ItemList.Add('somthing \column{}\style{+B}WMPaste\style{-B}(var Message: TMessage); message WM_PASTE');
end;

end.

