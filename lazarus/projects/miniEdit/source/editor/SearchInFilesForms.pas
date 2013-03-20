unit SearchInFilesForms;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, SynEdit, SynEditTypes, SynEditRegexSearch, SynEditMiscClasses,
  SynEditSearch, SearchProgressForms, ComCtrls;

type
  TSearchListItem = class(TListItem)
  private
    FColumn: Integer;
    FLength: Integer;
  public
    property Column: Integer read FColumn write FColumn;
    property Length: Integer read FLength write FLength;
  end;

  TSearchInFilesForm = class(TForm)
    Label1: TLabel;
    SearchTextEdit: TComboBox;
    SearchOptionsGrp: TGroupBox;
    FindBtn: TButton;
    CancelBtn: TButton;
    SearchCaseSensitiveChk: TCheckBox;
    SearchWholeWordsChk: TCheckBox;
    ReplaceWithEdit: TComboBox;
    ReplaceWithChk: TCheckBox;
    Label2: TLabel;
    SearchFolderEdit: TComboBox;
    SearchFilesGrp: TRadioGroup;
    procedure ReplaceWithChkClick(Sender: TObject);
  private
    procedure UpdateReplace;
    procedure FoundEvent(FileName: string; const Line: string; LineNo, Column, FoundLength: Integer);
    procedure SearchReplaceText;
    procedure SearchInFiles;
    procedure SearchInFile(FileName: string);
  protected
    FSearchCount: Integer;
    FProgressForm: TSearchProgressForm;
    FSearchText: string;
    FReplaceText: string;
    FSearchOptions: TSynSearchOptions;
    FSearchList: TListView;
  public
  end;

procedure ShowSearchInFilesForm(ASearchList: TListView; SearchText, SearchFolder: string; SearchFolderHistory, SearchHistory, ReplaceHistory: TStringList);

implementation

uses EditorEngine, SearchForms;

{$R *.lfm}

procedure DoSearchInFileCallback(AObject: TObject; const FileName: string; Count, Level:Integer; var Resume: Boolean);
begin
  with (AObject as TSearchInFilesForm) do
  begin
    SearchInFile(ExpandFileName(FileName));
    if (Count mod 25) = 0 then
    begin
      Application.ProcessMessages;
      if FProgressForm.Canceled then
        Resume := False;
      Application.ProcessMessages;
    end;
  end;
end;

procedure TSearchInFilesForm.SearchInFiles;
var
  aMasks: string;
begin
  if SearchFilesGrp.ItemIndex = 0 then
    aMasks := Engine.Perspective.Groups.CreateFilter(False)
  else
    aMasks := Engine.Groups.CreateFilter(False);

  EnumFileList(IncludeTrailingPathDelimiter(SearchFolderEdit.Text), aMasks, Engine.Options.IgnoreNames, @DoSearchInFileCallback, Self, 1000, 3, True, True);
end;

procedure TSearchInFilesForm.SearchReplaceText;
begin
  FProgressForm := TSearchProgressForm.Create(Application);
  try
    FProgressForm.Show;
    FSearchText := SearchTextEdit.Text;
    FReplaceText := ReplaceWithEdit.Text;
    if SearchCaseSensitiveChk.Checked then
      Include(FSearchOptions, ssoMatchCase);
    if SearchWholeWordsChk.Checked then
      Include(FSearchOptions, ssoWholeWord);

    if ReplaceWithChk.Checked then
      FSearchOptions := [ssoReplace, ssoReplaceAll];

    SearchInFiles;
    SetTextSearch(FSearchText, FReplaceText, FSearchOptions);// send text to normal text search
    Engine.Files.CheckChanged;
  finally
    FreeAndNil(FProgressForm);
  end;
end;

procedure ShowSearchInFilesForm(ASearchList: TListView; SearchText, SearchFolder: string; SearchFolderHistory, SearchHistory, ReplaceHistory: TStringList);
var
  aForm: TSearchInFilesForm;
  i: Integer;
begin
  aForm := TSearchInFilesForm.Create(Application);
  with aForm do
  try
    // assign search FSearchOptions
    // start with last search text
    FSearchList := ASearchList;
    if SearchHistory <> nil then
      SearchTextEdit.Items.Assign(SearchHistory);
    if ReplaceHistory <> nil then
      ReplaceWithEdit.Items.Assign(ReplaceHistory);
    if SearchFolderHistory <> nil then
      SearchFolderEdit.Items.Assign(SearchFolderHistory);

    UpdateReplace;

    SearchTextEdit.Text := SearchText;
    SearchFolderEdit.Text := SearchFolder;

    if ShowModal = mrOK then
    begin
      if SearchTextEdit.Text <> '' then
      begin
        FSearchList.Clear;
        SearchReplaceText;

        if SearchHistory <> nil then
        begin
          i := SearchHistory.IndexOf(SearchTextEdit.Text);
          if i >= 0 then
            SearchHistory.Delete(i);
          SearchHistory.Insert(0, SearchTextEdit.Text);
          while SearchHistory.Count > 25 do
          begin
            SearchHistory.Delete(SearchHistory.Count - 1);
          end;
        end;

        if SearchFolderHistory <> nil then
        begin
          i := SearchFolderHistory.IndexOf(SearchFolderEdit.Text);
          if i >= 0 then
            SearchFolderHistory.Delete(i);
          SearchFolderHistory.Insert(0, SearchFolderEdit.Text);
          while SearchFolderHistory.Count > 25 do
          begin
            SearchFolderHistory.Delete(SearchFolderHistory.Count - 1);
          end;
        end;

        if ReplaceWithEdit.Text <> '' then
          if ReplaceHistory <> nil then
          begin
            i := ReplaceHistory.IndexOf(ReplaceWithEdit.Text);
            if i >= 0 then
              ReplaceHistory.Delete(i);
            ReplaceHistory.Insert(0, ReplaceWithEdit.Text);
            while ReplaceHistory.Count > 25 do
            begin
              ReplaceHistory.Delete(ReplaceHistory.Count - 1);
            end;
          end;
      end;
    end;
  finally
    aForm.Free;
  end;
end;

procedure TSearchInFilesForm.ReplaceWithChkClick(Sender: TObject);
begin
  ReplaceWithEdit.Enabled := ReplaceWithChk.Checked;
  UpdateReplace;
end;

procedure TSearchInFilesForm.UpdateReplace;
begin
  if ReplaceWithChk.Checked then
  begin
    ReplaceWithEdit.Enabled := True;
    ReplaceWithEdit.Color := clWindow;
    ReplaceWithEdit.TabStop := True;
  end
  else
  begin
    ReplaceWithEdit.Enabled := False;
    ReplaceWithEdit.Color := clBtnFace;
    ReplaceWithEdit.TabStop := False;
  end;
end;

procedure TSearchInFilesForm.FoundEvent(FileName: string; const Line: string; LineNo, Column, FoundLength: Integer);
var
  aItem: TSearchListItem;
begin
  Inc(FSearchCount);
  aItem := TSearchListItem.Create(FSearchList.Items);
  FSearchList.Items.AddItem(aItem);
  aItem.Caption := FileName;
  aItem.ImageIndex := 34;
  aItem.Column := Column;
  aItem.Length := FoundLength;
  aItem.SubItems.Add(IntToStr(LineNo));
  aItem.SubItems.Add(Line);
end;

procedure TSearchInFilesForm.SearchInFile(FileName: string);
var
  Contents: string;
  Size: Integer;
  Stream: TFileStream;
  aStrings: TStringList;
  Mode: TEditorFileMode;
begin
  FileName := ExpandFileName(FileName);
  if FProgressForm <> nil then
  begin
    FProgressForm.FileNameLbl.Caption := FileName;
    Application.ProcessMessages;
  end;
  aStrings := TStringList.Create;
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      Size := Stream.Size - Stream.Position;
      SetString(Contents, nil, Size);
      Stream.Read(Pointer(Contents)^, Size);
      Mode := DetectFileMode(Contents);
      aStrings.Text := Contents;
    finally
      Stream.Free;
    end;
    Engine.SearchReplace(FileName, aStrings, FSearchText, FReplaceText, @FoundEvent, FSearchOptions);
    if ssoReplace in FSearchOptions then
      SaveAsMode(FileName, Mode, aStrings);
  finally
    aStrings.Free;
  end;
  if FProgressForm <> nil then
  begin
    FProgressForm.FoundLbl.Caption := IntToStr(FSearchCount);
    Application.ProcessMessages;
  end;
end;

end.

