unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, snow2cipher, ciphers, hexcipher, ComCtrls, md5;

const
  cBufferSize = 1024;

type

  TTestResult = (trUnknown, trError, trOK);

  TMySnowCiphr = class(TSnow2CipherStream)
  protected
    procedure Prepare; override;
  end;

  TMainForm = class(TForm)
    SrcEdit: TEdit;
    EncEdit: TEdit;
    DecEdit: TEdit;
    TestReadBtn: TButton;
    TestWriteBtn: TButton;
    FileNameLbl: TLabel;
    FileNameEdit: TEdit;
    SelectFileBtn: TButton;
    EncFileNameLbl: TLabel;
    DecFileNameLbl: TLabel;
    SrcTextLbl: TLabel;
    EncTextLbl: TLabel;
    DecTextLbl: TLabel;
    StatusBar: TStatusBar;
    MethodBox: TComboBox;
    MethodLbl: TLabel;
    ResultLbl: TLabel;
    procedure TestReadBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TestWriteBtnClick(Sender: TObject);
    procedure SelectFileBtnClick(Sender: TObject);
    procedure SrcEditChange(Sender: TObject);
    procedure MethodBoxClick(Sender: TObject);
  private
    FFileName: string;
    FTestResult: TTestResult;
    procedure SetFileName(const Value: string);
    function GetFileName: string;
    function GetDeccFileName: string;
    function GetEncFileName: string;
    function GetCipherClass: TCipherStreamClass;
    procedure SetTestResult(const Value: TTestResult);
    function GetFileSize: Cardinal;
    { Private declarations }
  public
    { Public declarations }
    function CreateCipherStream(AStream: TStream; Way: TCipherWay; Mode: TCipherMode; Owned: Boolean = True): TCipherStream;

    procedure TestCipher;

    procedure TestRead;
    procedure TestWrite;

    procedure ReadEncryptFile;
    procedure ReadDecryptFile;

    procedure WriteEncryptFile;
    procedure WriteDecryptFile;
    procedure UpdateInfo;

    property CipherClass: TCipherStreamClass read GetCipherClass;
    property FileName: string read GetFileName write SetFileName;
    property FileSize: Cardinal read GetFileSize;
    property EncFileName: string read GetEncFileName;
    property DecFileName: string read GetDeccFileName;
    property TestResult: TTestResult read FTestResult write SetTestResult;
    procedure CalcSpeed(vTime, vSize: Cardinal);
    procedure ShowInfo(const vInfo: string);
  end;

const
  cTestResult: array[Boolean] of TTestResult = (trError, trOK);

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

function _GetFileSize(const FName: String): LongWord;
var
  FileHandle: THandle;
begin
  Result := INVALID_FILE_SIZE;
  FileHandle := CreateFile(PChar(FName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  if FileHandle = INVALID_HANDLE_VALUE then Exit;
  Result := GetFileSize(FileHandle, nil);
  CloseHandle(FileHandle);
end;

procedure TMainForm.CalcSpeed(vTime, vSize: Cardinal);
var
  s: string;
  sp, sz: Double;
begin
  sz := vSize / (1024*1024);
  if vTime=0 then
    s := Format('Time: 0 ms Size: %.4g MB Speed: ~ MB/S ', [sz])
  else
  begin
    sp := vSize / (vTime*1024*1024/1000);
    s := Format('Time: %d ms Size: %.4g MB Speed: %f MB/S ', [vTime, sz, sp]);
  end;
  ShowInfo(s);
end;

function TMainForm.CreateCipherStream(AStream: TStream; Way: TCipherWay; Mode: TCipherMode; Owned: Boolean): TCipherStream;
begin
  if CipherClass<>nil then
    Result := CipherClass.Create(AStream, Way, Mode, Owned)
  else
    Result := nil;
  //Result := THexCipherStream.Create(AStream, Way, Mode, Owned);
end;

procedure TMainForm.ReadDecryptFile;
var
  st: string;
  fi, fo: TFileStream;
  scs: TCipherStream;
  i: Integer;
begin
  if FileExists(EncFileName) then
  begin
    fi := TFileStream.Create(EncFileName, fmOpenRead);
    fo := TFileStream.Create(DecFileName, fmCreate or fmOpenWrite);
    try
      scs := CreateCipherStream(fi, cyDecrypt, cimRead, false);
      try
        SetLength(st, cBufferSize);
        while True do
        begin
          i := scs.read(st[1], cBufferSize);
          if i=0 then Break;
          fo.Write(st[1], i);
          if i<cBufferSize then Break;          
        end;
        SetLength(st, 0);
      finally
        scs.Free;
      end;
    finally
      fi.Free;
      fo.Free;
    end;
  end;
end;

procedure TMainForm.ReadEncryptFile;
var
  st: string;
  fi, fo: TFileStream;
  scs: TCipherStream;
  i: Integer;
begin
  if FileName<>'' then
  begin
    fi := TFileStream.Create(FileName, fmOpenRead);
    fo := TFileStream.Create(EncFileName, fmCreate or fmOpenWrite);
    try
      scs := CreateCipherStream(fi, cyEncrypt, cimRead, false);
      try
        SetLength(st, cBufferSize);
        while True do
        begin
          i := scs.read(st[1], cBufferSize);
          if i=0 then Break;
          fo.Write(st[1], i);
          if i<cBufferSize then Break;
        end;
        SetLength(st, 0);
      finally
        scs.Free;
      end;
    finally
      fi.Free;
      fo.Free;
    end;
  end;
end;

procedure TMainForm.SelectFileBtnClick(Sender: TObject);
var
  dlg: TOpenDialog;
begin
  dlg := TOpenDialog.Create(nil);
  try
    if dlg.Execute then
      FileName := dlg.FileName;
  finally
    dlg.Free;
  end;
end;

procedure TMainForm.SetFileName(const Value: string);
begin
  if FFileName <> Value then
  begin
    FFileName := Value;
    FileNameEdit.Text := Value;
    UpdateInfo;
  end;
end;

procedure TMainForm.SetTestResult(const Value: TTestResult);
const
  cResultCaption: array[TTestResult] of string = ('Result: Unknown', 'Result: Error', 'Result: OK');
  cResultColor: array[TTestResult] of TColor = (clBlack, clRed, clGreen);

begin
  if FTestResult <> Value then
  begin
    FTestResult := Value;
    ResultLbl.Caption := cResultCaption[Value];
    ResultLbl.Font.Color := cResultColor[Value];
    Application.ProcessMessages;
  end;
end;

procedure TMainForm.ShowInfo(const vInfo: string);
begin
  StatusBar.Panels[0].Text := vInfo;
  Application.ProcessMessages;
end;

procedure TMainForm.SrcEditChange(Sender: TObject);
begin
  TestCipher;
end;

procedure TMainForm.WriteDecryptFile;
var
  st: string;
  fi, fo: TFileStream;
  scs: TCipherStream;
  i: Integer;
begin
  if FileExists(EncFileName) then
  begin
    fi := TFileStream.Create(EncFileName, fmOpenRead);
    fo := TFileStream.Create(DecFileName, fmCreate or fmOpenWrite);
    try
      scs := CreateCipherStream(fo, cyDecrypt, cimWrite, false);
      try
        SetLength(st, cBufferSize);
        while True do
        begin
          i := fi.Read(st[1], cBufferSize);
          if i=0 then Break;
          scs.Write(st[1], i);
        end;
        SetLength(st, 0);
      finally
        scs.Free;
      end;
    finally
      fi.Free;
      fo.Free;
    end;
  end;
end;

procedure TMainForm.WriteEncryptFile;
var
  st: string;
  fi, fo: TFileStream;
  scs: TCipherStream;
  i: Integer;
begin
  if FileExists(FileName) then
  begin
    fi := TFileStream.Create(FileName, fmOpenRead);
    fo := TFileStream.Create(EncFileName, fmCreate or fmOpenWrite);
    try
      scs := CreateCipherStream(fo, cyEncrypt, cimWrite, false);
      try
        SetLength(st, cBufferSize);
        while True do
        begin
          i := fi.Read(st[1], cBufferSize);
          if i=0 then Break;
          scs.Write(st[1], i);
        end;
        SetLength(st, 0);
      finally
        scs.Free;
      end;
    finally
      fi.Free;
      fo.Free;
    end;
  end;
end;

procedure TMainForm.TestReadBtnClick(Sender: TObject);
var
  t: Cardinal;
begin
  if FileName<>'' then
  begin
    TestResult := trUnknown;
    t := GetTickCount;
    TestRead;
    CalcSpeed(GetTickCount-t, FileSize);
    TestResult := cTestResult[MD5Print(MD5File(FileName))=MD5Print(MD5File(DecFileName))];
  end;
end;

{ TMySnowCiphr }

procedure TMySnowCiphr.Prepare;
begin
  inherited;
  Key [0] := $80;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MethodBox.Items.AddObject('Snow2 Cipher', TObject(TSnow2CipherStream));
  MethodBox.Items.AddObject('Hex Cipher', TObject(THexCipherStream));
  MethodBox.ItemIndex := 0;

  SrcEdit.Text := 'abcd';
  FileName := 'c:\1.txt';
end;

function TMainForm.GetCipherClass: TCipherStreamClass;
begin
  if MethodBox.ItemIndex<>-1 then
    Result := TCipherStreamClass(MethodBox.Items.Objects[MethodBox.ItemIndex])
  else
    Result := nil;
end;

function TMainForm.GetDeccFileName: string;
begin
  if FileName<>'' then
    Result := ChangeFileExt(FileName, '.dec'+ExtractFileExt(FileName))
  else
    Result := '';
end;

function TMainForm.GetEncFileName: string;
begin
  if FileName<>'' then
    Result := ChangeFileExt(FileName, '.enc'+ExtractFileExt(FileName))
  else
    Result := '';
end;

function TMainForm.GetFileName: string;
begin
  if (FFileName<>'')and FileExists(FFileName) then
    Result := FFileName
  else
    Result := '';
end;

function TMainForm.GetFileSize: Cardinal;
begin
  if FileName='' then
    Result := 0
  else
    Result := _GetFileSize(FileName); 
end;

procedure TMainForm.MethodBoxClick(Sender: TObject);
begin
  TestCipher;
end;

procedure TMainForm.TestRead;
begin
  if CipherClass<>nil then
  begin
    ReadEncryptFile;
    ReadDecryptFile;
  end;
end;

procedure TMainForm.TestWrite;
begin
  if CipherClass<>nil then
  begin
    WriteEncryptFile;
    WriteDecryptFile;
  end;
end;

procedure TMainForm.TestWriteBtnClick(Sender: TObject);
var
  t: Cardinal;
begin
  if FileName<>'' then
  begin
    TestResult := trUnknown;
    t := GetTickCount;
    TestWrite;
    CalcSpeed(GetTickCount-t, FileSize);
    TestResult := cTestResult[MD5Print(MD5File(FileName))=MD5Print(MD5File(DecFileName))];
  end;
end;

procedure TMainForm.UpdateInfo;
begin
  FileNameEdit.Text := FileName;
  EncFileNameLbl.Caption := Format('Encrypt FileName: %s', [EncFileName]);
  DecFileNameLbl.Caption := Format('Decrypt FileName: %s', [DecFileName]);

  EncFileNameLbl.Visible := FileName<>'';
  DecFileNameLbl.Visible := FileName<>'';
end;

procedure TMainForm.TestCipher;
var
  st: string;
  s: TStringStream;
  scs: TCipherStream;
  i: Integer;
begin
  if CipherClass<>nil then
  begin
    s := TStringStream.Create(SrcEdit.Text);
    try
      EncEdit.Clear;
      s.Seek(0, soFromBeginning);
      scs := CreateCipherStream(s, cyEncrypt, cimRead, false);
      try
        while True do
        begin
          SetLength(st, cBufferSize);
          i := scs.read(st[1], cBufferSize);
          if i=0 then Break;
          SetLength(st, i);
          EncEdit.Text := EncEdit.Text + st;
        end;
      finally
        scs.Free;
      end;
    finally
      s.Free;
    end;

    s := TStringStream.Create(EncEdit.Text);
    try
      DecEdit.Clear;
      s.Seek(0, soFromBeginning);
      scs := CreateCipherStream(s, cyDecrypt, cimRead, false);
      try
        while True do
        begin
          SetLength(st, cBufferSize);
          i := scs.read(st[1], cBufferSize);
          if i=0 then Break;
          SetLength(st, i);
          DecEdit.Text := DecEdit.Text + st;
        end;
      finally
        scs.Free;
      end;
    finally
      s.Free;
    end;
    TestResult := cTestResult[SrcEdit.Text = DecEdit.Text];
  end;
end;

end.
