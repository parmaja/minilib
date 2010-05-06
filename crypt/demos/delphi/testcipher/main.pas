unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, snow2cipher, ciphers, hexcipher, ComCtrls, md5, ZLib;

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
    LogBox: TListBox;
    TestZLibBtn: TButton;
    TestZLibBufferBtn: TButton;
    TestExCipherBtn: TButton;
    TestExWriteBtn: TButton;
    TextExReadBtn: TButton;
    procedure TestReadBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TestWriteBtnClick(Sender: TObject);
    procedure SelectFileBtnClick(Sender: TObject);
    procedure SrcEditChange(Sender: TObject);
    procedure MethodBoxClick(Sender: TObject);
    procedure TestZLibBtnClick(Sender: TObject);
    procedure TestZLibBufferBtnClick(Sender: TObject);
    procedure TestExCipherBtnClick(Sender: TObject);
    procedure TestExWriteBtnClick(Sender: TObject);
    procedure TextExReadBtnClick(Sender: TObject);
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
    function CreateExCipherStream(AStream: TStream; Way: TCipherWay; Mode: TCipherMode; Owned: Boolean = True): TExCipherStream;

    procedure TestCipher;
    procedure TestExCipher;

    procedure TestRead;
    procedure TestExRead;

    procedure TestWrite;
    procedure TestExWrite;

    procedure ReadEncryptFile;
    procedure ReadDecryptFile;

    procedure ReadExEncryptFile;
    procedure ReadExDecryptFile;

    procedure WriteEncryptFile;
    procedure WriteDecryptFile;
    procedure WriteExEncryptFile;
    procedure WriteExDecryptFile;
    procedure UpdateInfo;

    property CipherClass: TCipherStreamClass read GetCipherClass;
    property FileName: string read GetFileName write SetFileName;
    property FileSize: Cardinal read GetFileSize;
    property EncFileName: string read GetEncFileName;
    property DecFileName: string read GetDeccFileName;
    property TestResult: TTestResult read FTestResult write SetTestResult;
    procedure CalcSpeed(vTime, vSize: Cardinal; vEncrypt: Boolean=False);
    procedure ShowInfo(const vInfo: string);
    procedure AddLog(const vLog: string);
  end;

const
  cTestResult: array[Boolean] of TTestResult = (trError, trOK);

var
  MainForm: TMainForm;

implementation

uses
  zlibcipher;

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

procedure TMainForm.AddLog(const vLog: string);
var
  idx: Integer;
begin
  idx := LogBox.Items.Add(vLog);
  //LogBox.TopIndex := idx-3;
  LogBox.Selected[idx] := True;
end;

procedure TMainForm.CalcSpeed(vTime, vSize: Cardinal; vEncrypt: Boolean);
const
  cEncName: array[Boolean] of string = ('Decrypt ', 'Encrypt ');
var
  s: string;
  sp, sz: Double;
begin
  sz := vSize / (1024*1024);
  if vTime=0 then
    s := Format(cEncName[vEncrypt]+'Time: 0 ms Size: %.4g MB Speed: ~ MB/S ', [sz])
  else
  begin
    sp := vSize / (vTime*1024*1024/1000);
    s := Format(cEncName[vEncrypt]+'Time: %d ms Size: %.4g MB Speed: %f MB/S ', [vTime, sz, sp]);
  end;
  AddLog(s);
end;

function TMainForm.CreateCipherStream(AStream: TStream; Way: TCipherWay; Mode: TCipherMode; Owned: Boolean): TCipherStream;
begin
  if CipherClass<>nil then
    Result := CipherClass.Create(AStream, Way, Mode, Owned)
  else
    Result := nil;
  //Result := THexCipherStream.Create(AStream, Way, Mode, Owned);
end;

function TMainForm.CreateExCipherStream(AStream: TStream; Way: TCipherWay; Mode: TCipherMode; Owned: Boolean): TExCipherStream;
begin
  Result := THexExCipherStream.Create(AStream, Way, Mode, Owned)
  //Result := TSnow2ExCipherStream.Create(AStream, Way, Mode, Owned)
  //Result := THexExCipherStream.Create(AStream, Way, Mode, Owned);
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

procedure TMainForm.ReadExDecryptFile;
var
  st: string;
  fi, fo: TFileStream;
  scs: TExCipherStream;
  i: Integer;
begin
  if FileExists(EncFileName) then
  begin
    fi := TFileStream.Create(EncFileName, fmOpenRead);
    fo := TFileStream.Create(DecFileName, fmCreate or fmOpenWrite);
    try
      scs := CreateExCipherStream(fi, cyDecrypt, cimRead, false);
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

procedure TMainForm.ReadExEncryptFile;
var
  st: string;
  fi, fo: TFileStream;
  scs: TExCipherStream;
  i: Integer;
begin
  if FileName<>'' then
  begin
    fi := TFileStream.Create(FileName, fmOpenRead);
    fo := TFileStream.Create(EncFileName, fmCreate or fmOpenWrite);
    try
      scs := CreateExCipherStream(fi, cyEncrypt, cimRead, false);
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
    //ResultLbl.Caption := cResultCaption[Value];
    //ResultLbl.Font.Color := cResultColor[Value];
    if FTestResult=trUnknown then
      LogBox.Clear;
    ShowInfo(cResultCaption[Value]);
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

procedure TMainForm.WriteExDecryptFile;
var
  st: string;
  fi, fo: TFileStream;
  scs: TExCipherStream;
  i: Integer;
begin
  if FileExists(EncFileName) then
  begin
    fi := TFileStream.Create(EncFileName, fmOpenRead);
    fo := TFileStream.Create(DecFileName, fmCreate or fmOpenWrite);
    try
      scs := CreateExCipherStream(fo, cyDecrypt, cimWrite, false);
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

procedure TMainForm.WriteExEncryptFile;
var
  st: string;
  fi, fo: TFileStream;
  scs: TExCipherStream;
  i: Integer;
begin
  if FileExists(FileName) then
  begin
    fi := TFileStream.Create(FileName, fmOpenRead);
    fo := TFileStream.Create(EncFileName, fmCreate or fmOpenWrite);
    try
      scs := CreateExCipherStream(fo, cyEncrypt, cimWrite, false);
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
begin
  if FileName<>'' then
  begin
    TestResult := trUnknown;
    TestRead;
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
  MethodBox.Items.AddObject('ZLib Cipher', TObject(TZLibCipherStream));
  MethodBox.ItemIndex := 0;

  SrcEdit.Text := 'abcdefghijklmnopqrsstuvwxyzabcdef';
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
var
  t: Cardinal;
begin
  if CipherClass<>nil then
  begin
    t := GetTickCount;
    ReadEncryptFile;
    CalcSpeed(GetTickCount-t, FileSize, True);

    t := GetTickCount;
    ReadDecryptFile;
    CalcSpeed(GetTickCount-t, FileSize);
  end;
end;

procedure TMainForm.TestExRead;
var
  t: Cardinal;
begin
  if CipherClass<>nil then
  begin
    t := GetTickCount;
    ReadExEncryptFile;
    CalcSpeed(GetTickCount-t, FileSize, True);

    t := GetTickCount;
    ReadExDecryptFile;
    CalcSpeed(GetTickCount-t, FileSize);
  end;
end;

procedure TMainForm.TestWrite;
var
  t: Cardinal;
begin
  if CipherClass<>nil then
  begin
    t := GetTickCount;
    WriteEncryptFile;
    CalcSpeed(GetTickCount-t, FileSize, True);

    t := GetTickCount;
    WriteDecryptFile;
    CalcSpeed(GetTickCount-t, FileSize);
  end;
end;

procedure TMainForm.TestExWrite;
var
  t: Cardinal;
begin
  if CipherClass<>nil then
  begin
    t := GetTickCount;
    WriteExEncryptFile;
    CalcSpeed(GetTickCount-t, FileSize, True);

    t := GetTickCount;
    WriteExDecryptFile;
    CalcSpeed(GetTickCount-t, FileSize);
  end;
end;

procedure TMainForm.TestWriteBtnClick(Sender: TObject);
begin
  if FileName<>'' then
  begin
    TestResult := trUnknown;
    TestWrite;
    TestResult := cTestResult[MD5Print(MD5File(FileName))=MD5Print(MD5File(DecFileName))];
  end;
end;

procedure TMainForm.TestExWriteBtnClick(Sender: TObject);
begin
  if FileName<>'' then
  begin
    TestResult := trUnknown;
    TestExWrite;
    TestResult := cTestResult[MD5Print(MD5File(FileName))=MD5Print(MD5File(DecFileName))];
  end;
end;

procedure TMainForm.TestZLibBtnClick(Sender: TObject);
var
  st: string;
  fi, fo: TFileStream;
  cos: TCompressionStream;
  des: TDecompressionStream;
  i: Integer;
begin
  if FileExists(FileName) then
  begin
    fi := TFileStream.Create(FileName, fmOpenRead);
    fo := TFileStream.Create(EncFileName, fmCreate or fmOpenWrite);
    try
      cos := TCompressionStream.Create(clMax, fo);
      try
        SetLength(st, cBufferSize);
        while True do
        begin
          i := fi.read(st[1], cBufferSize);
          if i=0 then Break;
          cos.Write(st[1], i);
          if i<cBufferSize then Break;
        end;
        SetLength(st, 0);
      finally
        cos.Free;
      end;
    finally
      fi.Free;
      fo.Free;
    end;
  end;

  if FileExists(EncFileName) then
  begin
    fi := TFileStream.Create(EncFileName, fmOpenRead);
    fo := TFileStream.Create(DecFileName, fmCreate or fmOpenWrite);
    try
      des := TDecompressionStream.Create(fi);
      try
        SetLength(st, cBufferSize);
        while True do
        begin
          i := des.read(st[1], cBufferSize);
          if i=0 then Break;
          fo.Write(st[1], i);
          if i<cBufferSize then Break;
        end;
        SetLength(st, 0);
      finally
        des.Free;
      end;
    finally
      fi.Free;
      fo.Free;
    end;
  end;
end;

procedure TMainForm.TestZLibBufferBtnClick(Sender: TObject);
var
  st: string;
  fi, fo: TFileStream;
  des: TDecompressionStream;
  i: Integer;

  CompressedBytesRead: Integer;
  OutBuff: Pointer;
begin
  if FileExists(FileName) then
  begin
    fi := TFileStream.Create(FileName, fmOpenRead);
    fo := TFileStream.Create(EncFileName, fmCreate or fmOpenWrite);
    try
      SetLength(st, cBufferSize);
      while True do
      begin
        i := fi.read(st[1], cBufferSize);
        if i=0 then Break;
        try
          OutBuff := nil;
          CompressBuf(PChar(st), i, OutBuff, CompressedBytesRead);
          fo.Write(OutBuff, CompressedBytesRead);
        finally
          FreeMem(OutBuff);
        end;
        if i<cBufferSize then Break;
      end;
      SetLength(st, 0);
    finally
      fi.Free;
      fo.Free;
    end;
  end;

  if FileExists(EncFileName) then
  begin
    fi := TFileStream.Create(EncFileName, fmOpenRead);
    fo := TFileStream.Create(DecFileName, fmCreate or fmOpenWrite);
    try
      des := TDecompressionStream.Create(fi);
      try
        SetLength(st, cBufferSize);
        while True do
        begin
          i := des.read(st[1], cBufferSize);
          if i=0 then Break;
          fo.Write(st[1], i);
          if i<cBufferSize then Break;
        end;
        SetLength(st, 0);
      finally
        des.Free;
      end;
    finally
      fi.Free;
      fo.Free;
    end;
  end;
end;

procedure TMainForm.TextExReadBtnClick(Sender: TObject);
begin
  if FileName<>'' then
  begin
    TestResult := trUnknown;
    TestExRead;
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

procedure TMainForm.TestExCipher;
var
  st: string;
  s: TStringStream;
  scs: TExCipherStream;
  i: Integer;
begin
  //if ExCipherClass<>nil then
  begin
    s := TStringStream.Create(SrcEdit.Text);
    try
      EncEdit.Clear;
      s.Seek(0, soFromBeginning);
      scs := CreateExCipherStream(s, cyEncrypt, cimRead, false);
      try
        while True do
        begin
          SetLength(st, cBufferSize);
          i := scs.read(st[1], cBufferSize);
          SetLength(st, i);
          EncEdit.Text := EncEdit.Text + st;
          if i=0 then Break;
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
      scs := CreateExCipherStream(s, cyDecrypt, cimRead, false);
      try
        while True do
        begin
          SetLength(st, cBufferSize);
          i := scs.read(st[1], cBufferSize);
          SetLength(st, i);
          DecEdit.Text := DecEdit.Text + st;
          if i=0 then Break;
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

procedure TMainForm.TestExCipherBtnClick(Sender: TObject);
begin
  TestExCipher;
end;

end.
