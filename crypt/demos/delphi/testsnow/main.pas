unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, snow2cipher, ciphers, hexcipher;

const
  cBufferSize = 1024;

type

  TMySnowCiphr = class(TSnow2CipherStream)
  protected
    procedure Prepare; override;
  end;

  TMainForm = class(TForm)
    SrcMemo: TMemo;
    DecryptMemo: TMemo;
    EncryptMemo: TMemo;
    ExecuteBtn: TButton;
    procedure ExecuteBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function CreateCipherStraem(AStream: TStream; Way: TCipherWay; Mode: TCipherMode; Owned: Boolean = True): TCipherStream;
    procedure TestCipher;
    procedure Test2Cipher;
    procedure EncryptFile;
    procedure DecryptFile;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

function TMainForm.CreateCipherStraem(AStream: TStream; Way: TCipherWay; Mode: TCipherMode; Owned: Boolean): TCipherStream;
begin
  //Result := TMySnowCiphr.Create(AStream, Way, Mode, Owned);
  Result := THexCipherStream.Create(AStream, Way, Mode, Owned);
end;

procedure TMainForm.DecryptFile;
var
  st: string;
  fi, fo: TFileStream;
  scs: TCipherStream;
  i: Integer;
begin
  fi := TFileStream.Create('c:\2.txt', fmOpenRead);
  fo := TFileStream.Create('c:\3.txt', fmCreate or fmOpenWrite);
  try
    scs := CreateCipherStraem(fi, cyDecrypt, cimRead, false);
    try
      while True do
      begin
        SetLength(st, cBufferSize);
        i := scs.read(st[1], cBufferSize);
        if i=0 then Break;
        SetLength(st, i);
        fo.Write(st[1], i);
      end;
    finally
      scs.Free;
    end;
  finally
    fi.Free;
    fo.Free;
  end;
end;

procedure TMainForm.EncryptFile;
var
  st: string;
  fi, fo: TFileStream;
  scs: TCipherStream;
  i: Integer;
begin
  fi := TFileStream.Create('c:\1.txt', fmOpenRead);
  fo := TFileStream.Create('c:\2.txt', fmCreate or fmOpenWrite);
  try
    scs := CreateCipherStraem(fi, cyEncrypt, cimRead, false);
    try
      while True do
      begin
        SetLength(st, cBufferSize);
        i := scs.read(st[1], cBufferSize);
        if i=0 then Break;
        SetLength(st, i);
        fo.Write(st[1], i);
      end;
    finally
      scs.Free;
    end;
  finally
    fi.Free;
    fo.Free;
  end;
end;

procedure TMainForm.ExecuteBtnClick(Sender: TObject);
var
  t: Cardinal;
  i: Integer;
begin
  t := GetTickCount;
  Test2Cipher;
  ShowMessage(IntToStr(GetTickCount-t));
end;

{ TMySnowCiphr }

procedure TMySnowCiphr.Prepare;
begin
  inherited;
  Key [0] := $80;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SrcMemo.Text := 'abcd';
end;

procedure TMainForm.Test2Cipher;
begin
  EncryptFile;
  DecryptFile;
end;

procedure TMainForm.TestCipher;
var
  st: string;
  s: TStringStream;
  scs: TCipherStream;
  i: Integer;
begin

  s := TStringStream.Create(SrcMemo.Text);
  try
    EncryptMemo.Clear;
    s.Seek(0, soFromBeginning);
    scs := CreateCipherStraem(s, cyEncrypt, cimRead, false);
    try
      while True do
      begin
        SetLength(st, cBufferSize);
        i := scs.read(st[1], cBufferSize);
        if i=0 then Break;
        SetLength(st, i);
        EncryptMemo.Text := EncryptMemo.Text + st;
      end;
    finally
      scs.Free;
    end;
  finally
    s.Free;
  end;

  s := TStringStream.Create(EncryptMemo.Text);
  try
    DecryptMemo.Clear;
    s.Seek(0, soFromBeginning);
    scs := CreateCipherStraem(s, cyDecrypt, cimRead, false);
    try
      while True do
      begin
        SetLength(st, cBufferSize);
        i := scs.read(st[1], cBufferSize);
        if i=0 then Break;
        SetLength(st, i);
        DecryptMemo.Text := DecryptMemo.Text + st;
      end;
    finally
      scs.Free;
    end;
  finally
    s.Free;
  end;
end;

end.
