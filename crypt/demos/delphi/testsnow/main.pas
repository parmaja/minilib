unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, snow2cipher, ciphers, hexcipher;

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
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

function TMainForm.CreateCipherStraem(AStream: TStream; Way: TCipherWay; Mode: TCipherMode; Owned: Boolean): TCipherStream;
begin
  Result := THexCipherStream.Create(AStream, Way, Mode, Owned);
end;

procedure TMainForm.ExecuteBtnClick(Sender: TObject);
var
  st: string;
  s: TStringStream;
  scs: TCipherStream;
  i: Integer;
begin
  EncryptMemo.Clear;
  DecryptMemo.Clear;
  
  s := TStringStream.Create(SrcMemo.Text);
  try
    s.Seek(0, soFromBeginning);
    scs := CreateCipherStraem(s, cyEncrypt, cimRead, false);
    try
      while True do
      begin
        SetLength(st, 20);
        i := scs.read(st[1], 20);
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
    s.Seek(0, soFromBeginning);
    scs := CreateCipherStraem(s, cyDecrypt, cimRead, false);
    try
      while True do
      begin
        SetLength(st, 20);
        i := scs.read(st[1], 20);
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

end.
