unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, snow2cipher, ciphers;

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
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ExecuteBtnClick(Sender: TObject);
var
  st: string;
  s: TStringStream;
  scs: TSnow2CipherStream;
begin
  s := TStringStream.Create(SrcMemo.Text);
  try
    s.Seek(0, soFromBeginning);
    scs := TMySnowCiphr.Create(s, cyEncrypt, cimRead, false);
    try
      SetLength(st, 20);
      scs.read(st[1], 20);
      EncryptMemo.Text := st;
    finally
      scs.Free;
    end;
  finally
    s.Free;
  end;

  s := TStringStream.Create(EncryptMemo.Text);
  try
    s.Seek(0, soFromBeginning);
    scs := TMySnowCiphr.Create(s, cyDecrypt, cimRead, false);
    try
      SetLength(st, 20);
      scs.read(st[1], 20);
      DecryptMemo.Text := st;
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
