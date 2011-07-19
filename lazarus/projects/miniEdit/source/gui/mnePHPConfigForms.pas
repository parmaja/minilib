Unit mnePHPConfigForms;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TPHPConfigForm }

  TPHPConfigForm = Class(TForm)
    Button3: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    OkBtn: TButton;
    OpenDialog: TOpenDialog;
    PHPDirEdit: TEdit;
    Procedure Button3Click(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
  private
  public
  End;

Implementation

Uses
  EditorEngine, mnePHPClasses;

{$R *.lfm}

{ TPHPConfigForm }

Procedure TPHPConfigForm.Button3Click(Sender: TObject);
Var
  aFolder: String;
Begin
  aFolder := PHPDirEdit.Text;
  If SelectFolder('Select PHP directory', '', aFolder) Then
  Begin
    PHPDirEdit.Text := aFolder;
  End;
End;

Procedure TPHPConfigForm.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  If ModalResult = mrOk Then
  Begin
  End;
End;

Initialization
  Engine.Forms.Add(TPHPPerspective, TPHPConfigForm);
End.

