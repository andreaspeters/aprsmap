unit ueditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, Buttons,
  ComCtrls, ActnList, ExtCtrls, StdCtrls, SynEdit, utypes, SynEditKeyCmds,
  SynEditTypes, SynBeautifier, LCLType;

type

  { TTFEditor }

  TTFEditor = class(TForm)
    actClose: TAction;
    actSignature: TAction;
    actOpenFile: TAction;
    actSaveAs: TAction;
    actSend: TAction;
    actNew: TAction;
    ActionList1: TActionList;
    BPDefaultButtons: TButtonPanel;
    imgListSmall: TImageList;
    Label1: TLabel;
    lMaxMsgSize: TLabel;
    leToCall: TLabeledEdit;
    odOpenFile: TOpenDialog;
    Panel1: TPanel;
    rbMTypeMessage: TRadioButton;
    rbMTypeBulletin: TRadioButton;
    sdSaveFile: TSaveDialog;
    SEMessage: TSynEdit;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton5: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    procedure actCloseExecute(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure rbMTypeBulletinChange(Sender: TObject);
    procedure SEMessageStatusChange(Sender: TObject; Changes: TSynStatusChanges
      );
  private

  public
    procedure SetConfig(Config: PAPRSConfig);
  end;

var
  TFEditor: TTFEditor;
  FPConfig: PAPRSConfig;

implementation

uses umain;

{$R *.lfm}

procedure TTFEditor.OKButtonClick(Sender: TObject);
var i: Integer;
    Line, msg: String;
begin
  if Length(SEMessage.Text) > 67 then
  begin
    ShowMessage('Text can not be longer then 67 chars.');
    Exit;
  end;
  if Length(SEMessage.Text) <= 0 then
    Exit;

  for i := 0 to SEMessage.Lines.Count - 1 do
  begin
    Line := SEMessage.Lines[i];
    Line := StringReplace(Line, #13#10, #10, [rfReplaceAll]);  // Windows → Unix
    Line := StringReplace(Line, #13, #10, [rfReplaceAll]);     // Mac Classic → Unix
    Line := StringReplace(Line, #10, #13#10, [rfReplaceAll]);  // Unix → systemabhängig

    // Empy line need CRLF
    if (Line = '') then
      Line := ' ';

    if rbMTypeMessage.Checked then
      msg := Format(':%-9.9s:%s{%d', [leToCall.Caption, Line, Random(10000)]);

    if rbMTypeBulletin.Checked then
      msg := Format(':BLN%d     :%s', [leToCall.Caption, Random(10), Line]);

    if Length(msg) > 0 then
      FMain.SendStringCommand(APRSConfig.Channel, 0, msg);
  end;
  Close;
end;

procedure TTFEditor.rbMTypeBulletinChange(Sender: TObject);
begin
  leToCall.Enabled := not(rbMTypeBulletin.Checked);
end;

procedure TTFEditor.SEMessageStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
var i,a:Integer;
begin
  a := Length(SEMessage.Text);
  i := 67 - a;

  if i < 0 then
    lMaxMsgSize.Font.Color := clRed
  else
    lMaxMsgSize.Font.Color := clBlack;

  lMaxMsgSize.Caption := IntToStr(i);
end;

procedure TTFEditor.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TTFEditor.FormShow(Sender: TObject);
begin
  SEMessage.Text := '';
end;

procedure TTFEditor.actNewExecute(Sender: TObject);
begin
  SEMessage.Text := '';
end;

procedure TTFEditor.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TTFEditor.actOpenFileExecute(Sender: TObject);
begin
  if odOpenFile.Execute then
  begin
    SEMessage.Lines.LoadFromFile(odOpenFile.FileName);
    SEMessage.Text := SEMessage.Text;
  end;
end;

procedure TTFEditor.actSaveAsExecute(Sender: TObject);
begin

  if sdSaveFile.Execute then
    SEMessage.Lines.SaveToFile(sdSaveFile.FileName);
end;

procedure TTFEditor.SetConfig(Config: PAPRSConfig);
var Beauty: TSynBeautifier;
begin
  FPConfig := Config;

  Beauty := TSynBeautifier.Create(Self);
  Beauty.IndentType := sbitConvertToTabOnly;
  Beauty.AutoIndent := True;

  SEMessage.Beautifier := Beauty;
  SEMessage.BlockIndent := 0;
  SEMessage.BlockTabIndent := 0;
  SEMessage.DoubleBuffered := DoubleBuffered;
  SEMessage.Options := [eoBracketHighlight,eoGroupUndo,eoScrollPastEol,eoTrimTrailingSpaces];
  SEMessage.Options2 := [eoFoldedCopyPaste,eoOverwriteBlock,eoAcceptDragDropEditing];
end;

end.

