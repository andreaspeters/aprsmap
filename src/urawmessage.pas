unit urawmessage;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ActnList, utypes;

type

  { TFRawMessage }

  TFRawMessage = class(TForm)
    actClose: TAction;
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    mRawMessage: TMemo;
    procedure actCloseExecute(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FConfig: PTAPRSConfig;
    IsClosing: Boolean;
  public
    procedure SetConfig(Config: PTAPRSConfig);
  end;

var
  FRawMessage: TFRawMessage;

implementation

{$R *.lfm}

{ TFRawMessage }

procedure TFRawMessage.SetConfig(Config: PTAPRSConfig);
begin
  FConfig := Config;
  IsClosing := False;

  if (FConfig^.RawMessageWidth > 0) and (FConfig^.RawMessageHeight > 0) and
    (FConfig^.RawMessagePosX > 0) and (FConfig^.RawMessagePosY > 0) then
  begin
    Width  := FConfig^.RawMessageWidth;
    Height := FConfig^.RawMessageHeight;
    Left   := FConfig^.RawMessagePosX;
    Top    := FConfig^.RawMessagePosY;
  end;

  Visible := FConfig^.RawMessageVisible;
end;


procedure TFRawMessage.FormShow(Sender: TObject);
begin
  IsClosing := False;
  mRawMessage.Clear;
end;

procedure TFRawMessage.FormHide(Sender: TObject);
begin
  if not IsClosing then
  begin
    FConfig^.RawMessageVisible := False;
    FConfig^.RawMessageWidth := Width;
    FConfig^.RawMessageHeight := Height;
    FConfig^.RawMessagePosX := Left;
    FConfig^.RawMessagePosY := Top;
  end;
end;

procedure TFRawMessage.actCloseExecute(Sender: TObject);
begin
  IsClosing := True;
  close;
end;

end.

