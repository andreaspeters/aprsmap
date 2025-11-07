unit ulastseen;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ShellCtrls,
  ComCtrls, Menus, ActnList, utypes;

const
  MAX_ITEMS = 100;

type

  { TFLastSeen }

  TFLastSeen = class(TForm)
    actCloseWindow: TAction;
    ActionList1: TActionList;
    lvCallsigns: TListView;
    MainMenu1: TMainMenu;
    MenuItem3: TMenuItem;
    procedure actCloseWindowExecute(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure lvCallsignClick(Sender: TObject);
  private
    FConfig: PTAPRSConfig;
    IsClosing: Boolean;
  public
    procedure AddCallsign(const msg: PAPRSMessage);
    procedure SetConfig(Config: PTAPRSConfig);
  end;

var
  FLastSeen: TFLastSeen;


implementation

uses
  umain;

{$R *.lfm}


procedure TFLastSeen.SetConfig(Config: PTAPRSConfig);
begin
  FConfig := Config;
  IsClosing := False;

  if (FConfig^.LastSeenWidth > 0) and (FConfig^.LastSeenHeight > 0) and
    (FConfig^.LastSeenPosX > 0) and (FConfig^.LastSeenPosY > 0) then
  begin
    Width := FConfig^.LastSeenWidth;
    Height := FConfig^.LastSeenHeight;
    Left := FConfig^.LastSeenPosX;
    Top := FConfig^.LastSeenPosY;
  end;

  Visible := FConfig^.LastSeenVisible;
end;

procedure TFLastSeen.AddCallsign(const msg: PAPRSMessage);
var item: TListItem;
begin
  if not Assigned(msg) then
    Exit;

  if (Length(msg^.FromCall) <= 0) then
    Exit;

  if Length(msg^.FromCall) > 0 then
  begin
    item := lvCallsigns.Items.Insert(0);
    item.ImageIndex := msg^.ImageIndex;
    item.Caption := msg^.FromCall;
    item.SubItems.Add(IntToStr(Round(msg^.Distance)));
    item.SubItems.Add(IntToStr(msg^.Count));

    // max 100 callsigns
    while lvCallsigns.Items.Count > MAX_ITEMS do
      lvCallsigns.Items.Delete(lvCallsigns.Items.Count - 1);
  end;
end;

procedure TFLastSeen.lvCallsignClick(Sender: TObject);
begin
  FMain.SelectPOI(Sender);
end;

procedure TFLastSeen.actCloseWindowExecute(Sender: TObject);
begin
  IsClosing := True;
  close;
end;

procedure TFLastSeen.FormHide(Sender: TObject);
begin
  if not IsClosing then
  begin
    FConfig^.LastSeenVisible := False;
    FConfig^.LastSeenWidth := Width;
    FConfig^.LastSeenHeight := Height;
    FConfig^.LastSeenPosX := Left;
    FConfig^.LastSeenPosY := Top;
  end;
end;

end.

