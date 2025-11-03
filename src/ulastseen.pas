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
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    procedure actCloseWindowExecute(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure lvCallsignsDblClick(Sender: TObject);
  private
    FConfig: PTAPRSConfig;
    IsClosing: Boolean;
    function IsInList(Callsign: AnsiString): Boolean;
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
    if not IsInList(msg^.FromCall) then
    begin
      item := lvCallsigns.Items.Insert(0);
      item.ImageIndex := msg^.ImageIndex;
      item.Caption := msg^.FromCall;
      item.SubItems.Add(IntToStr(Round(msg^.Distance)));

      // maxx 100 callsigns
      while lvCallsigns.Items.Count > MAX_ITEMS do
        lvCallsigns.Items.Delete(lvCallsigns.Items.Count - 1);
    end;
end;

procedure TFLastSeen.lvCallsignsDblClick(Sender: TObject);
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


function TFLastSeen.IsInList(Callsign: AnsiString): Boolean;
var i: Integer;
begin
  Result := False;
  for i:= 0 to lvCallsigns.Items.Count - 1 do
    if Pos(lvCallsigns.Items[i].Caption, Callsign) > 0 then
    begin
      Result := True;
      Exit;
    end;
end;

end.

