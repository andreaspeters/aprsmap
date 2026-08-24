unit umodeslist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Grids, ExtCtrls;

type
  TModeSListForm = class(TForm)
    Grid: TStringGrid;
    RefreshTimer: TTimer;
    procedure FormShow(Sender: TObject);
    procedure RefreshTimerTimer(Sender: TObject);
  private
    procedure RefreshGrid;
  end;

var
  FModeSList: TModeSListForm;

implementation

uses
  umain, utypes;

{$R *.lfm}

procedure TModeSListForm.RefreshGrid;
var
  I, Row: Integer;
  Msg: PAPRSMessage;
  Altitude, Speed, Latitude, Longitude, Course, Status: String;
begin
  Grid.Cells[0, 0] := 'Identity';
  Grid.Cells[1, 0] := 'Latitude';
  Grid.Cells[2, 0] := 'Longitude';
  Grid.Cells[3, 0] := 'Altitude';
  Grid.Cells[4, 0] := 'Speed';
  Grid.Cells[5, 0] := 'Course';
  Grid.Cells[6, 0] := 'Last update';
  Grid.Cells[7, 0] := 'ICAO';
  Grid.Cells[8, 0] := 'Status';
  Grid.RowCount := 1;
  if not Assigned(FMain) or not Assigned(FMain.ModeS) or
     not Assigned(FMain.ModeS.ModeSMessageList) then Exit;
  for I := 0 to FMain.ModeS.ModeSMessageList.Count - 1 do
  begin
    Msg := PAPRSMessage(FMain.ModeS.ModeSMessageList.Items[I]);
    if not Assigned(Msg) then Continue;
    Row := Grid.RowCount;
    Grid.RowCount := Row + 1;
    if Assigned(Msg^.Altitude) and (Msg^.Altitude.Count > 0) then
      Altitude := FloatToStr(Msg^.Altitude[Msg^.Altitude.Count - 1]) else Altitude := 'n/a';
    if Assigned(Msg^.Speed) and (Msg^.Speed.Count > 0) then
      Speed := FloatToStr(Msg^.Speed[Msg^.Speed.Count - 1]) else Speed := 'n/a';
    if Msg^.ModeSPositionValid then
    begin
      Latitude := FloatToStr(Msg^.Latitude);
      Longitude := FloatToStr(Msg^.Longitude);
      Status := 'complete';
    end
    else
    begin
      Latitude := 'n/a';
      Longitude := 'n/a';
      Status := 'incomplete';
    end;
    if Assigned(Msg^.Altitude) and (Msg^.Altitude.Count > 0) then
      Altitude := FloatToStr(Msg^.Altitude[Msg^.Altitude.Count - 1]) else Altitude := 'n/a';
    if Msg^.ModeSPositionValid then Course := FloatToStr(Msg^.Course)
    else Course := 'n/a';
    Grid.Cells[0, Row] := Msg^.FromCall;
    Grid.Cells[1, Row] := Latitude;
    Grid.Cells[2, Row] := Longitude;
    Grid.Cells[3, Row] := Altitude;
    Grid.Cells[4, Row] := Speed;
    Grid.Cells[5, Row] := Course;
    Grid.Cells[6, Row] := DateTimeToStr(Msg^.Time);
    Grid.Cells[7, Row] := Msg^.Checksum;
    Grid.Cells[8, Row] := Status;
  end;
end;

procedure TModeSListForm.FormShow(Sender: TObject);
begin
  RefreshGrid;
end;

procedure TModeSListForm.RefreshTimerTimer(Sender: TObject);
begin
  RefreshGrid;
end;

end.
