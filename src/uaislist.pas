unit uaislist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Grids, ExtCtrls;

type
  TAISListForm = class(TForm)
    Grid: TStringGrid;
    RefreshTimer: TTimer;
    procedure FormShow(Sender: TObject);
    procedure RefreshTimerTimer(Sender: TObject);
  private
    procedure RefreshGrid;
  end;

var
  FAISList: TAISListForm;

implementation

uses
  umain, umodes, utypes;

{$R *.lfm}

procedure TAISListForm.RefreshGrid;
var
  I, Row: Integer;
  Msg: PAPRSMessage;
begin
  Grid.Cells[0, 0] := 'MMSI';
  Grid.Cells[1, 0] := 'Ship name';
  Grid.Cells[2, 0] := 'Latitude';
  Grid.Cells[3, 0] := 'Longitude';
  Grid.Cells[4, 0] := 'Speed';
  Grid.Cells[5, 0] := 'Course';
  Grid.Cells[6, 0] := 'Heading';
  Grid.Cells[7, 0] := 'Last update';
  Grid.RowCount := 1;
  if not Assigned(FMain) or not Assigned(FMain.ModeS) or
     not Assigned(FMain.ModeS.AISMessageList) then Exit;
  for I := 0 to FMain.ModeS.AISMessageList.Count - 1 do
  begin
    Msg := PAPRSMessage(FMain.ModeS.AISMessageList.Items[I]);
    if not Assigned(Msg) then Continue;
    Row := Grid.RowCount;
    Grid.RowCount := Row + 1;
    Grid.Cells[0, Row] := Msg^.Checksum;
    Grid.Cells[1, Row] := Msg^.FromCall;
    if Msg^.AISPositionValid then
    begin
      Grid.Cells[2, Row] := FloatToStr(Msg^.Latitude);
      Grid.Cells[3, Row] := FloatToStr(Msg^.Longitude);
    end
    else
    begin
      Grid.Cells[2, Row] := 'n/a';
      Grid.Cells[3, Row] := 'n/a';
    end;
    if Assigned(Msg^.Speed) and (Msg^.Speed.Count > 0) then
      Grid.Cells[4, Row] := FloatToStr(Msg^.Speed[Msg^.Speed.Count - 1])
    else
      Grid.Cells[4, Row] := 'n/a';
    if Msg^.AISPositionValid then
    begin
      Grid.Cells[5, Row] := FloatToStr(Msg^.Course);
      if Msg^.AISHeading < 511 then
        Grid.Cells[6, Row] := IntToStr(Msg^.AISHeading)
      else
        Grid.Cells[6, Row] := 'n/a';
    end
    else
    begin
      Grid.Cells[5, Row] := 'n/a';
      Grid.Cells[6, Row] := 'n/a';
    end;
    Grid.Cells[7, Row] := DateTimeToStr(Msg^.Time);
  end;
end;

procedure TAISListForm.FormShow(Sender: TObject);
begin
  RefreshGrid;
end;

procedure TAISListForm.RefreshTimerTimer(Sender: TObject);
begin
  RefreshGrid;
end;

end.
