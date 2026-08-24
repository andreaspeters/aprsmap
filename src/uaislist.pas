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

{$R *.lfm}

procedure TAISListForm.RefreshGrid;
begin
  Grid.Cells[0, 0] := 'MMSI';
  Grid.Cells[1, 0] := 'Ship name';
  Grid.Cells[2, 0] := 'Latitude';
  Grid.Cells[3, 0] := 'Longitude';
  Grid.Cells[4, 0] := 'Speed';
  Grid.Cells[5, 0] := 'Course';
  Grid.Cells[6, 0] := 'Heading';
  Grid.Cells[7, 0] := 'Last update';
  { The AIS list is populated when native AIS acquisition is enabled. }
  Grid.RowCount := 1;
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
