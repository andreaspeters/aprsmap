unit umodeslist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Grids, ExtCtrls, StdCtrls, ComCtrls;

type
  TModeSListForm = class(TForm)
    BottomPanel: TPanel;
    CloseButton: TButton;
    DetailMemo: TMemo;
    DetailPanel: TPanel;
    EntryCountLabel: TLabel;
    GridModeS: TStringGrid;
    HeaderPanel: TPanel;
    RefreshTimer: TTimer;
    StatusBar: TStatusBar;
    TitleLabel: TLabel;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridModeSClick(Sender: TObject);
    procedure RefreshTimerTimer(Sender: TObject);
  private
    SelectedICAO: String;
    procedure RefreshGrid;
    procedure UpdateViewerStatus;
    procedure ShowDetails(const ICAO: String);
  end;

var
  FModeSList: TModeSListForm;

implementation

uses
  umain, utypes, uaeromuxdb;

{$R *.lfm}

procedure TModeSListForm.RefreshGrid;
var
  I, Row: Integer;
  Msg: PAPRSMessage;
  Altitude, Speed, Latitude, Longitude, Course, Status: String;
begin
  if GridModeS.Row > 0 then
    SelectedICAO := GridModeS.Cells[7, GridModeS.Row];
  GridModeS.Cells[0, 0] := 'Identity';
  GridModeS.Cells[1, 0] := 'Latitude';
  GridModeS.Cells[2, 0] := 'Longitude';
  GridModeS.Cells[3, 0] := 'Altitude';
  GridModeS.Cells[4, 0] := 'Speed';
  GridModeS.Cells[5, 0] := 'Course';
  GridModeS.Cells[6, 0] := 'Last update';
  GridModeS.Cells[7, 0] := 'ICAO';
  GridModeS.Cells[8, 0] := 'Status';
  GridModeS.RowCount := 1;
  UpdateViewerStatus;
  if not Assigned(FMain) or not Assigned(FMain.ModeS) or
     not Assigned(FMain.ModeS.ModeSMessageList) then Exit;
  for I := 0 to FMain.ModeS.ModeSMessageList.Count - 1 do
  begin
    Msg := PAPRSMessage(FMain.ModeS.ModeSMessageList.Items[I]);
    if not Assigned(Msg) then Continue;
    Row := GridModeS.RowCount;
    GridModeS.RowCount := Row + 1;
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
    if Assigned(Msg^.Speed) and (Msg^.Speed.Count > 0) then Course := FloatToStr(Msg^.Course)
    else Course := 'n/a';
    GridModeS.Cells[0, Row] := Msg^.FromCall;
    GridModeS.Cells[1, Row] := Latitude;
    GridModeS.Cells[2, Row] := Longitude;
    GridModeS.Cells[3, Row] := Altitude;
    GridModeS.Cells[4, Row] := Speed;
    GridModeS.Cells[5, Row] := Course;
    GridModeS.Cells[6, Row] := DateTimeToStr(Msg^.Time);
    GridModeS.Cells[7, Row] := Msg^.Checksum;
    GridModeS.Cells[8, Row] := Status;
    if SameText(SelectedICAO, Msg^.Checksum) then
      GridModeS.Row := Row;
  end;
  UpdateViewerStatus;
end;

procedure TModeSListForm.ShowDetails(const ICAO: String);
var
  Aircraft: TAeromuxAircraft;
begin
  DetailMemo.Clear;
  if ICAO = '' then Exit;
  if not LookupAeromuxAircraft(ICAO, Aircraft) then
  begin
    DetailMemo.Lines.Add('No Aeromux DB record found.');
    Exit;
  end;
  DetailMemo.Lines.Add('ICAO: ' + Aircraft.ICAO);
  DetailMemo.Lines.Add('Registration: ' + Aircraft.Registration);
  DetailMemo.Lines.Add('Country: ' + Aircraft.Country);
  DetailMemo.Lines.Add('Type: ' + Aircraft.TypeCode + ' ' + Aircraft.TypeDescription);
  DetailMemo.Lines.Add('Model: ' + Aircraft.Model);
  DetailMemo.Lines.Add('Manufacturer: ' + Aircraft.Manufacturer);
  DetailMemo.Lines.Add('Operator: ' + Aircraft.OperatorName);
  DetailMemo.Lines.Add('Operator country: ' + Aircraft.OperatorCountry);
  DetailMemo.Lines.Add('Operator callsign: ' + Aircraft.OperatorCallsign);
  DetailMemo.Lines.Add('Serial number: ' + Aircraft.SerialNumber);
  DetailMemo.Lines.Add('Year: ' + Aircraft.YearOfManufacture);
  DetailMemo.Lines.Add('ICAO class: ' + Aircraft.TypeClass);
  DetailMemo.Lines.Add('Wake turbulence: ' + Aircraft.WTC);
  if Aircraft.Military <> 0 then DetailMemo.Lines.Add('Military: yes');
end;

procedure TModeSListForm.GridModeSClick(Sender: TObject);
begin
  if (GridModeS.Row <= 0) or (GridModeS.Cells[8, GridModeS.Row] <> 'complete') then
  begin
    SelectedICAO := '';
    DetailMemo.Clear;
    Exit;
  end;
  SelectedICAO := GridModeS.Cells[7, GridModeS.Row];
  ShowDetails(SelectedICAO);
end;

procedure TModeSListForm.UpdateViewerStatus;
var
  EntryCount: Integer;
begin
  EntryCount := GridModeS.RowCount - 1;
  EntryCountLabel.Caption := IntToStr(EntryCount) + ' aircraft';
  StatusBar.SimpleText := IntToStr(EntryCount) + ' Mode-S aircraft tracked';
end;

procedure TModeSListForm.CloseButtonClick(Sender: TObject);
begin
  Hide;
end;

procedure TModeSListForm.FormShow(Sender: TObject);
begin
  DetailMemo.Clear;
  RefreshGrid;
end;

procedure TModeSListForm.RefreshTimerTimer(Sender: TObject);
begin
  RefreshGrid;
end;

end.
