unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mvMapViewer, mvDLEFpc, mvDE_BGRA, mvDE_RGBGraphics, Forms,
  Controls, Graphics, Dialogs, ComCtrls, StdCtrls, ExtCtrls, FileCtrl, ComboEx,
  PairSplitter, uresize, utypes, ureadpipe, uaprs, mvGPSObj, RegExpr, mvTypes,
  mvEngine, RichMemo, Contnrs, uini;

type

  { TFMain }

  TFMain = class(TForm)
    CBPOIList: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    MAPRSMessage: TMemo;
    MVMap: TMapView;
    MvBGRADrawingEngine1: TMvBGRADrawingEngine;
    MVDEFPC1: TMVDEFPC;
    MAPRSMonitor: TRichMemo;
    PairSplitter1: TPairSplitter;
    PairSplitterSide2: TPairSplitterSide;
    SBMain: TStatusBar;
    STLatitude: TStaticText;
    STLatitudeDMS: TStaticText;
    STLongitude: TStaticText;
    STLongitudeDMS: TStaticText;
    TBZoomMap: TTrackBar;
    TMainLoop: TTimer;
    procedure FMainInit(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MoveMap(Sender: TObject);
    procedure SelectPOI(Sender: TObject);
    procedure ShowMapMousPosition(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TBZoomMapChange(Sender: TObject);
    procedure DecodeAPRSMessage(const Data: String);
    procedure TMainLoopTimer(Sender: TObject);
  private

  public

  end;

var
  FMain: TFMain;
  OrigWidth, OrigHeight: Integer;
  APRSMessageObject: PAPRSMessage;
  ReadPipe: TReadPipeThread;
  APRSConfig: TAPRSConfig;
  Copyright: Boolean;

implementation

{$R *.lfm}

{ TFMain }

procedure TFMain.FMainInit(Sender: TObject);
begin
  OrigWidth := Self.Width;
  OrigHeight := Self.Height;

  LoadConfigFromFile(@APRSConfig);

  TBZoomMap.Position := MVMap.Zoom;
  MVMap.CachePath := APRSConfig.MAPCache;
  StoreOriginalSizes(Self);

  APRSMessageList := TFPHashList.Create;

  Copyright := False;

  ReadPipe := TReadPipeThread.Create('flexpacketaprspipe');
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  SaveConfigToFile(@APRSConfig);
end;

procedure TFMain.FormResize(Sender: TObject);
var
  scaleFactorWidth, scaleFactorHeight, scaleFactor: Double;
  i: Integer;
begin
  scaleFactorWidth := Width / OrigWidth;
  scaleFactorHeight := Height / OrigHeight;
  scaleFactor := Min(scaleFactorWidth, scaleFactorHeight);

  for i := 0 to ControlCount - 1 do
    ResizeControl(Controls[i], scaleFactorWidth, scaleFactorHeight, scaleFactor);

end;

procedure TFMain.MoveMap(Sender: TObject);
var Area: TRealArea;
    List: TGPSObjList;
    poi: TGpsObj;
    i: Integer;
    curSel: String;
begin
  with MVMap.Canvas do
  begin
    Font.Name := 'Arial';
    Font.Size := 6;
    Font.Color := clBlack;
    TextOut(0, 0, 'Copyright OpenStreetmap');
  end;
  Copyright := True;

  MVMap.GPSItems.GetArea(Area);
  List := MVMap.GPSItems.GetObjectsInArea(Area);

  if List.Count <= 0 then
    Exit;

  curSel := '';
  if CBPOIList.ItemIndex >= 0 then
    curSel := CBPOIList.Items[CBPOIList.ItemIndex];

  CBPOIList.Clear;
  for i:=0 to List.Count - 1 do
  begin
    poi := List[i];
    if poi is TGpsPoint then
      CBPOIList.Items.Add(poi.Name);
  end;

  for i:=0 to CBPOIList.Items.Count - 1 do
  begin
    if CBPOIList.Items[i] = curSel then
      CBPOIList.ItemIndex := i;
  end;
end;

procedure TFMain.SelectPOI(Sender: TObject);
var msg: PAPRSMessage;
begin
  MAPRSMessage.Lines.Clear;
  msg := APRSMessageList.Find(CBPOIList.Items[CBPOIList.ItemIndex]);
  if msg <> nil then
  begin
    MAPRSMessage.Lines.Add(msg^.Message);
    STLatitude.Caption := LatToStr(msg^.Latitude, False);
    STLongitude.Caption := LonToStr(msg^.Longitude, False);
    STLatitudeDMS.Caption := LatToStr(msg^.Latitude, True);
    STLongitudeDMS.Caption := LonToStr(msg^.Longitude, True);
  end;
end;

procedure TFMain.ShowMapMousPosition(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  p: TRealPoint;
begin
  p := MVMap.ScreenToLonLat(Point(X, Y));
  SBMain.Panels[1].Text := 'Longitude: ' + LonToStr(P.Lon, False);
  SBMain.Panels[0].Text := 'Latitude: ' + LatToStr(P.Lat, False);
end;

procedure TFMain.TBZoomMapChange(Sender: TObject);
begin
  MVMap.Zoom := TBZoomMap.Position;
end;

procedure TFMain.DecodeAPRSMessage(const Data: String);
var Regex: TRegExpr;
    Lat, Lon: Double;
begin
  Regex := TRegExpr.Create;
   try
     //Regex.Expression := '^.*?Fm ([A-Z0-9]{1,6}(?:-[0-9]{1,2})?) To ([A-Z0-9]{1,6})(?: Via ([A-Z0-9,-]+))? .*?>\[(\d{2}:\d{2}:\d{2})\].?\s*(.+)$';
     Regex.Expression := '^.*?Fm\s(\S+)\sTo\s(\S+)\s(?:Via\s(\S+))? .*UI(?:[v]{0,1})\spid(?:[=|\s]{0,1})F0.*!(\d{4}\.\d{2}\w.*)$';
     Regex.ModifierI := False;
     if Regex.Exec(Data) then
     begin
       WriteLn('Source: ', Regex.Match[1]);
       WriteLn('Destination: ', Regex.Match[2]);
       WriteLn('Path: ', Regex.Match[3]);
       WriteLn('Payload: ', Regex.Match[4]);

       // FromCall: String;
       // ToCall: String;
       // Path: String;
       // Longitude: Double;
       // Latitude: Double;
       // Message: String;
       // Time: String

       New(APRSMessageObject);
       APRSMessageObject^.FromCall := Regex.Match[1];
       APRSMessageObject^.ToCall := Regex.Match[2];
       APRSMessageObject^.Path := Regex.Match[3];


       Regex.Expression := '^(\d{4}\.\d{2}\w)\/(\d{5}\.\d{2}\w)(\w)(.+)$';
       if Regex.Exec(Regex.Match[4]) then
       begin
         ConvertNMEAToLatLong(Regex.Match[1], Regex.Match[2], Lat, Lon);
         APRSMessageObject^.Latitude := Lat;
         APRSMessageObject^.Longitude := Lon;
         APRSMessageObject^.Message := Regex.Match[4];

           // Normalisierung der Ergebnisse
         WriteLn('Latitude: ',  LatToStr(Lat, False));
         WriteLn('Longitude: ', LonToStr(Lon, False));
         WriteLn('Type/Icon: ', Regex.Match[3]);
         WriteLn('Message: ', Regex.Match[4]);

         SetPoi(APRSMessageObject, MVMap.GPSItems);
       end;
     end;
   finally
     Regex.Free;
   end;
end;

procedure TFMain.TMainLoopTimer(Sender: TObject);
begin
  try
    MAPRSMonitor.Lines.Add(ReadPipe.PipeData);
    if MAPRSMonitor.Lines.Count > 3 then
    begin
      MAPRSMonitor.SelStart := MAPRSMonitor.GetTextLen;
      MAPRSMonitor.ScrollBy(0, MAPRSMonitor.Lines.Count);
      MAPRSMonitor.Refresh;
    end;
    DecodeAPRSMessage(ReadPipe.PipeData);
    MoveMap(Sender);
  except
  end;
end;

end.

