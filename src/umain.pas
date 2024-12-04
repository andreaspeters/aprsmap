unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mvMapViewer, mvDLEFpc, mvDE_BGRA, Forms,
  Controls, Graphics, Dialogs, ComCtrls, StdCtrls, ExtCtrls,
  PairSplitter, Menus, uresize, utypes, ureadpipe, uaprs, mvGPSObj, RegExpr,
  mvTypes, mvEngine, Contnrs, uini, uigate;

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
    MainMenu1: TMainMenu;
    MainMenu2: TMainMenu;
    MAPRSMessage: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MIFileExit1: TMenuItem;
    MvBGRADrawingEngine1: TMvBGRADrawingEngine;
    MvBGRADrawingEngine2: TMvBGRADrawingEngine;
    MvBGRADrawingEngine3: TMvBGRADrawingEngine;
    MVDEFPC1: TMVDEFPC;
    MVDEFPC2: TMVDEFPC;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Settings: TMenuItem;
    MIFileExit: TMenuItem;
    MVMap: TMapView;
    PairSplitter1: TPairSplitter;
    PairSplitterSide2: TPairSplitterSide;
    SBMain: TStatusBar;
    Settings1: TMenuItem;
    STLatitude: TStaticText;
    STLatitudeDMS: TStaticText;
    STLongitude: TStaticText;
    STLongitudeDMS: TStaticText;
    TBZoomMap: TTrackBar;
    TMainLoop: TTimer;
    TMainLoop1: TTimer;
    procedure FMainInit(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MIFileExitClick(Sender: TObject);
    procedure MVMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MVMapMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MVMapZoomChange(Sender: TObject);
    procedure SelectPOI(Sender: TObject);
    procedure ShowMapMousePosition(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TBZoomMapChange(Sender: TObject);
    procedure TMainLoopTimer(Sender: TObject);
    procedure UpdatePOIVisibility;
  private

  public

  end;

var
  FMain: TFMain;
  OrigWidth, OrigHeight: Integer;
  APRSMessageObject: PAPRSMessage;
  ReadPipe: TReadPipeThread;
  APRSConfig: TAPRSConfig;
  IGate: TIGateThread;
  PoiVisibility: Boolean;
  LastZoom: Byte;

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

  PoiVisibility := True;

  ReadPipe := TReadPipeThread.Create('flexpacketaprspipe');
  IGate := TIGateThread.Create(@APRSConfig);
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

procedure TFMain.MIFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFMain.MVMapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  PoiVisibility := False;
end;

procedure TFMain.MVMapMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  PoiVisibility := True;
end;

procedure TFMain.MVMapZoomChange(Sender: TObject);
var i: Byte;
begin
  i := MVMap.Zoom;
  if i > 20 then
    TBZoomMap.Position := 20;

  TBZoomMap.Position := i;
  if i <> LastZoom then
    LastZoom := i;
end;

procedure TFMain.UpdatePOIVisibility;
var
  i: Integer;
  areaPois: TGPSObjList;
begin
  for i := 0 to MVMap.GPSItems.Count - 1 do
  begin
    MVMap.GPSItems[i].Visible := False;
  end;

  if not PoiVisibility then
    Exit;

  areaPois := MVMap.GPSItems.GetObjectsInArea(MVMap.GetVisibleArea);
  for i:= 0 to areaPois.Count - 1 do
  begin
    areaPois.Items[i].Visible := True;
    SBMain.Panels[2].Text := '';
    if i = 4 then
    begin
      SBMain.Panels[2].Text := 'To many Data, please zoom in...';
      Break;
    end;
  end;

  MVMap.Refresh;
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

procedure TFMain.ShowMapMousePosition(Sender: TObject; Shift: TShiftState; X,
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



procedure TFMain.TMainLoopTimer(Sender: TObject);
var msg: TAPRSMessage;
    buffer: String;
begin
  UpdatePOIVisibility;
  PoiVisibility := True;
  if APRSConfig.IGateEnabled then
  begin
    try
      buffer := IGate.APRSBuffer;
      IGate.APRSBuffer := '';
      msg := IGate.DecodeAPRSMessage(buffer);

      if Length(msg.FromCall) > 0 then
      begin
        SetPoi(@msg, MVMap.GPSItems);

        //WriteLn('Source: ', Regex.Match[1]);
        //WriteLn('Destination: ', Regex.Match[2]);
        //WriteLn('Path: ', Regex.Match[3]);
        //WriteLn('Payload: ', Regex.Match[4]);
        //WriteLn('Latitude: ',  LatToStr(Lat, False));
        //WriteLn('Longitude: ', LonToStr(Lon, False));
        //WriteLn('Type/Icon: ', Regex.Match[3]);
        //WriteLn('Message: ', Regex.Match[4]);
      end;
    except
    end;
  end;

  try
    ReadPipe.DecodeAPRSMessage(ReadPipe.PipeData);
  except
  end;
end;

end.

