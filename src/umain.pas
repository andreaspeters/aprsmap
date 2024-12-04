unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mvMapViewer, mvDLEFpc, mvDE_BGRA, Forms, Controls,
  Graphics, Dialogs, ComCtrls, StdCtrls, ExtCtrls, PairSplitter, Menus, ComboEx,
  uresize, utypes, ureadpipe, uaprs, mvGPSObj, RegExpr, mvTypes, mvEngine,
  Contnrs, uini, uigate, StrUtils;

type

  { TFMain }

  TFMain = class(TForm)
    CBEPOIList: TComboBoxEx;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ImageList1: TImageList;
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
    procedure MVMapZoomChange(Sender: TObject);
    procedure SelectPOI(Sender: TObject);
    procedure ShowMapMousePosition(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TBZoomMapChange(Sender: TObject);
    procedure TMainLoopTimer(Sender: TObject);
    procedure AddCombobox(const msg: TAPRSMessage);
  private
    function FindGPSItem(const Call: String):TGPSObj;
  public

  end;

var
  FMain: TFMain;
  OrigWidth, OrigHeight: Integer;
  APRSMessageObject: PAPRSMessage;
  ReadPipe: TReadPipeThread;
  APRSConfig: TAPRSConfig;
  IGate: TIGateThread;
  LastZoom: Byte;
  MyPosition: TGPSObj;
  PoILayer: TMapLayer;

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

  ReadPipe := TReadPipeThread.Create('flexpacketaprspipe');
  IGate := TIGateThread.Create(@APRSConfig);

  PoILayer := (MVMap.Layers.Add as TMapLayer);
  SetPoi(PoILayer, APRSConfig.Latitude, APRSConfig.Longitude, APRSConfig.Callsign, True, GetImageIndex('y'), MVMap.GPSItems);
  MyPosition := FindGPSItem(APRSConfig.Callsign);
  MVMap.CenterOnObj(MyPosition);
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

procedure TFMain.SelectPOI(Sender: TObject);
var msg: PAPRSMessage;
    i, count: Integer;
    call: String;
begin
  MAPRSMessage.Lines.Clear;

  call := Trim(SplitString(CBEPOIList.ItemsEx.Items[CBEPOIList.ItemIndex].Caption, '>')[0]);
  msg := APRSMessageList.Find(call);
  if msg <> nil then
  begin
    MAPRSMessage.Lines.Add(msg^.Message);
    STLatitude.Caption := LatToStr(msg^.Latitude, False);
    STLongitude.Caption := LonToStr(msg^.Longitude, False);
    STLatitudeDMS.Caption := LatToStr(msg^.Latitude, True);
    STLongitudeDMS.Caption := LonToStr(msg^.Longitude, True);

    MVMap.CenterOnObj(FindGPSItem(call));
  end;
end;

function TFMain.FindGPSItem(const Call: String):TGPSObj;
var i, count: Integer;
begin
  count := PoILayer.PointsOfInterest.Count;
  for i := 0 to count - 1 do
  begin
    if PoILayer.PointsOfInterest[i].Caption = Call then
    begin
      Result := PoILayer.PointsOfInterest[i].GPSObj;
      Exit;
    end;
  end;
end;

procedure TFMain.ShowMapMousePosition(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  p: TRealPoint;
begin
  p := MVMap.ScreenToLatLon(Point(X, Y));
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
    newMSG: PAPRSMessage;
begin
  if APRSConfig.IGateEnabled then
  begin
    try
      buffer := IGate.APRSBuffer;
      IGate.APRSBuffer := '';
      msg := IGate.DecodeAPRSMessage(buffer);

      if Length(msg.FromCall) > 0 then
      begin
        if APRSMessageList.Find(msg.FromCall) = nil then
        begin
          New(newMSG);
          newMSG^ := msg;

          SetPoi(PoILayer, newMsg, MVMap.GPSItems);
          AddCombobox(msg);
          APRSMessageList.Add(msg.FromCall, newMSG);
        end;
        //WriteLn('Source: ', msg.FromCall);
        //WriteLn('Destination: ', msg.ToCall);
        //WriteLn('Path: ', msg.Path);
        //WriteLn('Latitude: ',  LatToStr(msg.Latitude, False));
        //WriteLn('Longitude: ', LonToStr(msg.Longitude, False));
        //WriteLn('Message: ', msg.Message);
      end;
    except
    end;
  end;

  try
    ReadPipe.DecodeAPRSMessage(ReadPipe.PipeData);
  except
  end;
end;


procedure TFMain.AddCombobox(const msg: TAPRSMessage);
var poi: TGpsPoint;
    km: Double;
    imageIndex: Byte;
begin
  poi := TGpsPoint(FindGPSItem(msg.FromCall));
  km := poi.DistanceInKmFrom(TGpsPoint(MyPosition),False);
  imageIndex := GetImageIndex(msg.IconPrimary);
  CBEPOIList.ItemsEx.AddItem(msg.FromCall + ' > ' + IntToStr(Round(km)) + 'km' , imageIndex, 0, 0, 0, nil);
end;

end.

