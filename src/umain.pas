unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mvMapViewer, mvDLEFpc, mvDE_BGRA, Forms, Controls,
  Graphics, Dialogs, ComCtrls, StdCtrls, ExtCtrls, Menus, ComboEx,
  uresize, utypes, ureadpipe, uaprs, mvGPSObj, RegExpr, mvTypes, mvEngine,
  Contnrs, uini, uigate, StrUtils, usettings, LCLIntf, uinfo;

type

  { TFMain }

  TFMain = class(TForm)
    CBEPOIList: TComboBoxEx;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MainMenu1: TMainMenu;
    MainMenu2: TMainMenu;
    MAPRSMessage: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MIInfo: TMenuItem;
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
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Settings: TMenuItem;
    MIFileExit: TMenuItem;
    MVMap: TMapView;
    SBMain: TStatusBar;
    Settings1: TMenuItem;
    STAltitude: TStaticText;
    StaticText1: TStaticText;
    STWXLum: TStaticText;
    STWXHumidity: TStaticText;
    STWXRainCount: TStaticText;
    STWXRainFall24h: TStaticText;
    STWXRainFallToday: TStaticText;
    STWXRainFall1h: TStaticText;
    STWXPressure: TStaticText;
    STWXSnowFall24h: TStaticText;
    STWXTemperature: TStaticText;
    STWXGust: TStaticText;
    STWXSpeed: TStaticText;
    STWXDirection: TStaticText;
    STRNGRange: TStaticText;
    STDFSDirectivity: TStaticText;
    STDFSHeight: TStaticText;
    STDFSGain: TStaticText;
    STStrength: TStaticText;
    STDirectivity: TStaticText;
    STGain: TStaticText;
    STHeight: TStaticText;
    STPower: TStaticText;
    STSpeed: TStaticText;
    STCourse: TStaticText;
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
    procedure MIInfoClick(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MIFileExitClick(Sender: TObject);
    procedure MVMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MVMapZoomChange(Sender: TObject);
    procedure SelectPOI(Sender: TObject);
    procedure SettingsClick(Sender: TObject);
    procedure ShowMapMousePosition(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TBZoomMapChange(Sender: TObject);
    procedure TMainLoopTimer(Sender: TObject);
    procedure AddCombobox(const msg: TAPRSMessage);
    procedure DeleteCombobox(const Call: String);
    procedure DelPoiByAge;
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
  SetPoi(PoILayer, APRSConfig.Latitude, APRSConfig.Longitude, APRSConfig.Callsign, True, GetImageIndex('y', '/'), MVMap.GPSItems);
  MyPosition := FindGPSItem(PoILayer, APRSConfig.Callsign);
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

  MVMap.Width := FMain.Width - GroupBox1.Width - 25;
  MVMap.Refresh;
end;

procedure TFMain.MIInfoClick(Sender: TObject);
begin
  FInfo.Show;
end;

procedure TFMain.MenuItem4Click(Sender: TObject);
begin
  if not OpenURL('https://github.com/andreaspeters/flexpacket') then
    ShowMessage('Could not open URL: https://github.com/andreaspeters/flexpacket');
end;

procedure TFMain.MIFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFMain.MVMapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var poi: TPointOfInterest;
    i, count, curZoom: Integer;
begin
  poi := FindGPSItem(PoILayer, x, y);
  if poi <> nil then
  begin
    count := CBEPOIList.ItemsEx.Count;
    for i:= 0 to count - 1 do
    begin
      if Trim(SplitString(CBEPOIList.ItemsEx.Items[i].Caption, '>')[0]) = poi.Caption then
      begin
        curZoom := MVMap.Zoom;
        CBEPOIList.ItemIndex := i;
        SelectPOI(Sender);
        MVMap.Zoom := curZoom;
        Break;
      end;
   end;
 end;
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
    call: String;
begin
  try
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
      STAltitude.Caption := IntToStr(msg^.Altitude);
      STCourse.Caption := IntToStr(msg^.Course);
      STSpeed.Caption := IntToStr(msg^.Speed);
      STPower.Caption := IntToStr(msg^.PHGPower);
      STHeight.Caption := IntToStr(msg^.PHGHeight);
      STGain.Caption := IntToStr(msg^.PHGGain);
      STDirectivity.Caption := msg^.PHGDirectivity;
      STStrength.Caption := IntToStr(msg^.DFSStrength);
      STDFSHeight.Caption := IntToStr(msg^.DFSHeight);
      STDFSGain.Caption := IntToStr(msg^.DFSGain);
      STDFSDirectivity.Caption := msg^.DFSDirectivity;
      STRNGRange.Caption := IntToStr(msg^.RNGRange);
      STWXDirection.Caption := IntToStr(msg^.WXDirection);
      STWXSpeed.Caption := IntToStr(msg^.WXSpeed);
      STWXGust.Caption := IntToStr(msg^.WXGust);
      STWXTemperature.Caption := IntToStr(msg^.WXTemperature);
      STWXPressure.Caption := FloatToStr(msg^.WXPressure);
      STWXRainFall1h.Caption := IntToStr(msg^.WXRainFall1h);
      STWXRainFall24h.Caption := IntToStr(msg^.WXRainFall24h);
      STWXRainFallToday.Caption := IntToStr(msg^.WXRainFallToday);
      STWXRainCount.Caption := IntToStr(msg^.WXRainCount);
      STWXHumidity.Caption := IntToStr(msg^.WXHumidity);
      STWXSnowFall24h.Caption := IntToStr(msg^.WXSnowFall);
      STWXLum.Caption := IntToStr(msg^.WXLum);

      MVMap.Zoom := 28;
      if (Sender is TComboBoxEx) then
        MVMap.CenterOnObj(FindGPSItem(PoiLayer, call));
    end;
  except
  end;
end;

procedure TFMain.SettingsClick(Sender: TObject);
begin
  FSettings.SetConfig(@APRSConfig);
  FSettings.Show;
end;

procedure TFMain.ShowMapMousePosition(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  p: TRealPoint;
begin
  p := MVMap.ScreenToLatLon(Point(X, Y));
  SBMain.Panels[1].Text := 'Longitude: ' + LonToStr(p.Lon, False);
  SBMain.Panels[0].Text := 'Latitude: ' + LatToStr(p.Lat, False);
end;

procedure TFMain.TBZoomMapChange(Sender: TObject);
begin
  MVMap.Zoom := TBZoomMap.Position;
end;


procedure TFMain.DelPoiByAge;
var i, count: Integer;
    curTime: TTime;
    msg: PAPRSMessage;
begin
  curTime := now();
  count := PoiLayer.PointsOfInterest.Count;
  for i := 1 to count - 1 do
  begin
    msg := APRSMessageList.Find(PoILayer.PointsOfInterest.Items[i].Caption);
    if msg <> nil then
    begin
      if Frac(curTime - msg^.Time)*1440 > 5 then
      begin
        DeleteCombobox(msg^.FromCall);
        APRSMessageList.Remove(msg);
        PoILayer.PointsOfInterest.Delete(i);
      end;
    end;
  end;
end;


procedure TFMain.TMainLoopTimer(Sender: TObject);
var msg: TAPRSMessage;
    buffer: String;
    newMSG: PAPRSMessage;
begin
  if APRSConfig.IGateEnabled then
  begin
    try
      DelPoIByAge;
      buffer := IGate.APRSBuffer;
      IGate.APRSBuffer := '';
      msg := IGate.DecodeAPRSMessage(buffer);

      if Length(msg.FromCall) > 0 then
      begin
        New(newMSG);
        newMSG^ := msg;

        if APRSMessageList.Find(msg.FromCall) = nil then
        begin
          // create a new poi
          SetPoi(PoILayer, newMsg, MVMap.GPSItems);
          AddCombobox(msg);
          MVMap.Refresh;
        end
        else
        begin
          // update poi data
          DelPoI(PoILayer, msg.FromCall);
          SetPoi(PoILayer, newMsg, MVMap.GPSItems);
          MVMap.Refresh;
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

procedure TFMain.DeleteCombobox(const Call: String);
var i, count: Integer;
begin
  count := CBEPOIList.ItemsEx.Count;
  for i:= 0 to count - 1 do
  begin
    if Trim(SplitString(CBEPOIList.ItemsEx.Items[i].Caption, '>')[0]) = Call then
    begin
      CBEPOIList.ItemsEx.Delete(i);
      Exit;
    end;
  end;
end;

procedure TFMain.AddCombobox(const msg: TAPRSMessage);
var poi: TGpsPoint;
    km: Double;
    imageIndex: Byte;
begin
  poi := TGpsPoint(FindGPSItem(PoILayer, msg.FromCall));
  km := poi.DistanceInKmFrom(TGpsPoint(MyPosition),False);
  imageIndex := GetImageIndex(msg.Icon, msg.IconPrimary);
  CBEPOIList.ItemsEx.AddItem(msg.FromCall + ' > ' + IntToStr(Round(km)) + 'km' , imageIndex, 0, 0, 0, nil);
end;

end.

