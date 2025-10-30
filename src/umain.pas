unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mvMapViewer, mvDLEFpc, mvDE_BGRA, Forms, Controls,
  Graphics, Dialogs, ComCtrls, StdCtrls, ExtCtrls, Menus, ComboEx, uresize,
  utypes, ureadpipe, uaprs, mvGPSObj, RegExpr, mvTypes, mvEngine,
  mvDE_RGBGraphics, Contnrs, uini, uigate, StrUtils, usettings, LCLIntf,
  FileCtrl, Buttons, PairSplitter, uinfo, mvMapProvider, umodes;

type

  { TFMain }

  TFMain = class(TForm)
    CBEPOIList: TComboBoxEx;
    CBEMapProvider: TComboBoxEx;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ICallsignIcon: TImage;
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
    Label34: TLabel;
    Label35: TLabel;
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
    btnBuymeacoffee: TMenuItem;
    MIKofi: TMenuItem;
    mntDonate: TMenuItem;
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
    MvRGBGraphicsDrawingEngine1: TMvRGBGraphicsDrawingEngine;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
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
    SPTrack: TSpeedButton;
    STAltitude: TStaticText;
    STMapCopyright: TStaticText;
    STCallsign: TStaticText;
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
    procedure btnBuymeacoffeeClick(Sender: TObject);
    procedure ChangeMapProvider(Sender: TObject);
    procedure FMainInit(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure MIKofiClick(Sender: TObject);
    procedure MIInfoClick(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MIFileExitClick(Sender: TObject);
    procedure mntDonateClick(Sender: TObject);
    procedure MVMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MVMapZoomChange(Sender: TObject);
    procedure SelectPOI(Sender: TObject);
    procedure SettingsClick(Sender: TObject);
    procedure ShowMapMousePosition(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SPTrackClick(Sender: TObject);
    procedure TBZoomMapChange(Sender: TObject);
    procedure TMainLoopTimer(Sender: TObject);
    procedure AddCombobox(const msg: TAPRSMessage);
    procedure DeleteCombobox(const Call: String);
    procedure DelPoiByAge;
    procedure AddPoI(msg: TAPRSMessage);
    function TrackHasPoint(Track: TGPSPointList; const Lat, Lon: Double): Boolean;
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
  ModeS: TModeSThread;
  LastZoom: Byte;
  MyPosition: TGPSObj;
  MyPositionGPS: TGPSPoint;
  PoILayer, myPoILayer: TMapLayer;
  ModeSCount: Integer;
  TrackID: Integer;

implementation

{$R *.lfm}

{ TFMain }

procedure TFMain.FMainInit(Sender: TObject);
var Providers: TStringList;
    i, CountProvider: Byte;
begin
  FormatSettings.DecimalSeparator := '.';
  OrigWidth := Self.Width;
  OrigHeight := Self.Height;
  ModeSCount := 0; // counter for Aircraft objects

  LoadConfigFromFile(@APRSConfig);

  MVMap.Engine.AddMapProvider('OpenStreetMap Local Tiles', ptEPSG3857, 'file:///'+APRSConfig.LocalTilesDirectory+'/%z%/%x%/%y%.png', 0, 19, 3,Nil);

  TBZoomMap.Position := MVMap.Zoom;
  MVMap.CachePath := APRSConfig.MAPCache;
  StoreOriginalSizes(Self);

  APRSMessageList := TFPHashList.Create;

  ReadPipe := TReadPipeThread.Create('flexpacketaprspipe');
  IGate := TIGateThread.Create(@APRSConfig);

  PoILayer := (MVMap.Layers.Add as TMapLayer);
  SetPoi(PoILayer, APRSConfig.Latitude, APRSConfig.Longitude, APRSConfig.Callsign, True, GetImageIndex('y', '/'), MVMap.GPSItems);
  MyPosition := FindGPSItem(PoILayer, APRSConfig.Callsign);
  if MyPosition <> nil then
    MyPositionGPS := TGpsPoint(MyPosition);
  MVMap.CenterOnObj(MyPosition);

  Providers := TStringList.Create;
  MVMap.GetMapProviders(Providers);
  CountProvider := Providers.Count;
  for i := 0 to CountProvider - 1 do
  begin
    CBEMapProvider.ItemsEx.AddItem(Providers.ValueFromIndex[i], -1, 0, 0, 0, nil);

    if Providers.ValueFromIndex[i] = APRSConfig.MAPProvider then
    begin
      CBEMapProvider.ItemIndex := i;
      ChangeMapProvider(Sender);
    end;
  end;

  ModeS := TModeSThread.Create(@APRSConfig);
end;

procedure TFMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveConfigToFile(@APRSConfig);
  try
    if Assigned(ModeS) then
    begin
      ModeS.Stop;
      ModeS.Terminate;
    end;
    if Assigned(ReadPipe) then
      ReadPipe.Terminate;
  except
  end;
end;

procedure TFMain.FormResize(Sender: TObject);
begin
  PairSplitter1.Position := Width - 619;
end;

procedure TFMain.ChangeMapProvider(Sender: TObject);
begin
  MVMap.MapProvider := CBEMapProvider.ItemsEx.Items[CBEMapProvider.ItemIndex].Caption;
  STMapCopyright.Caption := 'MAP Copyright '+MVMap.MapProvider;
  STMapCopyright.Width := Length(STMapCopyright.Caption)*7;
  APRSConfig.MAPProvider := MVMap.MapProvider;
  SaveConfigToFile(@APRSConfig);
end;

procedure TFMain.btnBuymeacoffeeClick(Sender: TObject);
begin
  if not OpenURL('https://buymeacoffee.com/hamradiotech') then
    ShowMessage('Could not open URL: https://buymeacoffee.com/hamradiotech');
end;

procedure TFMain.MIKofiClick(Sender: TObject);
begin
  if not OpenURL('https://ko-fi.com/andreaspeters') then
    ShowMessage('Could not open URL: https://ko-fi.com/andreaspeters');
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

procedure TFMain.mntDonateClick(Sender: TObject);
begin
  if not OpenURL('https://www.paypal.com/donate/?hosted_button_id=ZDB5ZSNJNK9XQ') then
    ShowMessage('Could not open URL: https://www.paypal.com/donate/?hosted_button_id=ZDB5ZSNJNK9XQ');
end;

procedure TFMain.MVMapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var poi: TMapPointOfInterest;
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

// User select one PoI
procedure TFMain.SelectPOI(Sender: TObject);
var msg: PAPRSMessage;
    call: String;
    poiGPS: TGPSObj;
begin
  try
    MAPRSMessage.Lines.Clear;

    call := Trim(SplitString(CBEPOIList.ItemsEx.Items[CBEPOIList.ItemIndex].Caption, '>')[0]);
    msg := APRSMessageList.Find(call);
    if msg <> nil then
    begin
      ICallSignIcon.ImageIndex := msg^.ImageIndex;
      MAPRSMessage.Lines.Add(msg^.Message);
      STCallsign.Caption := Call;
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
      begin
        poiGPS := FindGPSItem(PoiLayer, call);
        if poiGPS <> nil then
          MVMap.CenterOnObj(poiGPS)
      end;

      if msg^.Track.Visible then
        SPTrack.Down := True
      else
        SPTrack.Down := False;
    end;
  except
    on E: Exception do
    begin
      {$IFDEF UNIX}
      writeln('Select PoI Error: ', E.Message);
      {$ENDIF}
    end;
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
  SBMain.Panels[2].Text := 'Locator: ' + LatLonToLocator(p.Lat, p.Lon);
  SBMain.Panels[1].Text := 'Longitude: ' + LonToStr(p.Lon, False);
  SBMain.Panels[0].Text := 'Latitude: ' + LatToStr(p.Lat, False);
end;

procedure TFMain.SPTrackClick(Sender: TObject);
var msg: PAPRSMessage;
begin
  msg := APRSMessageList.Find(STCallsign.Caption);

  if SPTrack.Down then
    msg^.Track.Visible := True
  else
    msg^.Track.Visible := False;

end;

procedure TFMain.TBZoomMapChange(Sender: TObject);
begin
  MVMap.Zoom := TBZoomMap.Position;
end;


// Cleanup old PoIs
procedure TFMain.DelPoiByAge;
var i: Integer;
    curTime: TTime;
    poi: TMapPointOfInterest;
    msg: PAPRSMessage;
    call: String;
begin
  curTime := now();
  // Do not check position 0 because it's ourself.
  i := 1;
  while i < PoiLayer.PointsOfInterest.Count do
  begin
    try
      poi := PoILayer.PointsOfInterest.Items[i];
      if poi = nil then
      begin
        inc(i);
        Continue;
      end;

      call := PoILayer.PointsOfInterest.Items[i].caption;
      if Length(call) <= 0 then
      begin
        PoILayer.PointsOfInterest.Delete(i);
        Continue;
      end;

      msg := APRSMessageList.Find(call);
      if msg = nil then
      begin
        DeleteCombobox(call);
        DelPoI(PoILayer, call);
        Continue;
      end;

      if Frac(curTime - msg^.Time)*1440 > APRSConfig.CleanupTime then
      begin
        if Assigned(msg^.Track) then
        begin
          try
            if Assigned(MVMap.GPSItems) then
              MVMap.GPSItems.Delete(msg^.Track);
            msg^.Track := TGPSTrack.Create;
          except
            {$IFDEF UNIX}
            writeln('Error Delete GPS Track')
            {$ENDIF}
          end;
        end;
        ModeS.ModeSMessageList.Remove(msg);
        DeleteCombobox(call);
        DelPoI(PoILayer, call);
        Continue;
      end;
    except
      {$IFDEF UNIX}
      writeln('Error Cleanup Old PoI')
      {$ENDIF}
    end;
    inc(i);
  end;

  // cleanup modes
  i := 0;
  while i < ModeS.ModeSMessageList.Count do
  begin
    try
      msg := ModeS.ModeSMessageList.Items[i];
      if not Assigned(APRSMessageList.Find(msg^.FromCall)) then
      begin
        ModeS.ModeSMessageList.Delete(i);
        Continue;
      end;
    except
      {$IFDEF UNIX}
      writeln('Error Cleanup Old ModeS PoI')
      {$ENDIF}
    end;
    inc(i);
  end;

  MVMap.Refresh;
end;

// Add APRS message as PoI
procedure TFMain.AddPoI(msg: TAPRSMessage);
var newMSG, oldMSG: PAPRSMessage;
begin
  try
    if Length(msg.FromCall) > 0 then
    begin
      New(newMSG);
      newMSG^ := msg;

      oldMSG := APRSMessageList.Find(msg.FromCall);

      // if not already exist, create TrackID, else reuse old TrackID and ImageIndex
      if oldMSG = nil then
      begin
        inc(TrackID);
        newMsg^.TrackID := TrackID;
        MVMap.GPSItems.Add(newMsg^.Track, newMsg^.TrackID);
      end
      else
      begin
        newMsg^.Track := oldMsg^.Track;
        newMsg^.ImageIndex := oldMsg^.ImageIndex;
      end;

      if (newMsg^.Longitude > 0) and (newMsg^.Latitude > 0) then
        if not TrackHasPoint(newMsg^.Track.Points, newMsg^.Latitude, newMsg^.Longitude) then
          newMsg^.Track.Points.Add(TGPSPoint.Create(newMsg^.Longitude, newMsg^.Latitude, newMsg^.Altitude));

      SetPoi(PoILayer, newMsg, MVMap.GPSItems);

      // add callsign to Combobox
      if oldMSG = nil then
        AddCombobox(newMsg^);

      MVMap.Refresh;
    end;
  except
    on E: Exception do
    begin
      {$IFDEF UNIX}
      writeln('Error AddPoi: ', E.Message);
      {$ENDIF}
      if Assigned(newMsg) and (Length(newMsg^.FromCall) > 0) then
        DelPoI(PoILayer, newMsg^.FromCall);

      if Assigned(oldMsg) and (Length(newMsg^.FromCall) > 0) then
        DelPoI(PoILayer, oldMsg^.FromCall);
    end;
  end;
end;

procedure TFMain.TMainLoopTimer(Sender: TObject);
var buffer: String;
begin
  DelPoIByAge;

  if APRSConfig.IGateEnabled then
  begin
    try
      buffer := IGate.APRSBuffer;
      IGate.APRSBuffer := '';
      AddPoI(IGate.DecodeAPRSMessage(buffer));
    except
      on E: Exception do
      begin
        {$IFDEF UNIX}
        writeln('Error Main Loop IGate: ', E.Message);
        {$ENDIF}
      end;
    end;
  end;

  try
    AddPoI(ReadPipe.DecodeAPRSMessage(ReadPipe.PipeData));
  except
    on E: Exception do
      {$IFDEF UNIX}
      writeln('Error Main Loop Receive Data Pipe: ', E.Message);
      {$ENDIF}
  end;

  if Assigned(ModeS.ModeSMessageList) then
    if ModeS.ModeSMessageList.Count > 0 then
    begin
      if ModeSCount >= ModeS.ModeSMessageList.Count then
        ModeSCount := 0;

      try
        if Assigned(ModeS.ModeSMessageList.Items[ModeSCount]) then
          AddPoI(PAPRSMessage(ModeS.ModeSMessageList.Items[ModeSCount])^);
      except
        on E: Exception do
        begin
          {$IFDEF UNIX}
          writeln('Error Main Loop ModeS: ', E.Message);
          {$ENDIF}
        end;
      end;
      inc(ModeSCount);
    end;
end;

// Check if track with given Points already exist
function TFMain.TrackHasPoint(Track: TGPSPointList; const Lat, Lon: Double): Boolean;
var i: Integer;
    P: TGPSPoint;
begin
  Result := False;

  if not Assigned(Track) or (Track.Count <= 0) then
    Exit;

  for i := 0 to Track.Count - 1 do
  begin
    P := Track[i];
    if (P.Lat = Lat) and (P.Lon = Lon) then
      Result := True;
  end;
end;

// Delete callsign from Combobox
procedure TFMain.DeleteCombobox(const Call: String);
var i: Integer;
begin
  for i:= 0 to CBEPOIList.ItemsEx.Count - 1 do
  begin
    if Trim(SplitString(CBEPOIList.ItemsEx.Items[i].Caption, '>')[0]) = Trim(Call) then
    begin
      CBEPOIList.ItemsEx.Delete(i);
      CBEPOIList.Refresh;
      Exit;
    end;
  end;
end;

// Add callsign into Combobox
procedure TFMain.AddCombobox(const msg: TAPRSMessage);
var poi, me: TGpsPoint;
    km: Double;
begin
  poi := TGpsPoint(FindGPSItem(PoILayer, msg.FromCall));

  if (poi = nil) or (MyPositionGPS = nil) then
    Exit;

  try
    km := poi.DistanceInKmFrom(MyPositionGPS,False);
    CBEPOIList.ItemsEx.AddItem(msg.FromCall + ' > ' + IntToStr(Round(km)) + 'km' , msg.ImageIndex, 0, 0, 0, nil);
  except
    on E: Exception do
    begin
      {$IFDEF UNIX}
      writeln('Calculate Distance in km: ', E.Message);
      {$ENDIF}
    end;
  end;
end;

end.

