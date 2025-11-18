unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mvMapViewer, mvDLEFpc, mvDE_BGRA, Forms, Controls,
  Graphics, Dialogs, ComCtrls, StdCtrls, ExtCtrls, Menus, ComboEx, uresize,
  utypes, ureadpipe, uaprs, mvGPSObj, RegExpr, mvTypes, mvEngine,
  mvDE_RGBGraphics, Contnrs, uini, uigate, StrUtils, usettings, LCLIntf,
  Buttons, PairSplitter, ActnList, TAGraph, TAStyles, fpexprpars,
  uinfo, mvMapProvider, umodes, UniqueInstance, ulastseen, urawmessage,
  TASeries, TATools, u_rs41sg;

type

  { TFMain }

  TFMain = class(TForm)
    actExit: TAction;
    actSettings: TAction;
    actOpenLastseen: TAction;
    actShowHide: TAction;
    ActionList1: TActionList;
    CBEFilter: TComboBoxEx;
    CBEMapProvider: TComboBoxEx;
    CBEPOIList: TComboBoxEx;
    fpWX: TFlowPanel;
    fpCharts: TFlowPanel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
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
    Label36: TLabel;
    Label37: TLabel;
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
    MenuItem3: TMenuItem;
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
    MVMap: TMapView;
    MvRGBGraphicsDrawingEngine1: TMvRGBGraphicsDrawingEngine;
    pcPoITab: TPageControl;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    pmTray: TPopupMenu;
    sbShowRawMessages: TSpeedButton;
    scWX: TScrollBox;
    scCharts: TScrollBox;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Settings: TMenuItem;
    MIFileExit: TMenuItem;
    SBMain: TStatusBar;
    Settings1: TMenuItem;
    SPTrack: TSpeedButton;
    STAltitude: TStaticText;
    stCount: TStaticText;
    STCallsign: TStaticText;
    STCourse: TStaticText;
    STDFSDirectivity: TStaticText;
    STDFSGain: TStaticText;
    STDFSHeight: TStaticText;
    STDirectivity: TStaticText;
    STGain: TStaticText;
    STHeight: TStaticText;
    STIconDescription: TStaticText;
    stLastUpdate: TStaticText;
    STLatitude: TStaticText;
    STLatitudeDMS: TStaticText;
    STLongitude: TStaticText;
    STLongitudeDMS: TStaticText;
    STMapCopyright: TStaticText;
    STPower: TStaticText;
    STRNGRange: TStaticText;
    STSpeed: TStaticText;
    STStrength: TStaticText;
    STWXDirection: TStaticText;
    STWXGust: TStaticText;
    STWXHumidity: TStaticText;
    STWXLum: TStaticText;
    STWXPressure: TStaticText;
    STWXRainCount: TStaticText;
    STWXRainFall1h: TStaticText;
    STWXRainFall24h: TStaticText;
    STWXRainFallToday: TStaticText;
    STWXSnowFall24h: TStaticText;
    STWXSpeed: TStaticText;
    STWXTemperature: TStaticText;
    tsMain: TTabSheet;
    tsCharts: TTabSheet;
    tsWX: TTabSheet;
    TBZoomMap: TTrackBar;
    tRefresh: TTimer;
    TMainLoop: TTimer;
    TMainLoop1: TTimer;
    TrayIcon1: TTrayIcon;
    UniqueInstance1: TUniqueInstance;
    procedure actExitExecute(Sender: TObject);
    procedure actOpenLastseenExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actShowHideExecute(Sender: TObject);
    procedure btnBuymeacoffeeClick(Sender: TObject);
    procedure CBEFilterSelect(Sender: TObject);
    procedure ChangeMapProvider(Sender: TObject);
    procedure FMainInit(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormHide(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MIKofiClick(Sender: TObject);
    procedure MIInfoClick(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure mntDonateClick(Sender: TObject);
    procedure MVMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MVMapZoomChange(Sender: TObject);
    procedure pcPoITabChange(Sender: TObject);
    procedure sbShowRawMessagesClick(Sender: TObject);
    procedure SelectPOI(Sender: TObject);
    procedure ShowMapMousePosition(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SPTrackClick(Sender: TObject);
    procedure TBZoomMapChange(Sender: TObject);
    procedure TMainLoopTimer(Sender: TObject);
    procedure AddCombobox(const msg: TAPRSMessage);
    procedure DeleteCombobox(const Call: String);
    procedure DelPoiByAge;
    procedure AddPoI(msg: TAPRSMessage);
    procedure SetupFilterCombo;
    procedure tRefreshTimer(Sender: TObject);
    procedure WriteChart(X: TDoubleList; Title, TitleX, TitleY: String; ParentCtrl: TWinControl);
    procedure UpdateWXCaption(msg: TAPRSMessage);
    procedure UpdateDevices(newMsg, oldMsg: PAPRSMessage);
    function GetWXCaption(wx: TDoubleList; calc: String): String;
    function GetWXCaption(wx: TDoubleList): String;
    function TrackHasPoint(Track: TGPSPointList; const Lat, Lon: Double): Boolean;
  private
  public
    Debug: Boolean;
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
  IsClosing: Boolean;

implementation

{$R *.lfm}

{ TFMain }

procedure TFMain.FMainInit(Sender: TObject);
var Providers: TStringList;
    i, CountProvider: Byte;
begin
  Debug := False;
  isClosing := False;

  if ParamCount > 0 then
    if ParamStr(1) = '-d' then
      Debug := True;

  FormatSettings.DecimalSeparator := '.';
  ModeSCount := 0; // counter for Aircraft objects

  LoadConfigFromFile(@APRSConfig);

  OrigWidth := APRSConfig.MainWidth;
  OrigHeight := APRSConfig.MainHeight;

  MVMap.Engine.AddMapProvider('OpenStreetMap Local Tiles', ptEPSG3857, 'file:///'+APRSConfig.LocalTilesDirectory+'/%z%/%x%/%y%.png', 0, 19, 3,Nil);

  TBZoomMap.Position := MVMap.Zoom;
  MVMap.CachePath := APRSConfig.MAPCache;
  StoreOriginalSizes(Self);

  APRSMessageList := TFPHashList.Create;

  ReadPipe := TReadPipeThread.Create('flexpacketaprspipe');
  IGate := TIGateThread.Create(@APRSConfig);

  PoILayer := (MVMap.Layers.Add as TMapLayer);
  SetPoi(PoILayer, APRSConfig.Latitude, APRSConfig.Longitude, APRSConfig.Callsign, True, APRSConfig.AprsSymbol+1, MVMap.GPSItems);
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
  SetupFilterCombo;

  pcPoITab.ActivePage := tsMain;
end;

procedure TFMain.SetupFilterCombo;
var i, count: Byte;
begin
  CBEFilter.ItemsEx.AddItem('All', 0, 0, 0, 0, nil);

  // Primary Icons
  count := Length(APRSPrimarySymbolTable);
  for i := 1 to count do
    CBEFilter.ItemsEx.AddItem(APRSPrimarySymbolTable[i].Description, i, 0, 0, 0, nil);

  // Alternate Icons
  count := Length(APRSAlternateSymbolTable);
  for i := 1 to count do
    CBEFilter.ItemsEx.AddItem(APRSAlternateSymbolTable[i].Description, i+96, 0, 0, 0, nil);
end;

procedure TFMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IsClosing := True;

  APRSConfig.MainPosY := FMain.Top;
  APRSConfig.MainPosX := FMain.Left;
  APRSConfig.MainWidth := Width;
  APRSConfig.MainHeight := Height;

  APRSConfig.LastSeenPosY := FLastSeen.Top;
  APRSConfig.LastSeenPosX := FLastSeen.Left;
  APRSConfig.LastSeenWidth := FLastSeen.Width;
  APRSConfig.LastSeenHeight := FLastSeen.Height;
  APRSConfig.LastSeenVisible := FLastSeen.Visible;

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

procedure TFMain.FormHide(Sender: TObject);
begin
  if not IsClosing then
  begin
    APRSConfig.MainPosX := FMain.Left;
    APRSConfig.MainPosY := FMain.Top;
  end;
end;

procedure TFMain.FormResize(Sender: TObject);
begin
  PairSplitter1.Position := Width - 619;
end;

procedure TFMain.FormShow(Sender: TObject);
begin
  FLastSeen.SetConfig(@APRSConfig);
  FLastSeen.Visible := APRSConfig.LastSeenVisible;

  FRAWMessage.SetConfig(@APRSConfig);
  FRAWMessage.Visible := APRSConfig.RawMessageVisible;

  Width := APRSConfig.MainWidth;
  Height := APRSConfig.MainHeight;

  if (APRSConfig.MainPosX > 0) and (APRSConfig.MainPosY > 0) then
  begin
    Top := APRSConfig.MainPosY;
    Left := APRSConfig.MainPosX;
  end;
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

procedure TFMain.actShowHideExecute(Sender: TObject);
begin
  if FMain.WindowState = wsMinimized then
  begin
    FMain.WindowState := wsNormal;
    FMain.Show
  end
  else
  begin
    FMain.WindowState := wsMinimized;
    FMain.Hide;
  end;
end;

procedure TFMain.actOpenLastseenExecute(Sender: TObject);
begin
  if FLastSeen.Visible then
  begin
    FLastSeen.Visible := False;
    FLastSeen.Hide;
  end
  else
    FLastSeen.Show;
end;

procedure TFMain.actSettingsExecute(Sender: TObject);
begin
  FSettings.SetConfig(@APRSConfig);
  FSettings.Show;
end;

procedure TFMain.actExitExecute(Sender: TObject);
begin
  close;
end;

procedure TFMain.CBEFilterSelect(Sender: TObject);
var description: String;
    i: Byte;
    msg: PAPRSMessage;
    poi: TMapPointOfInterest;
    count: Integer;
begin
  description := CBEFilter.ItemsEx.Items[CBEFilter.ItemIndex].Caption;

  if Length(description) > 0 then
  begin
    if not Assigned(PoiLayer) or not Assigned(PoiLayer.PointsOfInterest) then
      Exit;

    count := PoiLayer.PointsOfInterest.Count;
    if count = 0 then
      Exit;

    // Hide All
    for i := 0 to count - 1 do
    begin
      poi := PoiLayer.PointsOfInterest[i];
      if not Assigned(poi) then
        Continue;
      poi.Visible := False;
    end;

    // Show PoI's by filter
    for i := 0 to count - 1 do
    begin
      poi := PoiLayer.PointsOfInterest[i];
      if not Assigned(poi) then
        Continue;
      try
        msg := APRSMessageList.Find(poi.Caption);

        if Assigned(msg) then
        begin
          // Sichtbarkeit filtern
          if SameText(msg^.ImageDescription, description) or SameText(description, 'All') then
            poi.Visible := True;

        end;
      except
        on E: Exception do
        begin
          {$IFDEF UNIX}
          writeln('Filter PoI Error bei Index ', i, ': ', E.Message);
          {$ENDIF}
        end;
      end;
    end;
  end;
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

procedure TFMain.pcPoITabChange(Sender: TObject);
var msg: PAPRSMessage;
begin
  msg := APRSMessageList.Find(STCallsign.Caption);
  if not Assigned(msg) then
    Exit;

  msg^.ActiveTabSheet := (Sender as TPageControl).ActivePage;
end;

procedure TFMain.sbShowRawMessagesClick(Sender: TObject);
var msg: PAPRSMessage;
begin
  msg := APRSMessageList.Find(STCallsign.Caption);
  if not Assigned(msg) then
  begin
    sbShowRawMessages.Down := False;
    Exit;
  end;

  if sbShowRawMessages.Down then
  begin
    FRawMessage.Show;
    FRawMessage.mRawMessage.Lines.AddStrings(msg^.RAWMessages);
  end
  else
    FRawMessage.Close;


end;

// User select one PoI
procedure TFMain.SelectPOI(Sender: TObject);
var msg: PAPRSMessage;
    call: String;
    poiGPS: TGPSObj;
    i, chartScroll, wxScroll: Integer;
begin
  try
    MAPRSMessage.Lines.Clear;

    // Cleanup all Charts.
    chartScroll := scCharts.VertScrollBar.Position;
    wxScroll := scWX.VertScrollBar.Position;
    for i := 0 to fpCharts.ControlCount - 1 do
    begin
      if (fpCharts.Controls[i] is TChart) then
        TChart(fpCharts.Controls[i]).Visible := False;
    end;
    for i := 0 to fpWX.ControlCount - 1 do
    begin
      if (fpWX.Controls[i] is TChart) then
        TChart(fpWX.Controls[i]).Visible := False;
    end;

    if (CBEPOIList.ItemIndex >= 0) and (CBEPOIList.ItemsEx.Count >= 0) then
      call := Trim(SplitString(CBEPOIList.ItemsEx.Items[CBEPOIList.ItemIndex].Caption, '>')[0]);

    if Sender is TListView then
      call := (Sender as TListView).Selected.Caption;

    if Sender is TTimer then
      call := STCallsign.Caption;

    msg := APRSMessageList.Find(call);
    if Assigned(msg) then
    begin
      ICallSignIcon.ImageIndex := msg^.ImageIndex;
      MAPRSMessage.Lines.Add(msg^.Message);
      STCallsign.Caption := Call;
      STIconDescription.Caption := msg^.ImageDescription;
      STLatitude.Caption := LatToStr(msg^.Latitude, False);
      STLongitude.Caption := LonToStr(msg^.Longitude, False);
      STLatitudeDMS.Caption := LatToStr(msg^.Latitude, True);
      STLongitudeDMS.Caption := LonToStr(msg^.Longitude, True);
      STAltitude.Caption := FloatToStr(msg^.Altitude.Last);
      STCourse.Caption := FloatToStr(msg^.Course);
      STPower.Caption := FloatToStr(msg^.PHGPower);
      STHeight.Caption := FloatToStr(msg^.PHGHeight);
      STGain.Caption := FloatToStr(msg^.PHGGain);
      STDirectivity.Caption := msg^.PHGDirectivity;
      STStrength.Caption := FloatToStr(msg^.DFSStrength);
      STDFSHeight.Caption := FloatToStr(msg^.DFSHeight);
      STDFSGain.Caption := FloatToStr(msg^.DFSGain);
      STDFSDirectivity.Caption := msg^.DFSDirectivity;
      STRNGRange.Caption := FloatToStr(msg^.RNGRange);
      stLastUpdate.Caption := FormatDateTime('dd.mm.yyyy - hh:nn:ss', msg^.Time);
      stCount.Caption := '#'+IntToStr(msg^.Count);

      if not (Sender is TTimer) then
      begin
        MVMap.Zoom := 28;
        if (Sender is TComboBoxEx) or (Sender is TListView) then
        begin
          poiGPS := FindGPSItem(PoiLayer, call);
          if Assigned(poiGPS) then
            MVMap.CenterOnObj(poiGPS)
        end;
      end;

      if msg^.Track.Visible then
        SPTrack.Down := True
      else
        SPTrack.Down := False;

      if Assigned(msg^.Speed) and (msg^.Speed.Count > 0) then
      begin
        WriteChart(msg^.Speed, 'Speed', 'Time', 'Speed (km/h)', fpCharts);
        STSpeed.Caption := FloatToStr(msg^.Speed.Last);
      end;


      if Assigned(msg^.Altitude) and (msg^.Altitude.Count > 0) then
      begin
        WriteChart(msg^.Altitude, 'Altitude', 'Time', 'Altitude (m)', fpCharts);
        STAltitude.Caption := FloatToStr(msg^.Altitude.Last);
      end;

      if not (msg^.ModeS) then
      begin
        UpdateWXCaption(msg^);

        FRawMessage.mRawMessage.Clear;

        // update Raw Message window
        if FRawMessage.Visible and (Trim(STCallsign.Caption) = Trim(msg^.FromCall)) then
          FRawMessage.mRawMessage.Lines.AddStrings(msg^.RAWMessages);
      end;

      with msg^.Devices do
      begin
        if RS41.Enabled then
          RS41SGPChart(msg, fpCharts);
      end;

      scCharts.VertScrollBar.Position := chartScroll;
      scWX.VertScrollBar.Position := wxScroll;
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

procedure TFMain.UpdateWXCaption(msg: TAPRSMessage);
begin
  try
    STWXGust.Caption := GetWXCaption(msg.WXGust);
    STWXSpeed.Caption := GetWXCaption(msg.WXSpeed);
    STWXDirection.Caption := GetWXCaption(msg.WXDirection);
    STWXLum.Caption := GetWXCaption(msg.WXLum);
    STWXSnowFall24h.Caption := GetWXCaption(msg.WXSnowFall);
    STWXRainCount.Caption := GetWXCaption(msg.WXRainCount);
    STWXRainFallToday.Caption := GetWXCaption(msg.WXRainFallToday);
    STWXRainFall24h.Caption := GetWXCaption(msg.WXRainFall24h);
    STWXRainFall1h.Caption := GetWXCaption(msg.WXRainFall1h);

    STWXTemperature.Caption := GetWXCaption(msg.WXTemperature);

    STWXPressure.Caption := GetWXCaption(msg.WXPressure);
    STWXHumidity.Caption := GetWXCaption(msg.WXHumidity);


    if Assigned(msg.WXTemperature) and (msg.WXTemperature.Count > 0) then
      WriteChart(msg.WXTemperature, 'Temperature', 'Time', 'Temperature (°C)', fpWX);

    if Assigned(msg.WXPressure) and (msg.WXPressure.Count > 0) then
      WriteChart(msg.WXPressure, 'Pressure', 'Time', 'Pressure (mb)', fpWX);

    if Assigned(msg.WXHumidity) and (msg.WXHumidity.Count > 0) then
      WriteChart(msg.WXHumidity, 'Humidity', 'Time', 'Humidity (%)', fpWX);

  except
    on E: Exception do
    begin
      {$IFDEF UNIX}
      writeln(Format('UpdateWXCaption %s Error: %s', [msg.FromCall, E.Message]));
      {$ENDIF}
    end;
  end;
end;

function TFMain.GetWXCaption(wx: TDoubleList): String;
begin
  Result := '';
  if Assigned(wx) and (wx.Count > 0) and (wx[wx.Count-1] <> -999999) then
    Result := FloatToStr(wx[wx.Count-1]);
end;

function TFMain.GetWXCaption(wx: TDoubleList; calc: String): String;
var expr: TFPExpressionParser;
begin
  Result := '';

  if not Assigned(wx) or (wx.Count = 0) or (wx[wx.Count-1] = -999999) then
    Exit;

  expr := TFPExpressionParser.Create(Self);
  try
    expr.Builtins := [bcMath];
    expr.Expression := FloatToStr(wx[wx.Count-1]) + Trim(calc);

    Result := FloatToStr(expr.Evaluate.ResFloat);
  except
    on E: Exception do
    begin
      {$IFDEF UNIX}
      writeln('GetWXCaption Error: ', E.Message);
      {$ENDIF}
    end;
  end;
end;


// create simple charts
procedure TFMain.WriteChart(X: TDoubleList; Title, TitleX, TitleY: String; ParentCtrl: TWinControl);
var Chart: TChart;
    ChartPoint: TLineSeries;
    i: Integer;
    ChartName: String;
begin
  if (not Assigned(X)) or (X.Count = 0) or (ParentCtrl = nil) then
    Exit;

  try
    ChartName := 'AutoGen_' + StringReplace(Title, ' ', '_', [rfReplaceAll]);

    // Check if chart with this name already exist.
    Chart := nil;
    for i := 0 to ParentCtrl.ControlCount - 1 do
    begin
      if (ParentCtrl.Controls[i] is TChart) and (TChart(ParentCtrl.Controls[i]).Name = ChartName) then
      begin
        Chart := TChart(ParentCtrl.Controls[i]);
        Chart.ClearSeries;
        Chart.Visible := True;;
        Break;
      end;
    end;

    // Create Chart
    if not Assigned(Chart) then
    begin
      Chart := TChart.Create(ParentCtrl);
      Chart.Parent := ParentCtrl;
      Chart.Align := alClient;
      Chart.Name := ChartName;
      Chart.Title.Text.Clear;
      Chart.Title.Text.Add(Title);
      Chart.Title.Visible := True;
    end;

    // X
    Chart.BottomAxis.Title.Caption := TitleX;
    Chart.BottomAxis.Title.Visible := True;
    Chart.BottomAxis.Range.Min := 0;
    if X.Count <= 10 then
      Chart.BottomAxis.Range.Max := 10
    else
      Chart.BottomAxis.Range.Max := X.Count;
    Chart.BottomAxis.Range.UseMin := True;
    Chart.BottomAxis.Range.UseMax := True;
    Chart.BottomAxis.Intervals.MaxLength := 100;
    Chart.BottomAxis.Intervals.MinLength := 20;
    Chart.BottomAxis.Marks.Format := '%0.f';

    // Y
    Chart.LeftAxis.Title.Caption := TitleY;
    Chart.LeftAxis.Title.Visible := True;
    Chart.LeftAxis.Intervals.MaxLength := 50;
    Chart.LeftAxis.Intervals.MinLength := 10;

    // Create Points
    ChartPoint := TLineSeries.Create(Chart);
    ChartPoint.LinePen.Width := 2;
    ChartPoint.SeriesColor := clRed;
    Chart.AddSeries(ChartPoint);

    // Punkte einfüllen
    for i := 0 to X.Count - 1 do
      ChartPoint.AddXY(i, X[i]);
  except
    on E: Exception do
    begin
      {$IFDEF UNIX}
      writeln('WriteChart AutoGen Error: ', E.Message);
      {$ENDIF}
    end;
  end;
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

  if not Assigned(msg) then
    Exit;

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
var i, x: Integer;
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
        PoILayer.PointsOfInterest.Delete(i);
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
        DeleteCombobox(call);
        PoILayer.PointsOfInterest.Delete(i);
        ModeS.ModeSMessageList.Remove(msg);
        // cleanup als call copies in the APRSMessage list
        x := 1;
        repeat
          msg := APRSMessageList.Items[x];
          if Assigned(msg) and (SameText(Trim(msg^.FromCall), Trim(call))) then
            APRSMessageList.Delete(x)
          else
            inc(x)
        until not Assigned(msg) or (x >= APRSMessageList.Count);
        Continue;
      end;
    except
      on E: Exception do
      begin
        {$IFDEF UNIX}
        writeln('Error DelPoiByAge: ', E.Message);
        {$ENDIF}
      end;
    end;
    inc(i);
  end;

  // cleanup modes
  i := 0;
  while i < ModeS.ModeSMessageList.Count do
  begin
    try
      msg := ModeS.ModeSMessageList.Items[i];
      if Frac(curTime - msg^.Time)*1440 > APRSConfig.CleanupTime then
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
    poi: TGpsPoint;
    visibility: Boolean;
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
        // Preserve old Data
        newMsg^.Track := oldMsg^.Track;
        newMsg^.ImageIndex := oldMsg^.ImageIndex;
        newMsg^.Count := oldMsg^.Count;

        PrependDoubleList(newMsg^.Speed, oldMsg^.Speed);
        PrependDoubleList(newMsg^.Altitude, oldMsg^.Altitude);
        PrependDoubleList(newMsg^.WXTemperature, oldMsg^.WXTemperature);
        PrependDoubleList(newMsg^.WXHumidity, oldMsg^.WXHumidity);
        PrependDoubleList(newMsg^.WXPressure, oldMsg^.WXPressure);

        // PrependDoubleList for all Devices
        UpdateDevices(newMsg, oldMsg);

        if not newMsg^.ModeS then
          newMsg^.RAWMessages.AddStrings(oldMsg^.RAWMessages);
      end;

      // how often we saw that call
      inc(newMsg^.Count);

      // update Raw Message window
      if FRawMessage.Visible and (Trim(STCallsign.Caption) = Trim(newMsg^.FromCall)) then
        FRawMessage.mRawMessage.Lines.AddStrings(newMsg^.RAWMessages);


      if (newMsg^.Longitude > 0) and (newMsg^.Latitude > 0) then
        if not TrackHasPoint(newMsg^.Track.Points, newMsg^.Latitude, newMsg^.Longitude) then
          newMsg^.Track.Points.Add(TGPSPoint.Create(newMsg^.Longitude, newMsg^.Latitude, newMsg^.Altitude.Last));

      // Filter is set
      visibility := True;
      if (FMain.CBEFilter.ItemIndex > 0) and not (newMsg^.ModeS) then
        if not SameText(FMain.CBEFilter.ItemsEx.Items[FMain.CBEFilter.ItemIndex].Caption, newMsg^.ImageDescription) then
          visibility := False;

      SetPoi(PoILayer, newMsg, visibility);

      poi := TGpsPoint(FindGPSItem(PoILayer, newMsg^.FromCall));
      if Assigned(MyPositionGPS) and Assigned(poi) then
        newMsg^.Distance := poi.DistanceInKmFrom(MyPositionGPS,False);

      FLastSeen.AddCallsign(newMsg);

      // add callsign to Combobox
      if oldMSG = nil then
        AddCombobox(newMsg^);

      MVMap.Refresh;
    end;
  except
    on E: Exception do
    begin
      {$IFDEF UNIX}
      writeln(Format('Error AddPoi (%s): %s', [newMsg^.FromCall, E.Message]));
      {$ENDIF}
      // Cleanup broken PoIs
      if Assigned(newMsg) and (Length(newMsg^.FromCall) > 0) then
        DelPoI(PoILayer, newMsg^.FromCall);

      if Assigned(oldMsg) and (Length(newMsg^.FromCall) > 0) then
        DelPoI(PoILayer, oldMsg^.FromCall);
    end;
  end;
end;

procedure TFMain.UpdateDevices(newMsg, oldMsg: PAPRSMessage);
begin
  try
    if (newMsg^.Devices.RS41.Enabled) then
      RS41SGPUpdate(newMsg, OldMsg)
  except
    on E: Exception do
    begin
      {$IFDEF UNIX}
      writeln(Format('UpdateDevices Error %s: %s ', [newMsg^.FromCall, E.Message]));
      {$ENDIF}
    end;
  end;
end;

procedure TFMain.TMainLoopTimer(Sender: TObject);
var buffer: String;
    msg: PAPRSMessage;
begin
  DelPoIByAge;

  if APRSConfig.IGateEnabled then
  begin
    try
      if not IGate.Error then
      begin
        buffer := IGate.APRSBuffer;
        IGate.APRSBuffer := '';
        AddPoI(IGate.DecodeAPRSMessage(buffer));
      end;
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
    if not ReadPipe.Error then
      AddPoI(ReadPipe.DecodeAPRSMessage(ReadPipe.PipeData));
  except
    on E: Exception do
      {$IFDEF UNIX}
      writeln('Error Main Loop Receive Data Pipe: ', E.Message);
      {$ENDIF}
  end;

  if not ModeS.Error and Assigned(ModeS.ModeSMessageList) then
  begin
    if ModeS.ModeSMessageList.Count > 0 then
    begin
      if ModeSCount >= ModeS.ModeSMessageList.Count then
        ModeSCount := 0;

      try
        if Assigned(ModeS.ModeSMessageList.Items[ModeSCount]) then
          msg := PAPRSMessage(ModeS.ModeSMessageList.Items[ModeSCount]);
          if Assigned(msg) then
          begin
            msg^.ModeS := True;
            AddPoI(msg^);
          end;
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

procedure TFMain.tRefreshTimer(Sender: TObject);
begin
  SelectPoI(Sender);
end;

// Delete callsign from Combobox
procedure TFMain.DeleteCombobox(const Call: String);
var i: Integer;
begin
  try
    for i:= 0 to CBEPOIList.ItemsEx.Count - 1 do
    begin
      if Trim(SplitString(CBEPOIList.ItemsEx.Items[i].Caption, '>')[0]) = Trim(Call) then
      begin
        CBEPOIList.ItemsEx.Delete(i);
        CBEPOIList.Refresh;
        Exit;
      end;
    end;
  except
    on E: Exception do
    begin
      {$IFDEF UNIX}
      writeln('Error DeleteCombobox: ', E.Message);
      {$ENDIF}
    end;
  end;
end;

// Add callsign into Combobox
procedure TFMain.AddCombobox(const msg: TAPRSMessage);
var km: Double;
begin
  try
    km := msg.Distance;
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

