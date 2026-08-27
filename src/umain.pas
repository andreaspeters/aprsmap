unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mvMapViewer, mvDLEFpc, mvDE_BGRA, Forms, Controls,
  Graphics, Dialogs, ComCtrls, StdCtrls, ExtCtrls, Menus, ComboEx, uresize,
  utypes, ureadpipe, uaprs, mvGPSObj, RegExpr, mvTypes, mvEngine,
  mvDE_RGBGraphics, Contnrs, uini, uigate, StrUtils, usettings, LCLIntf,
  Buttons, PairSplitter, ActnList, TAGraph, fpexprpars, base64,
  uinfo, mvMapProvider, umodes, UniqueInstance, ulastseen, urawmessage,
  TASeries, TATools, u_rs41sg, ugps, ulistmails, ueditor, mvGeoMath, Types,
  umeshcore, LResources, umodeslist, uaislist, uaeromuxdb;

type

  TObjectReporterLinkTrack = class(TGPSTrack)
  public
    procedure Draw(AView: TObject; {%H-}Area: TRealArea); override;
  end;

  { TFMain }

  TFMain = class(TForm)
    actExit: TAction;
    actGPS: TAction;
    actUpdateModeSDatabase: TAction;
    actSendPosition: TAction;
    actShowMessages: TAction;
    actSettings: TAction;
    actOpenLastseen: TAction;
    actShowHide: TAction;
    ActionList1: TActionList;
    CBEFilter: TComboBoxEx;
    CBEMapProvider: TComboBoxEx;
    CBEPOIList: TComboBoxEx;
    Chart1: TChart;
    fpWX: TFlowPanel;
    fpCharts: TFlowPanel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    ICallsignIcon: TImage;
    ilMessageStatus: TImage;
    MenuItem14: TMenuItem;
    Separator3: TMenuItem;
    shGPSDStatus: TShape;
    shRTLSDRStatus: TShape;
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
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    miSendPosition: TMenuItem;
    MenuItem2: TMenuItem;
    btnBuymeacoffee: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem9: TMenuItem;
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
    scData: TScrollBox;
    scWX: TScrollBox;
    scCharts: TScrollBox;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Settings: TMenuItem;
    MIFileExit: TMenuItem;
    SBMain: TStatusBar;
    Settings1: TMenuItem;
    sbSendMessageTo: TSpeedButton;
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
    tBake: TTimer;
    tRepeatSendMail: TTimer;
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
    procedure actGPSExecute(Sender: TObject);
    procedure actOpenLastseenExecute(Sender: TObject);
    procedure actSendPositionExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actShowHideExecute(Sender: TObject);
    procedure actShowMessagesExecute(Sender: TObject);
    procedure actUpdateModeSDatabaseExecute(Sender: TObject);
    procedure btnBuymeacoffeeClick(Sender: TObject);
    procedure CBEFilterSelect(Sender: TObject);
    procedure ChangeMapProvider(Sender: TObject);
    procedure FMainInit(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormHide(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MIKofiClick(Sender: TObject);
    procedure ShowModeSTracking(Sender: TObject);
    procedure ShowAISTracking(Sender: TObject);
    procedure MIInfoClick(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure mntDonateClick(Sender: TObject);
    procedure MVMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MVMapZoomChange(Sender: TObject);
    procedure pcPoITabChange(Sender: TObject);
    procedure sbSendMessageToClick(Sender: TObject);
    procedure sbShowRawMessagesClick(Sender: TObject);
    procedure SelectPOI(Sender: TObject);
    procedure ShowMapMousePosition(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SPTrackClick(Sender: TObject);
    procedure tBakeTimer(Sender: TObject);
    procedure TBZoomMapChange(Sender: TObject);
    procedure TMainLoopTimer(Sender: TObject);
    procedure AddCombobox(const msg: TAPRSMessage);
    procedure DeleteCombobox(const Call: String);
    procedure DelPoiByAge;
    procedure AddPoI(msg: TAPRSMessage);
    procedure SetupFilterCombo;
    procedure tRefreshTimer(Sender: TObject);
    procedure tRepeatSendMailTimer(Sender: TObject);
    procedure WriteChart(X: TDoubleList; Title, TitleX, TitleY: String; ParentCtrl: TWinControl);
    procedure UpdateWXCaption(msg: TAPRSMessage);
    procedure UpdateDevices(newMsg, oldMsg: PAPRSMessage);
    procedure ShowChartPopup(Sender: TObject);
    procedure UpdateGPSDStatus;
    procedure UpdateRTLSDRStatus;
    procedure ClearObjectReporterLink;
    procedure ShowObjectReporterLink(const Message: PAPRSMessage);
    function FindObjectReporter(const Message: PAPRSMessage): PAPRSMessage;
    function GetWXCaption(wx: TDoubleList; calc: String): String;
    function GetWXCaption(wx: TDoubleList): String;
    function TrackHasPoint(Track: TGPSPointList; const Lat, Lon: Double): Boolean;
  private
    MapRefreshPending: Boolean;
  public
    PoILayer, myPoILayer: TMapLayer;
    MyPosition: TGPSObj;
    MyPositionGPS: TGPSPoint;
    Debug: Boolean;
    IGate: TIGateThread;
    ModeS: TModeSThread;
    MeshCore: TMeshCoreClient;
    ObjectReporterLink: TGPSTrack;
    SendOutMessage: array of TMessage;
    procedure SendStringCommand(const Channel, Code: byte; const Command: String);
  end;

var
  FMain: TFMain;
  OrigWidth, OrigHeight: Integer;
  APRSMessageObject: PAPRSMessage;
  ReadPipe: TReadPipeThread;
  APRSConfig: TAPRSConfig;
  LastZoom: Byte;
  ModeSCount: Integer;
  TrackID: Integer;
  MeshCoreImageIndex: Integer;
  IsClosing: Boolean;
  chartScroll, wxScroll, dataScroll: Integer;

implementation

{$R *.lfm}

const
  ObjectReporterLinkId = -1001;
  MaxTrackPoints = 500;

procedure TObjectReporterLinkTrack.Draw(AView: TObject; {%H-}Area: TRealArea);
var
  MapView: TMapView;
  StartPoint, EndPoint: TPoint;
begin
  if not Visible or (Points.Count < 2) then
    Exit;

  MapView := TMapView(AView);
  StartPoint := MapView.LatLonToScreen(Points[0].RealPoint);
  EndPoint := MapView.LatLonToScreen(Points[1].RealPoint);
  MapView.DrawingEngine.StoreState;
  try
    MapView.DrawingEngine.SetPen(psDash, 2, clAqua);
    MapView.DrawingEngine.Line(StartPoint.X, StartPoint.Y, EndPoint.X, EndPoint.Y);
  finally
    MapView.DrawingEngine.RestoreState;
  end;
end;

{ TFMain }

procedure TFMain.FMainInit(Sender: TObject);
var Providers: TStringList;
    i, CountProvider: Byte;
    MeshCoreBitmap: TBitmap;
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

  MeshCoreImageIndex := 243;
  MeshCoreBitmap := TBitmap.Create;
  try
  finally
    MeshCoreBitmap.Free;
  end;

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

  ModeS := nil;
  if APRSConfig.ModeSEnabled or APRSConfig.AISEnabled then
    ModeS := TModeSThread.Create(@APRSConfig);
  UpdateRTLSDRStatus;

  IGate := nil;
  if APRSConfig.IGateEnabled then
    IGate := TIGateThread.Create(@APRSConfig);

  MeshCore := nil;
  if APRSConfig.MeshCoreEnabled and (Trim(APRSConfig.MeshCoreAddress) <> '') then
    MeshCore := TMeshCoreClient.Create(APRSConfig.MeshCoreAddress);

  // Init Pipe
  ReadPipe := nil;
  ReadPipe := TReadPipeThread.Create('flexpacketwritepipe');
  if ReadPipe.IsPipeExisting('flexpacketreadpipe') then
    SendStringCommand(0,1,'C APN000 V WIDE1');

  SetupFilterCombo;

  pcPoITab.ActivePage := tsMain;

  FGPS.Start(@APRSConfig);
  UpdateGPSDStatus;

  ilMessageStatus.ImageIndex := 241;

  // Minutes to milliseconds
  tBake.Interval := APRSConfig.AprsUpdateInterval * 60 * 1000
end;

procedure TFMain.UpdateRTLSDRStatus;
var
  StatusEnabled, Available: Boolean;
begin
  StatusEnabled := APRSConfig.ModeSEnabled or APRSConfig.AISEnabled;
  shRTLSDRStatus.Visible := StatusEnabled;
  if not StatusEnabled then Exit;

  Available := Assigned(ModeS) and not ModeS.Error;
  if Available then
  begin
    shRTLSDRStatus.Brush.Color := clLime;
    shRTLSDRStatus.Pen.Color := clGreen;
    shRTLSDRStatus.Hint := 'RTL-SDR hardware status: available';
  end
  else
  begin
    shRTLSDRStatus.Brush.Color := clRed;
    shRTLSDRStatus.Pen.Color := clMaroon;
    shRTLSDRStatus.Hint := 'RTL-SDR hardware status: unavailable';
  end;
end;

procedure TFMain.UpdateGPSDStatus;
var
  Available: Boolean;
begin
  Available := Assigned(GPSd) and GPSd.Connected;
  if Available then
  begin
    shGPSDStatus.Brush.Color := clLime;
    shGPSDStatus.Pen.Color := clGreen;
    shGPSDStatus.Hint := 'GPSD status: reachable';
  end
  else
  begin
    shGPSDStatus.Brush.Color := clGray;
    shGPSDStatus.Pen.Color := clMedGray;
    shGPSDStatus.Hint := 'GPSD status: unreachable';
  end;
end;

procedure TFMain.FormChangeBounds(Sender: TObject);
begin
  if (FLastseen.WindowState <> wsMinimized) and FLastseen.Visible then
  begin
    if Abs(FLastseen.Left - (FMain.Left + FMain.Width)) <= 100 then
    begin
      // Dock Lastseen window
      FLastseen.Left := FMain.Left + FMain.Width + 1;
      FLastseen.Top  := FMain.Top;
      FLastseen.Height  := FMain.Height+5;
    end;
  end;
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
  CBEFilter.ItemsEx.AddItem('MeshCore', MeshCoreImageIndex, 0, 0, 0, nil);
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
      ModeS.WaitFor;
      FreeAndNil(ModeS);
    end;
  except
  end;
  FreeAndNil(MeshCore);
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

procedure TFMain.actShowMessagesExecute(Sender: TObject);
begin
  FListMails.SetConfig(@APRSConfig);
  FListMails.Show;
  ilMessageStatus.ImageIndex := 241;
end;

procedure TFMain.actUpdateModeSDatabaseExecute(Sender: TObject);
begin
  try
    if EnsureAeromuxDatabase then
      ShowMessage('ModeS database updated.')
    else
      ShowMessage('Could not update the ModeS database.');
  finally
  end;
end;

procedure TFMain.ShowModeSTracking(Sender: TObject);
begin
  FModeSList.Show;
end;

procedure TFMain.ShowAISTracking(Sender: TObject);
begin
  FAISList.Show;
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

procedure TFMain.actSendPositionExecute(Sender: TObject);
begin
  actSendPosition.Checked := not(actSendPosition.Checked);

  tBake.Enabled := actSendPosition.Checked;

  if actSendPosition.Checked then
    tBakeTimer(Sender);
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

procedure TFMain.actGPSExecute(Sender: TObject);
begin
  FGPS.Show;
end;

procedure TFMain.CBEFilterSelect(Sender: TObject);
var description: String;
    i: Integer;
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
    for i := 1 to count - 1 do
    begin
      poi := PoiLayer.PointsOfInterest[i];
      if not Assigned(poi) then
        Continue;
      poi.Visible := False;
    end;

    // Show PoI's by filter
    for i := 1 to count - 1 do
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

procedure TFMain.sbSendMessageToClick(Sender: TObject);
begin
  if Length(STCallsign.Caption) < 0 then
    Exit;

  TFEditor.Show;
  TFEditor.leToCall.Caption := STCallsign.Caption;
  TFEditor.rbMTypeMessage.Checked := True;
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

procedure TFMain.ClearObjectReporterLink;
begin
  if Assigned(ObjectReporterLink) then
  begin
    MVMap.GPSItems.Delete(ObjectReporterLink);
    ObjectReporterLink := nil;
  end;
end;

function TFMain.FindObjectReporter(const Message: PAPRSMessage): PAPRSMessage;
var
  PathParts: TStringArray;
  Candidate: String;
  I: Integer;
begin
  Result := nil;
  if not Assigned(Message) then
    Exit;

  // APRS-IS packets identify the RF-side forwarding station after the qA
  // construct. Prefer a station from that WIDE/IGate path when it is known.
  if (Pos('QA', UpperCase(Message^.Path)) > 0) or
     (Pos('TCPIP', UpperCase(Message^.Path)) > 0) then
  begin
    PathParts := Message^.Path.Split(',');
    for I := High(PathParts) downto 0 do
    begin
      Candidate := Trim(StringReplace(PathParts[I], '*', '', [rfReplaceAll]));
      if (Candidate = '') or StartsText('QA', Candidate) then
        Continue;
      Result := APRSMessageList.Find(Candidate);
      if Assigned(Result) and (Result^.Latitude <> 0.0) and
         (Result^.Longitude <> 0.0) then
        Exit;
      Result := nil;
    end;
  end;

  if Message^.ReporterCall <> '' then
    Result := APRSMessageList.Find(Message^.ReporterCall);
end;

procedure TFMain.ShowObjectReporterLink(const Message: PAPRSMessage);
var
  Reporter: PAPRSMessage;
begin
  if not Assigned(Message) or (Message^.DataType <> ';') or
     (Message^.ReporterCall = '') or (Message^.Latitude = 0.0) or
     (Message^.Longitude = 0.0) then
    Exit;

  Reporter := FindObjectReporter(Message);
  if not Assigned(Reporter) or (Reporter = Message) or
     (Reporter^.Latitude = 0.0) or (Reporter^.Longitude = 0.0) then
    Exit;

  ObjectReporterLink := TObjectReporterLinkTrack.Create;
  ObjectReporterLink.Points.Add(TGPSPoint.Create(Message^.Longitude,
    Message^.Latitude, 0));
  ObjectReporterLink.Points.Add(TGPSPoint.Create(Reporter^.Longitude,
    Reporter^.Latitude, 0));
  MVMap.GPSItems.Add(ObjectReporterLink, ObjectReporterLinkId, MaxInt);
end;

// User select one PoI
procedure TFMain.SelectPOI(Sender: TObject);
var msg: PAPRSMessage;
    call: String;
    poiGPS: TGPSObj;
    i: Integer;
begin
  try
    ClearObjectReporterLink;
    MAPRSMessage.Lines.Clear;

    // Cleanup all Charts.
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
      ShowObjectReporterLink(msg);

      ICallSignIcon.ImageIndex := msg^.ImageIndex;
      MAPRSMessage.Lines.Add(msg^.Message);
      STCallsign.Caption := Call;
      STIconDescription.Caption := msg^.ImageDescription;
      if Length(msg^.MicEMessage) > 0 then
        STIconDescription.Caption := Format('%s - %s', [STIconDescription.Caption, msg^.MicEMessage]);

      if (Length(msg^.MicEMessage) > 0) and (Length(msg^.ImageDescription) <= 0) then
        STIconDescription.Caption := msg^.MicEMessage;

      STLatitude.Caption := LatToStr(msg^.Latitude, False);
      STLongitude.Caption := LonToStr(msg^.Longitude, False);
      STLatitudeDMS.Caption := LatToStr(msg^.Latitude, True);
      STLongitudeDMS.Caption := LonToStr(msg^.Longitude, True);

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

      if Assigned(msg^.Track) and (msg^.Track.Visible) then
        SPTrack.Down := True
      else
        SPTrack.Down := False;

      if Assigned(msg^.Speed) and (msg^.Speed.Count > 0) then
      begin
        WriteChart(msg^.Speed, 'Speed', 'Time', 'Speed (km/h)', fpCharts);
        STSpeed.Caption := FloatToStr(msg^.Speed.Last);
      end
      else
        STSpeed.Caption := '';


      if Assigned(msg^.Altitude) and (msg^.Altitude.Count > 0) then
      begin
        WriteChart(msg^.Altitude, 'Altitude', 'Time', 'Altitude (m)', fpCharts);
        STAltitude.Caption := FloatToStr(msg^.Altitude.Last);
      end
      else
        STAltitude.Caption := '';

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
    i, min: Integer;
    ChartName: String;
begin
  try
    if not Assigned(X) or not Assigned(ParentCtrl) then
      Exit;

    if (X.Count = 0) then
      Exit;

    if (ParentCtrl = nil) then
      Exit;
  except
  end;

  try
    ChartName := 'AutoGen_' + StringReplace(Title, ' ', '_', [rfReplaceAll]);

    // Check if chart with this name already exist.
    Chart := nil;
    for i := 0 to ParentCtrl.ControlCount - 1 do
    begin
      if (ParentCtrl.Controls[i] is TChart) and (TChart(ParentCtrl.Controls[i]).Name = ChartName) then
      begin
        Chart := TChart(ParentCtrl.Controls[i]);
        if Chart.Hint = STCallsign.Caption + ':' + IntToStr(X.Count) then
        begin
          Chart.Visible := True;
          Exit;
        end;
        Chart.ClearSeries;
        Chart.Visible := True;
        Break;
      end;
    end;

    // Create Chart
    if not Assigned(Chart) then
    begin
      Chart := TChart.Create(ParentCtrl);
      Chart.Parent := ParentCtrl;
      Chart.Align := alNone;
      Chart.Name := ChartName;
      Chart.Title.Text.Clear;
      Chart.Title.Text.Add(Title);
      Chart.Title.Visible := True;
      Chart.Width := 290;
    end;
    Chart.Hint := STCallsign.Caption + ':' + IntToStr(X.Count);

    // X
    min := 0;
    Chart.BottomAxis.Title.Caption := TitleX;
    Chart.BottomAxis.Title.Visible := True;
    if X.Count >= 100 then
    begin
      Chart.BottomAxis.Range.Min := X.Count - 100;
      min := X.Count - 100;
    end
    else
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
    Chart.OnDblClick :=  @ShowChartPopup;

    // Punkte einfüllen
    for i := min to X.Count - 1 do
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

procedure TFMain.ShowChartPopup(Sender: TObject);
var PopupForm: TForm;
    PopupChart, Chart: TChart;
    i: Integer;
begin
  if not (Sender is TChart) then
    Exit;

  Chart := Sender as TChart;

  PopupForm := TForm.Create(Parent);
  try
    PopupForm.Caption := Chart.Title.Text.Text;
    PopupForm.Width := 800;
    PopupForm.Height := 600;
    PopupForm.Position := poScreenCenter;

    PopupChart := TChart.Create(PopupForm);
    PopupChart.Parent := PopupForm;
    PopupChart.Align := alClient;

    for i := 0 to Chart.SeriesCount - 1 do
      PopupChart.AddSeries(Chart.Series[i]);

    PopupForm.ShowModal;
  finally
    PopupForm.Free;
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

// Send own Position
procedure TFMain.tBakeTimer(Sender: TObject);
var msg, lat, lon, MeshLine: String;
begin
  if (APRSConfig.Latitude > 0) and (APRSConfig.Longitude > 0) then
  begin
    lat := FormatLatitude(APRSConfig.Latitude);
    lon := FormatLongitude(APRSConfig.Longitude);
    msg := Format('!%s%s%s%s%s', [lat, GetImageTable(APRSConfig.AprsSymbol), lon, GetImageSymbol(APRSConfig.AprsSymbol), APRSConfig.AprsMessage]);

    SendStringCommand(APRSConfig.Channel, 0, msg);
    if APRSConfig.MeshCoreSendPosition and Assigned(MeshCore) then
    begin
      MeshLine := Format('%s>APRSMP,MESH*:%s', [APRSConfig.Callsign, msg]);
      MeshCore.SendChannelText(MeshLine);
    end;
  end;
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
        if Assigned(ModeS) then
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
  if APRSConfig.ModeSEnabled and Assigned(ModeS) and (ModeS.ModeSMessageList.Count > 0) then
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

  MapRefreshPending := True;
end;

// Add APRS message as PoI
procedure TFMain.AddPoI(msg: TAPRSMessage);
var newMSG, oldMSG: PAPRSMessage;
    poi: TGpsPoint;
    visibility: Boolean;
    Alt: Double;
begin
  try
    if Length(msg.FromCall) > 0 then
    begin
      New(newMSG);
      newMSG^ := msg;

      oldMSG := APRSMessageList.Find(msg.FromCall);

      // if not already exist, create TrackID, else reuse old TrackID and ImageIndex
      if not Assigned(oldMSG) then
      begin
        inc(TrackID);
        newMsg^.TrackID := TrackID;
        if Assigned(newMsg^.Track) and (newMsg^.TrackID > 0) then
          MVMap.GPSItems.Add(newMsg^.Track, newMsg^.TrackID);
      end
      else
      begin
        // Preserve old Data
        newMsg^.Track := oldMsg^.Track;
        newMsg^.Count := oldMsg^.Count;

        if newMsg^.Speed <> oldMsg^.Speed then
          PrependDoubleList(newMsg^.Speed, oldMsg^.Speed);
        if newMsg^.Altitude <> oldMsg^.Altitude then
          PrependDoubleList(newMsg^.Altitude, oldMsg^.Altitude);
        if newMsg^.WXTemperature <> oldMsg^.WXTemperature then
          PrependDoubleList(newMsg^.WXTemperature, oldMsg^.WXTemperature);
        if newMsg^.WXHumidity <> oldMsg^.WXHumidity then
          PrependDoubleList(newMsg^.WXHumidity, oldMsg^.WXHumidity);
        if newMsg^.WXPressure <> oldMsg^.WXPressure then
          PrependDoubleList(newMsg^.WXPressure, oldMsg^.WXPressure);
        if newMsg^.WXLum <> oldMsg^.WXLum then
          PrependDoubleList(newMsg^.WXLum, oldMsg^.WXLum);

        // PrependDoubleList for all Devices
        UpdateDevices(newMsg, oldMsg);

        if not newMsg^.ModeS and Assigned(newMsg^.RAWMessages) then
          newMsg^.RAWMessages.AddStrings(oldMsg^.RAWMessages);
      end;


      // how often we saw that call
      inc(newMsg^.Count);

      // update Raw Message window
      if FRawMessage.Visible and (Trim(STCallsign.Caption) = Trim(newMsg^.FromCall)) and
         Assigned(FRawMessage.mRawMessage) and Assigned(newMsg^.RAWMessages) then
        FRawMessage.mRawMessage.Lines.AddStrings(newMsg^.RAWMessages);

      if Assigned(newMsg^.Altitude) and (newMsg^.Altitude.Count > 0) then
        Alt := newMsg^.Altitude.Last
      else
        Alt := 0;

      if (newMsg^.Longitude <> 0.0) and (newMsg^.Latitude <> 0.0) and Assigned(newMsg^.Track) then
        if not TrackHasPoint(newMsg^.Track.Points, newMsg^.Latitude, newMsg^.Longitude) then
        begin
          newMsg^.Track.Points.Add(TGPSPoint.Create(newMsg^.Longitude, newMsg^.Latitude, Alt));
          while newMsg^.Track.Points.Count > MaxTrackPoints do
            newMsg^.Track.Points.Delete(0);
        end;

      // Filter is set
      visibility := True;
      if (FMain.CBEFilter.ItemIndex > 0) and not (newMsg^.ModeS) then
        if not SameText(FMain.CBEFilter.ItemsEx.Items[FMain.CBEFilter.ItemIndex].Caption, newMsg^.ImageDescription) then
          visibility := False;

      APRSMessageList.Add(newMsg^.FromCall, newMsg);
      SetPoi(PoILayer, newMsg, visibility);

      poi := TGpsPoint(FindGPSItem(PoILayer, newMsg^.FromCall));
      if Assigned(MyPositionGPS) and Assigned(poi) then
      begin
        newMsg^.Distance := poi.DistanceInKmFrom(MyPositionGPS,False);
        if newMsg^.Distance <= 0 then
          newMsg^.Distance := 0;
      end;

      FLastSeen.AddCallsign(newMsg);

      // add callsign to Combobox
      if not Assigned(oldMSG) then
        AddCombobox(newMsg^);

      MapRefreshPending := True;
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
    ModeSKey, AISKey: String;
    msg: PAPRSMessage;
    MeshMessage: TMeshCoreMessage;
    MeshNode: TMeshCoreNode;
    MeshWeather: TMeshCoreWeather;
    MeshAPRSMessage: TAPRSMessage;
begin
  DelPoIByAge;
  UpdateGPSDStatus;
  UpdateRTLSDRStatus;

  if APRSConfig.IGateEnabled and Assigned(IGate) then
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

  if Assigned(MeshCore) then
  begin
    try
      while MeshCore.TryDequeueAPRS(buffer) do
        AddPoI(DecodeAPRSLine(buffer));
      while MeshCore.TryDequeueMessage(MeshMessage) do
      begin
        if StoreMeshCoreMessage(@APRSConfig, MeshMessage) then
          ilMessageStatus.ImageIndex := 242
        else
          {$IFDEF UNIX}
          writeln('Error storing MeshCore message')
          {$ENDIF};
      end;
      while MeshCore.TryDequeueNode(MeshNode) do
      begin
        MeshAPRSMessage := InitAPRSMessage;
        MeshAPRSMessage.FromCall := MeshNode.Name;
        MeshAPRSMessage.Latitude := MeshNode.Latitude;
        MeshAPRSMessage.Longitude := MeshNode.Longitude;
        MeshAPRSMessage.Time := Now;
        MeshAPRSMessage.ImageIndex := MeshCoreImageIndex;
        MeshAPRSMessage.ImageDescription := 'MeshCore';
        MeshAPRSMessage.Message := 'MeshCore ' +
          MeshCoreNodeTypeName(MeshNode.Kind);
        AddPoI(MeshAPRSMessage);
      end;
      while MeshCore.TryDequeueWeather(MeshWeather) do
      begin
        msg := APRSMessageList.Find(MeshWeather.Sender);
        if not Assigned(msg) then
        begin
          MeshAPRSMessage := InitAPRSMessage;
          MeshAPRSMessage.FromCall := MeshWeather.Sender;
          MeshAPRSMessage.Time := Now;
          MeshAPRSMessage.ImageIndex := MeshCoreImageIndex;
          MeshAPRSMessage.ImageDescription := 'MeshCore';
          MeshAPRSMessage.Message := 'MeshCore Wetterdaten';
          if MeshWeather.HasTemperature then
            MeshAPRSMessage.WXTemperature.Add(MeshWeather.Temperature);
          if MeshWeather.HasHumidity then
            MeshAPRSMessage.WXHumidity.Add(MeshWeather.Humidity);
          if MeshWeather.HasPressure then
            MeshAPRSMessage.WXPressure.Add(MeshWeather.Pressure);
          if MeshWeather.HasIlluminance then
            MeshAPRSMessage.WXLum.Add(MeshWeather.Illuminance);
          AddPoI(MeshAPRSMessage);
        end
        else
        begin
          msg^.Time := Now;
          if MeshWeather.HasTemperature then
            msg^.WXTemperature.Add(MeshWeather.Temperature);
          if MeshWeather.HasHumidity then
            msg^.WXHumidity.Add(MeshWeather.Humidity);
          if MeshWeather.HasPressure then
            msg^.WXPressure.Add(MeshWeather.Pressure);
          if MeshWeather.HasIlluminance then
            msg^.WXLum.Add(MeshWeather.Illuminance);
          if SameText(Trim(STCallsign.Caption), Trim(msg^.FromCall)) then
            UpdateWXCaption(msg^);
        end;
      end;
    except
      on E: Exception do
      begin
        {$IFDEF UNIX}
        writeln('Error Main Loop MeshCore: ', E.Message);
        {$ENDIF}
      end;
    end;
  end;

  try
    if not ReadPipe.Error then
    begin
      buffer := ReadPipe.PipeData;
      if Length(buffer) > 0 then
        AddPoI(ReadPipe.DecodeAPRSMessage(buffer));
      ReadPipe.PipeData := '';
    end;
  except
    on E: Exception do
      {$IFDEF UNIX}
      writeln('Error Main Loop Receive Data Pipe: ', E.Message);
      {$ENDIF}
  end;

  if APRSConfig.ModeSEnabled and not ModeS.Error and Assigned(ModeS.ModeSMessageList) and
     Assigned(ModeS.ModeSUpdateQueue) then
  begin
    if ModeS.ModeSUpdateQueue.Count > 0 then
    begin
      try
        ModeSKey := ModeS.ModeSUpdateQueue[0];
        msg := PAPRSMessage(ModeS.ModeSMessageList.Find(ModeSKey));
        if Assigned(msg) then
        begin
          msg^.ModeS := True;
          AddPoI(msg^);
        end;
        ModeS.ModeSUpdateQueue.Delete(0);
      except
        on E: Exception do
        begin
          {$IFDEF UNIX}
          writeln('Error Main Loop ModeS: ', E.Message);
          {$ENDIF}
        end;
      end;
    end;
  end;
  if APRSConfig.AISEnabled and not ModeS.Error and Assigned(ModeS.AISMessageList) and
     Assigned(ModeS.AISUpdateQueue) then
  begin
    if ModeS.AISUpdateQueue.Count > 0 then
    begin
      try
        AISKey := ModeS.AISUpdateQueue[0];
        msg := PAPRSMessage(ModeS.AISMessageList.Find(AISKey));
        if Assigned(msg) then
        begin
          msg^.ModeS := False;
          AddPoI(msg^);
        end;
        ModeS.AISUpdateQueue.Delete(0);
      except
        on E: Exception do
        begin
          {$IFDEF UNIX}
          writeln('Error Main Loop AIS: ', E.Message);
          {$ENDIF}
        end;
      end;
    end;
  end;
  if MapRefreshPending then
  begin
    MapRefreshPending := False;
    MVMap.Refresh;
  end;
end;

// Check if track with given Points already exist
function TFMain.TrackHasPoint(Track: TGPSPointList; const Lat, Lon: Double): Boolean;
var P: TGPSPoint;
begin
  Result := False;

  if not Assigned(Track) or (Track.Count <= 0) then
    Exit;

  P := Track[Track.Count - 1];
  Result := (P.Lat = Lat) and (P.Lon = Lon);
end;

procedure TFMain.tRefreshTimer(Sender: TObject);
begin
  scWX.DisableAlign;
  scCharts.DisableAlign;
  scData.DisableAlign;

  try
    chartScroll := scCharts.VertScrollBar.Position;
    wxScroll := scWx.VertScrollBar.Position;
    dataScroll := scData.VertScrollBar.Position;

    SelectPoI(Sender);

    scWX.VertScrollBar.Position := wxScroll;
    scCharts.VertScrollBar.Position := chartScroll;
    scData.VertScrollBar.Position := dataScroll;

  finally
    scWX.EnableAlign;
    scCharts.EnableAlign;
    scData.EnableAlign;
  end;
end;

// Repeat Send unacklowleged Mail
procedure TFMain.tRepeatSendMailTimer(Sender: TObject);
var i: Integer;
    msg: String;
begin
  try
    for i:= 0 to Length(SendOutMessage) - 1 do
    begin
      if (Length(SendOutMessage[i].Text) > 0) and not (SendOutMessage[i].Ack) and (SendOutMessage[i].RepeatNr <= 10) then
      begin
        inc(SendOutMessage[i].RepeatNr);
        msg := Format(':%-9.9s:%s{%d', [SendOutMessage[i].ToCallsign, SendOutMessage[i].Text, SendOutMessage[i].Nr]);
        if Length(msg) > 0 then
          SendStringCommand(APRSConfig.Channel, 0, msg);
      end;
    end;
  except
    on E: Exception do
    begin
      {$IFDEF UNIX}
      writeln('Error tRepeatSendMailTimer: ', E.Message);
      {$ENDIF}
    end;
  end;
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
    if km < 0 then
      km := 0;
    if (msg.ImageIndex >= 0) and (msg.ImageIndex < ImageList1.Count) then
      CBEPOIList.ItemsEx.AddItem(msg.FromCall + ' > ' + IntToStr(Round(km)) + 'km' , msg.ImageIndex, 0, 0, 0, nil)
    else
      CBEPOIList.ItemsEx.AddItem(msg.FromCall + ' > ' + IntToStr(Round(km)) + 'km' , 0, 0, 0, 0, nil);
  except
    on E: Exception do
    begin
      {$IFDEF UNIX}
      writeln('Calculate Distance in km: ', E.Message);
      {$ENDIF}
    end;
  end;
end;


procedure TFMain.SendStringCommand(const Channel, Code: byte; const Command: String);
var msg: String;
begin
  if (Length(Command) <= 0) or not Assigned(ReadPipe) then
    Exit;

  // Channel Nr in FP | Message as Base64
  msg := Format('%d|%d|%s', [Channel,Code,EncodeStringBase64(Command)]);
  ReadPipe.WriteToPipe('flexpacketreadpipe', msg);
end;

end.

