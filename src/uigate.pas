unit uigate;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Sockets, utypes,{$IFDEF UNIX}netdb{$ELSE}WinSock, Windows{$ENDIF}, RegExpr, uaprs;

type
  PTAPRSConfig = ^TAPRSConfig;

  { TIGateThread }
  TIGateThread = class(TThread)
  private
    FConfig: PTAPRSConfig;
    FSocket: TSocket;
    function IsValidIPAddress(const IP: string): Boolean;
  protected
    procedure Execute; override;
  public
    APRSBuffer: String;
    procedure Disconnect;
    function DecodeAPRSMessage(const Data: String): TAPRSMessage;
    constructor Create(Config: PTAPRSConfig);
    destructor Destroy; override;
  end;


var
  APRSMessageObject: TAPRSMessage;

implementation

{ TIGateThread }

constructor TIGateThread.Create(Config: PTAPRSConfig);
begin
  inherited Create(True);
  FConfig := Config;
  FreeOnTerminate := True;
  Start;
end;

destructor TIGateThread.Destroy;
begin
  Disconnect;
end;

procedure TIGateThread.Disconnect;
begin
  if FSocket <> -1 then
  begin
    fpShutdown(FSocket, SHUT_RDWR);
    FSocket := -1;
  end;
end;

{$IFDEF UNIX}
procedure TIGateThread.Execute;
var
  Addr: TInetSockAddr;
  Data: array[0..1023] of Char;
  LoginString, Response, Line: string;
  BytesRead, SockState, i: Integer;
  Lines: TStringList;
  Host : Array [1..10] of THostAddr;
begin
  FSocket := fpSocket(AF_INET, SOCK_STREAM, 0);
  if FSocket = -1 then
  begin
    write('Failed to create socket.');
    Exit;
  end;

  try
    Addr.sin_family := AF_INET;
    Addr.sin_port := htons(FConfig^.IGatePort);

    if IsValidIPAddress(FConfig^.IGateServer) then
      Addr.sin_addr := StrToHostAddr(FConfig^.IGateServer)
    else
    begin
      i := ResolveName(FConfig^.IGateServer, Host);
      if i = 0 then
      begin
        writeln('Cannot Resolve '+FConfig^.IGateServer);
        Exit;
      end;
      Addr.sin_addr := Host[1];
    end;

    SockState := fpConnect(FSocket, @Addr, SizeOf(Addr));
    if SockState < 0 then
    begin
      fpShutdown(FSocket,  SHUT_RDWR);
      write('Failed to connect to AGWPE server: ' + IntToStr(SockState));
      Exit;
    end;

    LoginString := Format('user %s pass %s vers aprsmap/flexpacket v0.1.0 filter r/%.2f/%.2f/100'#10,
      [FConfig^.Callsign, FConfig^.IGatePassword, FConfig^.Latitude, FConfig^.Longitude]);

    fpSend(FSocket, PChar(LoginString), Length(LoginString), 0);

    while not Terminated do
    begin
      BytesRead := fpRecv(FSocket, @Data, SizeOf(Data) - 1, 0);
      if BytesRead <= 0 then
        Break;

      Data[BytesRead] := #0; // Null-Terminierung
      Response := StrPas(Data);

      Lines := TStringList.Create;
      try
        Lines.Text := Response;
        for Line in Lines do
        begin
          if Length(Trim(Line)) > 0 then
            APRSBuffer := Line;
        end;
      finally
        Lines.Free;
      end;
      sleep(10);
    end;
  finally
    Disconnect;
  end;
end;
{$ELSE}
procedure TIGateThread.Execute;
var
  WSAData: TWSAData;
  Addr: TSockAddrIn;
  Data: array[0..1023] of Char;
  HostEnt: PHostEnt;
  LoginString, Response, Line: string;
  BytesRead, SockState: Integer;
  Lines: TStringList;
begin
  // WinSock initialisieren
  if WSAStartup($0202, WSAData) <> 0 then
  begin
    writeln('Failed to initialize WinSock.');
    Exit;
  end;

  try
    FSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if FSocket = INVALID_SOCKET then
    begin
      {$IFDEF UNIX}
      writeln('Failed to create socket.');
      {$ENDIF}
      WSACleanup();
      Exit;
    end;

    try
      if IsValidIPAddress(FConfig^.IGateServer) then
      begin
        Addr.sin_family := AF_INET;
        Addr.sin_port := htons(FConfig^.IGatePort);
        Addr.sin_addr.S_addr := inet_addr(PAnsiChar(FConfig^.IGateServer));
      end
      else
      begin
        HostEnt := gethostbyname(PAnsiChar(FConfig^.IGateServer));
        if HostEnt = nil then
        begin
          {$IFDEF UNIX}
          writeln('Cannot resolve ', FConfig^.IGateServer);
          {$ENDIF}
          closesocket(FSocket);
          WSACleanup();
          Exit;
        end;

        Addr.sin_family := AF_INET;
        Addr.sin_port := htons(FConfig^.IGatePort);
        Addr.sin_addr := PInAddr(HostEnt^.h_addr_list^)^;
      end;

      SockState := connect(FSocket, TSockAddr(Addr), SizeOf(Addr));
      if SockState = SOCKET_ERROR then
      begin
        {$IFDEF UNIX}
        writeln('Failed to connect to IGate server.');
        {$ENDIF}
        closesocket(FSocket);
        WSACleanup();
        Exit;
      end;

      // Login-String senden
      LoginString := Format('user %s pass %s vers aprsmap/flexpacket v0.1.0 filter r/%.2f/%.2f/100'#10,
        [FConfig^.Callsign, FConfig^.IGatePassword, FConfig^.Latitude, FConfig^.Longitude]);

      send(FSocket, LoginString[1], Length(LoginString), 0);

      // Daten empfangen
      while not Terminated do
      begin
        BytesRead := recv(FSocket, Data, SizeOf(Data) - 1, 0);
        if BytesRead <= 0 then
          Break;

        Data[BytesRead] := #0; // Null-Terminierung
        Response := StrPas(Data);

        Lines := TStringList.Create;
        try
          Lines.Text := Response;
          for Line in Lines do
          begin
            if Length(Trim(Line)) > 0 then
              APRSBuffer := Line;
          end;
        finally
          Lines.Free;
        end;

        Sleep(10);
      end;
    finally
      closesocket(FSocket); // Socket schließen
    end;
  finally
    WSACleanup(); // WinSock aufräumen
  end;
end;

{$ENDIF}

function TIGateThread.IsValidIPAddress(const IP: string): Boolean;
var
  Parts: TStringArray;
  PartValue, I: Integer;
begin
  Result := False;

  Parts := IP.Split(['.']);

  if Length(Parts) <> 4 then
    Exit;

  for I := 0 to High(Parts) do
  begin
    if not TryStrToInt(Parts[I], PartValue) then
      Exit;

    if (PartValue < 0) or (PartValue > 255) then
      Exit;
  end;

  Result := True;
end;

function TIGateThread.DecodeAPRSMessage(const Data: String): TAPRSMessage;
var Regex: TRegExpr;
    Lat, Lon: Double;
const
    PositionType = '!=/@zh';
begin
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^(\S+)>(\S+),(?:TCPIP).*([!|=|\/@|z|h]{1})(\d{4}\.\d{2}[N|S])(.)(\d{5}\.\d{2}[E|W])(.)(.+)$';
    Regex.ModifierI := False;
    if Regex.Exec(Data) then
    begin
      // FromCall: String;
      // ToCall: String;
      // IconPrimary: Double;
      // IconSecondary: Double;
      // Path: String;
      // Longitude: Double;
      // Latitude: Double;
      // Message: String;
      // Time: String
      APRSMessageObject.FromCall := Trim(Regex.Match[1]);
      APRSMessageObject.ToCall := Trim(Regex.Match[2]);
      APRSMessageObject.DataType := Regex.Match[3];

      if Pos(APRSMessageObject.DataType, PositionType) > 0 then
      begin
        ConvertNMEAToLatLong(Regex.Match[4], Regex.Match[6], Lat, Lon, 1);
        APRSMessageObject.Latitude := Lat;
        APRSMessageObject.Longitude := Lon;
        APRSMessageObject.IconPrimary := Regex.Match[5];
        APRSMessageObject.Icon := Regex.Match[7];
        APRSMessageObject.Message := Regex.Match[8];
        APRSMessageObject.Altitude := GetAltitude(APRSMessageObject.Message);
        APRSMessageObject.Course := GetCourse(APRSMessageObject.Message);
        APRSMessageObject.Speed := GetSpeed(APRSMessageObject.Message);
        APRSMessageObject.PHGPower := GetPHGPower(APRSMessageObject.Message);
        APRSMessageObject.PHGHeight := GetPHGHeight(APRSMessageObject.Message);
        APRSMessageObject.PHGGain := GetPHGGain(APRSMessageObject.Message);
        APRSMessageObject.PHGDirectivity := GetPHGDirectivity(APRSMessageObject.Message);
        APRSMessageObject.DFSStrength := GetDFSStrength(APRSMessageObject.Message);
        APRSMessageObject.DFSHeight := GetDFSHeight(APRSMessageObject.Message);
        APRSMessageObject.DFSGain := GetDFSGain(APRSMessageObject.Message);
        APRSMessageObject.DFSDirectivity := GetDFSDirectivity(APRSMessageObject.Message);
        APRSMessageObject.RNGRange := GetRNG(APRSMessageObject.Message);

        if (APRSMessageObject.Icon = '_') or (APRSMessageObject.Icon = '@') or (APRSMessageObject.Icon = 'w') then
        begin
          APRSMessageObject.WXDirection := StrToInt(GetWX(APRSMessageObject.Message,'c'));
          APRSMessageObject.WXSpeed := Round(StrToInt(GetWX(APRSMessageObject.Message,'s'))*1.85);
          APRSMessageObject.WXGust := Round(StrToInt(GetWX(APRSMessageObject.Message,'g'))*1.85);
          APRSMessageObject.WXTemperature := Round((StrToInt(GetWX(APRSMessageObject.Message,'t')) - 32)*5/9);
          APRSMessageObject.WXRainFall1h := Round(StrToInt(GetWX(APRSMessageObject.Message,'r'))*25.4);
          APRSMessageObject.WXRainFall24h := Round(StrToInt(GetWX(APRSMessageObject.Message,'p'))*25.4);
          APRSMessageObject.WXRainFallToday := Round(StrToInt(GetWX(APRSMessageObject.Message,'P'))*25.4);
          APRSMessageObject.WXHumidity := StrToInt(GetWX(APRSMessageObject.Message,'h'));
          APRSMessageObject.WXPressure := Round(StrToInt(GetWX(APRSMessageObject.Message,'b'))/10);
          APRSMessageObject.WXLum := StrToInt(GetWX(APRSMessageObject.Message,'L'));
          APRSMessageObject.WXSnowFall := StrToInt(GetWX(APRSMessageObject.Message,'s'));
          APRSMessageObject.WXRainCount := StrToInt(GetWX(APRSMessageObject.Message,'#'));
        end;

        APRSMessageObject.Time := now();
        APRSMessageObject.Track := False;
      end;
      Result := APRSMessageObject;
    end;
  except
    on E: Exception do
    begin
       {$IFDEF UNIX}
       writeln('APS Data Error: ', E.Message)
       {$ENDIF}
    end;
  end;

end;

end.

