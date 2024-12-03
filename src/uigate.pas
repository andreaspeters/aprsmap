unit uigate;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Sockets, utypes, netdb, RegExpr, uaprs, mvTypes, mvEngine,
  mvMapViewer;

type
  PTAPRSConfig = ^TAPRSConfig;

  { TIGateThread }
  TIGateThread = class(TThread)
  private
    FConfig: PTAPRSConfig;
    FSocket: TSocket;
    procedure DecodeAPRSMessage(const Data: String);
    function IsValidIPAddress(const IP: string): Boolean;
  protected
    procedure Execute; override;
  public
    procedure Disconnect;
    constructor Create(Config: PTAPRSConfig; Map: TMapView);
    destructor Destroy; override;
  end;


var
  APRSMessageObject: PAPRSMessage;
  FMap: TMapView;

implementation

{ TIGateThread }

constructor TIGateThread.Create(Config: PTAPRSConfig; Map: TMapView);
begin
  inherited Create(True);
  FConfig := Config;
  FMap := Map;
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

    LoginString := Format('user %s pass %s vers aprsmap/flexpacket v0.1.0 filter r/49.0/8.4/100'#10,
      [FConfig^.Callsign, FConfig^.IGatePassword]);
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
          if Trim(Line) <> '' then
          begin
            DecodeAPRSMessage(line);
          end;
        end;
      finally
        Lines.Free;
      end;
    end;
  finally
    Disconnect;
  end;
end;

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

procedure TIGateThread.DecodeAPRSMessage(const Data: String);
var Regex: TRegExpr;
    Lat, Lon: Double;
begin
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^(\S+)>(\S+),(?:TCPIP).*:!(\d{4}\.\d{2}\w.*)';
    Regex.ModifierI := False;
    if Regex.Exec(Data) then
    begin
      WriteLn('Source: ', Regex.Match[1]);
      WriteLn('Destination: ', Regex.Match[2]);
      WriteLn('Payload: ', Regex.Match[3]);
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

      Regex.Expression := '^(\d{4}\.\d{2}\w)\S(\d{5}\.\d{2}\w)(\w)(.+)$';
      if Regex.Exec(Regex.Match[3]) then
      begin
        ConvertNMEAToLatLong(Regex.Match[1], Regex.Match[2], Lat, Lon, 1);
        APRSMessageObject^.Latitude := Lat;
        APRSMessageObject^.Longitude := Lon;
        APRSMessageObject^.Message := Regex.Match[4];

        // Normalisierung der Ergebnisse
        WriteLn('Latitude: ',  LatToStr(Lat, False));
        WriteLn('Longitude: ', LonToStr(Lon, False));
        WriteLn('Type/Icon: ', Regex.Match[3]);
        WriteLn('Message: ', Regex.Match[4]);
        SetPoi(APRSMessageObject, FMap.GPSItems);
      end;
    end;
  finally
    Regex.Free;
  end;
end;
end.

