unit uigate;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Sockets, utypes, netdb, RegExpr, uaprs;

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
        APRSMessageObject.Time := now();
        APRSMessageObject.Track := False;
      end;
      Result := APRSMessageObject;
    end;
  finally
    Regex.Free;
  end;

end;

end.

