unit ureadpipe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utypes, RegExpr, uaprs, {$IFDEF UNIX}BaseUnix{$ELSE}Windows{$ENDIF};

type
  TReadPipeThread = class(TThread)
  private
    FPipeName: string;
  protected
    procedure Execute; override;
  public
    PipeData: String;
    function DecodeAPRSMessage(const Data: String): TAPRSMessage;
    constructor Create(const PipeName: string);
  end;

var
  APRSMessageObject: TAPRSMessage;

implementation

{ TReadPipeThread }

constructor TReadPipeThread.Create(const PipeName: string);
begin
  inherited Create(True);
  FPipeName := PipeName;
  FreeOnTerminate := True;
  Start;
end;

procedure TReadPipeThread.Execute;
{$IFDEF UNIX}
var
  Pipe, i: Integer;
  Buffer: array[0..255] of Char;
  BytesRead: ssize_t;
  Text : String;
begin
  Pipe := FpOpen(PChar('/tmp/' + FPipeName), O_RDONLY);
  if Pipe < 0 then
  begin
    Writeln('Could not open Pipe to read: ', FPipeName);
    Exit;
  end;

  while not Terminated do
  begin
    BytesRead := FpRead(Pipe, Buffer, SizeOf(Buffer) - 1);
    if BytesRead > 0 then
    begin
      Text := '';
      for i := 0 to BytesRead do
      begin
        Text := Text + Buffer[i];
      end;
      PipeData := Text;
    end;
  end;
  FpClose(Pipe);
end;
{$ELSE}
var
  PipeHandle: THandle;
  Buffer: array[0..255] of Char;
  BytesRead: DWORD;
begin
  PipeHandle := CreateNamedPipe(
    PChar('\\.\pipe\' + FPipeName),
    PIPE_ACCESS_INBOUND,
    PIPE_TYPE_BYTE or PIPE_WAIT,
    1,
    0,
    0,
    0,
    nil
  );

  if PipeHandle = INVALID_HANDLE_VALUE then
  begin
    Writeln('Could not create Pipe to read: ', FPipeName);
    Exit;
  end;

  try
    if ConnectNamedPipe(PipeHandle, nil) or (GetLastError = ERROR_PIPE_CONNECTED) then
    begin
      while not Terminated do
      begin
        FillChar(Buffer, SizeOf(Buffer), 0);
        if ReadFile(PipeHandle, Buffer, SizeOf(Buffer) - 1, BytesRead, nil) then
          Synchronize(@ProcessData, string(Buffer))
        else
        begin
          Writeln('Error reading from pipe or pipe closed.');
          Break;
        end;
      end;
    end;
  finally
    CloseHandle(PipeHandle);
  end;
end;
{$ENDIF}

function TReadPipeThread.DecodeAPRSMessage(const Data: String): TAPRSMessage;
var Regex: TRegExpr;
    Lat, Lon: Double;
const
    PositionType = '!=/@zh';
begin
  Regex := TRegExpr.Create;
  try
    //Regex.Expression := '^.*?Fm ([A-Z0-9]{1,6}(?:-[0-9]{1,2})?) To ([A-Z0-9]{1,6})(?: Via ([A-Z0-9,-]+))? .*?>\[(\d{2}:\d{2}:\d{2})\].?\s*(.+)$';
    Regex.Expression := '^.*?Fm\s(\S+)\sTo\s(\S+)\s(?:Via\s(\S+))? .*UI(?:[v]{0,1})\spid(?:[=|\s]{0,1})F0.*([!|=|\/@|z|h]{1})(\d{4}\.\d{2}[N|S])(.)(\d{5}\.\d{2}[E|W])(.)(.+)$';
    Regex.ModifierI := False;
    if Regex.Exec(Data) then
    begin
      // FromCall: String;
      // ToCall: String;
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
  finally
    Regex.Free;
  end;
end;

end.

