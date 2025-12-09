unit ureadpipe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utypes, RegExpr, uaprs, base64,
  {$IFDEF UNIX}BaseUnix{$ELSE}Windows{$ENDIF};

type
  TReadPipeThread = class(TThread)
  private
    FPipeName: string;
  protected
    procedure Execute; override;
  public
    PipeData: String;
    Error: Boolean;
    function DecodeAPRSMessage(const Data: String): TAPRSMessage;
    constructor Create(const PipeName: string);
  end;

var
  APRSMessageObject: TAPRSMessage;

implementation

uses
  UMain;

{ TReadPipeThread }

constructor TReadPipeThread.Create(const PipeName: string);
begin
  inherited Create(True);
  FPipeName := PipeName;
  FreeOnTerminate := True;
  Error := False;
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
    Error := True;
    Exit;
  end;

  for i := 0 to High(Buffer) do
      Buffer[i] := #0;

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
    sleep(100);
  end;
  FpClose(Pipe);
end;
{$ELSE}
var
  PipeHandle: THandle;
  Buffer: array[0..255] of Char;
  BytesRead: DWORD;
  Text: string;
  i: Integer;

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
    {$IFDEF UNIX}
    Writeln('Could not create Pipe to read: ', FPipeName);
    {$ENDIF}
    Error := True;
    Exit;
  end;

  try
    if ConnectNamedPipe(PipeHandle, nil) or (GetLastError = ERROR_PIPE_CONNECTED) then
    begin
      while not Terminated do
      begin
        if ReadFile(PipeHandle, Buffer, SizeOf(Buffer) - 1, BytesRead, nil) then
        begin
          if BytesRead > 0 then
          begin
            Text := '';
            for i := 0 to BytesRead - 1 do
            begin
              Text := Text + Buffer[i];
            end;
            PipeData := Text;
          end;
        end
        else
        begin
          // Fehlerbehandlung bei ReadFile
          if GetLastError() = ERROR_BROKEN_PIPE then
          begin
            // Die Pipe wurde geschlossen
            Break;
          end;
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
    PRData, DataType, DataMessage: String;
    msg: TStringArray;
    Channel: Integer;
begin

  msg := Data.Split('|');
  if Length(msg) < 2 then
  begin
    Channel := StrToInt(msg[0]);
    PRData := DecodeStringBase64(msg[1]);

    if Pos('<UI', PRData) >= 0 then
    begin
      Regex := TRegExpr.Create;
      Regex.Expression := '^.*?Fm\s(\S+)\sTo\s(\S+)\s(?:Via\s(\S+))? <UI pid=F0.*(?:\[(\d{2}:\d{2}:\d{2})\]){1}(.*)';
      Regex.ModifierI := False;
      if Regex.Exec(PRData) then
        if Regex.SubExprMatchCount >= 5 then
        begin
          DataMessage := Regex.Match[5];
          DataMessage := StringReplace(DataMessage, #13, ' ', [rfReplaceAll]);
        end;
    end;

//      if Pos('ctl UI', PRData) >= 0 then
//      begin
//        Regex.Expression := '^.*?fm\s(\S+)\sto\s(\S+)\s(?:via\s(.*))? ctl UI(?:(\S){1})? pid F0?';
//        Regex.ModifierI := False;
//        if Regex.Exec(Data) then
//          if Regex.SubExprMatchCount >= 3 then
//            APRSHeader := Regex.Match[1]+'|'+Regex.Match[2]+'|'+Regex.Match[3];
//
//        Regex.Expression := '^([!=\/@;#*)_:>]{1})(.*)$';
//        Regex.ModifierI := False;
//        if Regex.Exec(PRData) then
//        begin
//          DataMessage := APRSHeader + '|' + Data;
//          DataMessage := StringReplace(APRSMsg, #13, ' ', [rfReplaceAll]);
//          WriteToPipe('flexpacketaprspipe', APRSMsg);
//          APRSHeader := '';
//        end;
//      end;


    Result := Default(TAPRSMessage);
    Regex := TRegExpr.Create;
    try
      // check type
      if Length(DataMessage) <= 0 then
        Exit;
      Regex.Expression := '(?:\s)?([!=\/@;#*)_:>]{1})(.*)';
      Regex.ModifierI := False;
      if Regex.Exec(DataMessage) then
      begin
        // check if type is a position type
        DataType := Regex.Match[1];
        DataMessage := Regex.Match[2];
        {$IFDEF UNIX}
        if FMain.Debug then
          writeln(DataMessage);
        {$ENDIF}
        Result := GetAPRSMessageObject(Data, DataType, DataMessage);
      end;
    finally
      Regex.Free;
    end;
  end;
end;

end.

