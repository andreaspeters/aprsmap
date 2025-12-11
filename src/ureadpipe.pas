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
  APRSHeader: String;

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
var Pipe: Integer;
    Buffer: array[0..255] of Char;
    BytesRead: ssize_t;
    Text : String;
begin
  repeat
    Pipe := FpOpen(PChar('/tmp/' + FPipeName), O_RDONLY);
    if Pipe < 0 then
    begin
      Writeln('Could not open Pipe to read: ', FPipeName);
      Exit;
    end;
    repeat
      BytesRead := FpRead(Pipe, Buffer, SizeOf(Buffer));
      if BytesRead > 0 then
      begin
        // Convert the bytes that were actually read into a string.
        SetLength(Text, BytesRead);
        Move(Buffer[0], Text[1], BytesRead);  // faster than Text := Text + Buffer[i];
        PipeData := Text;
      end;
      // Wenn BytesRead = 0 bedeutet, dass der Schreiber die Pipe geschlossen hat.
    until BytesRead = 0;  // EOF erreicht

    FpClose(Pipe);
    // Warten bis ein neuer Schreiber die Pipe erneut öffnet.
    Sleep(100);
  until Terminated;
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
  Channel := 0;

  msg := Data.Split('|');
  if Length(msg) = 2 then
  begin
    Channel := StrToInt(msg[0]);
    PRData := DecodeStringBase64(msg[1]);

    if (Pos('<UI', PRData) > 0) or (Pos('ctl UI', PRData) > 0) then
      APRSHeader := PRData;

    Regex := TRegExpr.Create;
    try
      Regex.Expression := '(?:\s)?([!=\/@;#*)_:>]{1})(.*)';
      Regex.ModifierI := False;
      if Regex.Exec(PRData) then
      begin
        if Regex.SubExprMatchCount <> 2 then
          Exit;

        // check if type is a position type
        DataType := Regex.Match[1];
        DataMessage := Regex.Match[2];

        if Pos(DataMessage, APRSHeader) <= 0 then
          PRData := NormalizeString(Format('%s %s', [APRSHeader, PRData]));

        PRData := StringReplace(PRData, #13#10, '', [rfReplaceAll]);
        writeln(PRData);
        {$IFDEF UNIX}
        if FMain.Debug then
          writeln(PRData);
        {$ENDIF}
        Result := GetAPRSMessageObject(PRData, DataType, DataMessage);
      end;
    finally
      Regex.Free;
    end;
  end;
end;

end.

