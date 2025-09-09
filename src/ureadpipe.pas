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
    DataType, DataMessage: String;
begin
  Regex := TRegExpr.Create;
  try
    // check type
    if Length(Data) <= 0 then
      Exit;
    Regex.Expression := '(?:\s)?([!=\/@;#*)_:>]{1})(.*)';
    Regex.ModifierI := False;
    if Regex.Exec(Data.Split('|')[3]) then
    begin
      // check if type is a position type
      DataType := Regex.Match[1];
      DataMessage := Regex.Match[2];
      {$IFDEF UNIX}
      writeln(Data);
      {$ENDIF}
      Result := GetAPRSMessageObject(Data, DataType, DataMessage);
    end;
  finally
    Regex.Free;
  end;
end;

end.

