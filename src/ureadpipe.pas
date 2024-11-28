unit ureadpipe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$IFDEF UNIX}BaseUnix{$ELSE}Windows{$ENDIF};

type
  TReadPipeThread = class(TThread)
  private
    FPipeName: string;
  protected
    procedure Execute; override;
  public
    PipeData: String;
    constructor Create(const PipeName: string);
  end;

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



end.

