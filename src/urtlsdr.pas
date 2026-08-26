unit urtlsdr;

{$mode objfpc}{$H+}
{$calling cdecl}

interface

uses
  SysUtils;

type
  TRtlSdrDev = Pointer;
  TRtlSdrReadAsyncCB = procedure(Buf: PByte; Len: Cardinal; Ctx: Pointer); cdecl;

function RtlSdrGetDeviceCount: Cardinal;
function RtlSdrOpen(var Dev: TRtlSdrDev; Index: Cardinal): Integer;
function RtlSdrClose(Dev: TRtlSdrDev): Integer;
function RtlSdrSetCenterFreq(Dev: TRtlSdrDev; Freq: Cardinal): Integer;
function RtlSdrSetSampleRate(Dev: TRtlSdrDev; Rate: Cardinal): Integer;
function RtlSdrSetTunerGainMode(Dev: TRtlSdrDev; Manual: Integer): Integer;
function RtlSdrSetAgcMode(Dev: TRtlSdrDev; On1: Integer): Integer;
function RtlSdrResetBuffer(Dev: TRtlSdrDev): Integer;
function RtlSdrReadSync(Dev: TRtlSdrDev; var Buf; Len: Integer; var NRead: Integer): Integer;
function RtlSdrReadAsync(Dev: TRtlSdrDev; Callback: TRtlSdrReadAsyncCB;
  Context: Pointer; BufferCount, BufferLength: Cardinal): Integer;
function RtlSdrCancelAsync(Dev: TRtlSdrDev): Integer;

implementation

uses
  Classes, DynLibs;

type
  TGetDeviceCount = function: Cardinal; cdecl;
  TOpen = function(var Dev: TRtlSdrDev; Index: Cardinal): Integer; cdecl;
  TClose = function(Dev: TRtlSdrDev): Integer; cdecl;
  TSetCenterFreq = function(Dev: TRtlSdrDev; Freq: Cardinal): Integer; cdecl;
  TSetSampleRate = function(Dev: TRtlSdrDev; Rate: Cardinal): Integer; cdecl;
  TSetTunerGainMode = function(Dev: TRtlSdrDev; Manual: Integer): Integer; cdecl;
  TSetAgcMode = function(Dev: TRtlSdrDev; On1: Integer): Integer; cdecl;
  TResetBuffer = function(Dev: TRtlSdrDev): Integer; cdecl;
  TReadSync = function(Dev: TRtlSdrDev; Buf: Pointer; Len: Integer; var NRead: Integer): Integer; cdecl;
  TReadAsync = function(Dev: TRtlSdrDev; Callback: TRtlSdrReadAsyncCB;
    Context: Pointer; BufferCount, BufferLength: Cardinal): Integer; cdecl;
  TCancelAsync = function(Dev: TRtlSdrDev): Integer; cdecl;

var
  RTLHandle: TLibHandle = NilHandle;
  RTLLoadAttempted: Boolean = False;
  FnGetDeviceCount: TGetDeviceCount;
  FnOpen: TOpen;
  FnClose: TClose;
  FnSetCenterFreq: TSetCenterFreq;
  FnSetSampleRate: TSetSampleRate;
  FnSetTunerGainMode: TSetTunerGainMode;
  FnSetAgcMode: TSetAgcMode;
  FnResetBuffer: TResetBuffer;
  FnReadSync: TReadSync;
  FnReadAsync: TReadAsync;
  FnCancelAsync: TCancelAsync;

function LoadRTL: Boolean;
const
  {$IFDEF WINDOWS}
  LIBRARIES: array[0..1] of PChar = ('rtlsdr.dll', 'librtlsdr.dll');
  {$ELSE}
  LIBRARIES: array[0..1] of PChar = ('librtlsdr.so.0', 'librtlsdr.so');
  {$ENDIF}
var
  {$IFDEF UNIX}
  I, J: Integer;
  LibraryPaths: TStringList;
  {$ENDIF}
  {$IFDEF WINDOWS}
  I: Integer;
  {$ENDIF}
  function Symbol(const Name: PChar): Pointer;
  begin
    Result := GetProcedureAddress(RTLHandle, Name);
  end;
begin
  if RTLLoadAttempted then
    Exit(RTLHandle <> NilHandle);
  RTLLoadAttempted := True;

  {$IFDEF UNIX}
  // Linux builds load only from the standard library locations, never Nix.
  LibraryPaths := TStringList.Create;
  try
    LibraryPaths.Add('/usr/local/lib');
    LibraryPaths.Add('/usr/local/lib64');
    LibraryPaths.Add('/usr/lib');
    LibraryPaths.Add('/usr/lib64');
    LibraryPaths.Add('/lib');
    LibraryPaths.Add('/lib64');
    LibraryPaths.Add('/usr/lib/x86_64-linux-gnu');
    LibraryPaths.Add('/lib/x86_64-linux-gnu');
    LibraryPaths.Add('/usr/lib/aarch64-linux-gnu');
    LibraryPaths.Add('/lib/aarch64-linux-gnu');
    LibraryPaths.Add('/usr/lib/arm-linux-gnueabihf');
    LibraryPaths.Add('/lib/arm-linux-gnueabihf');
    LibraryPaths.Add('/usr/lib/i386-linux-gnu');
    LibraryPaths.Add('/lib/i386-linux-gnu');
    for I := 0 to LibraryPaths.Count - 1 do
    begin
      for J := Low(LIBRARIES) to High(LIBRARIES) do
      begin
        RTLHandle := LoadLibrary(PChar(IncludeTrailingPathDelimiter(
          LibraryPaths[I]) + String(LIBRARIES[J])));
        if RTLHandle <> NilHandle then
          Break;
      end;
      if RTLHandle <> NilHandle then
        Break;
    end;
  finally
    LibraryPaths.Free;
  end;
  {$ELSE}
  for I := Low(LIBRARIES) to High(LIBRARIES) do
  begin
    RTLHandle := LoadLibrary(LIBRARIES[I]);
    if RTLHandle <> NilHandle then
      Break;
  end;
  {$ENDIF}
  if RTLHandle = NilHandle then Exit(False);
  Pointer(FnGetDeviceCount) := Symbol('rtlsdr_get_device_count');
  Pointer(FnOpen) := Symbol('rtlsdr_open');
  Pointer(FnClose) := Symbol('rtlsdr_close');
  Pointer(FnSetCenterFreq) := Symbol('rtlsdr_set_center_freq');
  Pointer(FnSetSampleRate) := Symbol('rtlsdr_set_sample_rate');
  Pointer(FnSetTunerGainMode) := Symbol('rtlsdr_set_tuner_gain_mode');
  Pointer(FnSetAgcMode) := Symbol('rtlsdr_set_agc_mode');
  Pointer(FnResetBuffer) := Symbol('rtlsdr_reset_buffer');
  Pointer(FnReadSync) := Symbol('rtlsdr_read_sync');
  Pointer(FnReadAsync) := Symbol('rtlsdr_read_async');
  Pointer(FnCancelAsync) := Symbol('rtlsdr_cancel_async');
  Result := Assigned(FnGetDeviceCount) and Assigned(FnOpen) and Assigned(FnClose) and
            Assigned(FnSetCenterFreq) and Assigned(FnSetSampleRate) and
            Assigned(FnSetTunerGainMode) and Assigned(FnSetAgcMode) and
            Assigned(FnResetBuffer) and Assigned(FnReadSync) and Assigned(FnReadAsync) and
            Assigned(FnCancelAsync);
  if not Result then
  begin
    UnloadLibrary(RTLHandle);
    RTLHandle := NilHandle;
  end;
end;

function RtlSdrGetDeviceCount: Cardinal;
begin
  if not LoadRTL then Exit(0);
  Result := FnGetDeviceCount();
end;

function RtlSdrOpen(var Dev: TRtlSdrDev; Index: Cardinal): Integer;
begin
  if not LoadRTL then Exit(-1);
  Result := FnOpen(Dev, Index);
end;

function RtlSdrClose(Dev: TRtlSdrDev): Integer;
begin
  if not LoadRTL then Exit(-1);
  Result := FnClose(Dev);
end;

function RtlSdrSetCenterFreq(Dev: TRtlSdrDev; Freq: Cardinal): Integer;
begin
  if not LoadRTL then Exit(-1);
  Result := FnSetCenterFreq(Dev, Freq);
end;

function RtlSdrSetSampleRate(Dev: TRtlSdrDev; Rate: Cardinal): Integer;
begin
  if not LoadRTL then Exit(-1);
  Result := FnSetSampleRate(Dev, Rate);
end;

function RtlSdrSetTunerGainMode(Dev: TRtlSdrDev; Manual: Integer): Integer;
begin
  if not LoadRTL then Exit(-1);
  Result := FnSetTunerGainMode(Dev, Manual);
end;

function RtlSdrSetAgcMode(Dev: TRtlSdrDev; On1: Integer): Integer;
begin
  if not LoadRTL then Exit(-1);
  Result := FnSetAgcMode(Dev, On1);
end;

function RtlSdrResetBuffer(Dev: TRtlSdrDev): Integer;
begin
  if not LoadRTL then Exit(-1);
  Result := FnResetBuffer(Dev);
end;

function RtlSdrReadSync(Dev: TRtlSdrDev; var Buf; Len: Integer; var NRead: Integer): Integer;
begin
  if not LoadRTL then Exit(-1);
  Result := FnReadSync(Dev, @Buf, Len, NRead);
end;

function RtlSdrReadAsync(Dev: TRtlSdrDev; Callback: TRtlSdrReadAsyncCB;
  Context: Pointer; BufferCount, BufferLength: Cardinal): Integer;
begin
  if not LoadRTL then Exit(-1);
  Result := FnReadAsync(Dev, Callback, Context, BufferCount, BufferLength);
end;

function RtlSdrCancelAsync(Dev: TRtlSdrDev): Integer;
begin
  if not LoadRTL then Exit(-1);
  Result := FnCancelAsync(Dev);
end;

finalization
  if RTLHandle <> NilHandle then
    UnloadLibrary(RTLHandle);
end.
