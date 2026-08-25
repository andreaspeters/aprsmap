unit uaisreceiver;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Math, uaisdecoder;

type
  TAISMessageEvent = procedure(const Msg: TAISMessage) of object;
  TAISBitArray = array of Byte;

  { Native AIS receiver for 161.975 MHz and 162.025 MHz around a 162.000 MHz
    RTL-SDR centre frequency. IQ is mixed digitally for each channel, reduced
    to 48 kHz, frequency-discriminated, then sliced at the AIS 9600 baud rate. }
  TAISReceiver = class
  private
    FOnMessage: TAISMessageEvent;
    FPhaseA, FPhaseB: Double;
    FPreviousIA, FPreviousQA, FPreviousIB, FPreviousQB: Double;
    FAccumulatorIA, FAccumulatorQA, FAccumulatorIB, FAccumulatorQB: Double;
    FDecimationCount: Integer;
    FSymbolCountA, FSymbolCountB: Integer;
    FSymbolAccumulatorA, FSymbolAccumulatorB: Double;
    FLastLevelA, FLastLevelB: Boolean;
    FNRZIA, FNRZIB: TAISBitArray;
    procedure ProcessChannel(const Value: Double; var Accumulator: Double;
      var SymbolCount: Integer; var Level: Boolean; var Bits: TAISBitArray);
    procedure EmitHDLCFrame(const NRZIBits: TAISBitArray);
  public
    constructor Create(AOnMessage: TAISMessageEvent);
    procedure Reset;
    procedure ProcessIQ(const Buffer: PByte; const ByteCount: Integer);
  end;

function AISHDLCPayload(const NRZIBits: array of Byte; out PayloadBits: TBytes): Boolean;
function AISCRC16(const Data: TBytes): Word;

implementation

const
  AIS_SAMPLE_RATE = 240000.0;
  AIS_CHANNEL_OFFSET = 25000.0;
  AIS_DECIMATION = 5;
  AIS_SYMBOL_SAMPLES = 5;
  AIS_FLAG = $7E;

function AISCRC16(const Data: TBytes): Word;
var
  I, J: Integer;
  CRC: Word;
begin
  CRC := $FFFF;
  for I := 0 to High(Data) do
  begin
    CRC := CRC xor Data[I];
    for J := 0 to 7 do
      if (CRC and 1) <> 0 then
        CRC := (CRC shr 1) xor $8408
      else
        CRC := CRC shr 1;
  end;
  Result := CRC;
end;

function BitsToByteLSB(const Bits: array of Byte; const Start: Integer): Byte;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to 7 do
    Result := Result or ((Bits[Start + I] and 1) shl I);
end;

function EndsWithHDLCFlag(const NRZIBits: array of Byte): Boolean;
var
  I, Start: Integer;
  DecodedByte: Byte;
begin
  Result := False;
  if Length(NRZIBits) < 9 then Exit;
  Start := Length(NRZIBits) - 9;
  DecodedByte := 0;
  for I := 0 to 7 do
    if (NRZIBits[Start + I] and 1) = (NRZIBits[Start + I + 1] and 1) then
      DecodedByte := DecodedByte or (Byte(1) shl I);
  Result := DecodedByte = AIS_FLAG;
end;

function AISHDLCPayload(const NRZIBits: array of Byte; out PayloadBits: TBytes): Boolean;
var
  Decoded, Unstuffed: TBytes;
  I, J, Ones, StartFlag, EndFlag, DataBits, ByteCount: Integer;
  LastLevel: Byte;
  FrameBytes: TBytes;
  CRC: Word;
begin
  SetLength(PayloadBits, 0);
  Result := False;
  if Length(NRZIBits) < 24 then Exit;
  SetLength(Decoded, Length(NRZIBits));
  LastLevel := NRZIBits[0] and 1;
  for I := 1 to High(NRZIBits) do
  begin
    Decoded[I - 1] := Byte(Ord((NRZIBits[I] and 1) = LastLevel));
    LastLevel := NRZIBits[I] and 1;
  end;
  SetLength(Decoded, Length(NRZIBits) - 1);
  StartFlag := -1;
  EndFlag := -1;
  for I := 0 to Length(Decoded) - 8 do
    if BitsToByteLSB(Decoded, I) = AIS_FLAG then
    begin
      if StartFlag < 0 then
        StartFlag := I + 8
      else if I = StartFlag then
        { Consecutive HDLC flags are an idle/preamble sequence.  The last
          preamble flag, not the first pair, is the actual frame start. }
        StartFlag := I + 8
      else
      begin
        EndFlag := I;
        Break;
      end;
    end;
  if (StartFlag < 0) or (EndFlag <= StartFlag) then Exit;
  SetLength(Unstuffed, EndFlag - StartFlag);
  J := 0;
  Ones := 0;
  I := StartFlag;
  while I < EndFlag do
  begin
    if Decoded[I] <> 0 then
    begin
      Inc(Ones);
      Unstuffed[J] := 1;
      Inc(J);
      if Ones = 5 then
      begin
        Inc(I);
        if (I >= EndFlag) or (Decoded[I] <> 0) then Exit;
        Ones := 0;
      end;
    end
    else
    begin
      Ones := 0;
      Unstuffed[J] := 0;
      Inc(J);
    end;
    Inc(I);
  end;
  DataBits := J;
  if (DataBits < 24) or ((DataBits mod 8) <> 0) then Exit;
  ByteCount := DataBits div 8;
  SetLength(FrameBytes, ByteCount);
  for I := 0 to ByteCount - 1 do
    FrameBytes[I] := BitsToByteLSB(Unstuffed, I * 8);
  CRC := AISCRC16(FrameBytes);
  if CRC <> $F0B8 then Exit;
  SetLength(PayloadBits, (ByteCount - 2) * 8);
  for I := 0 to ByteCount - 3 do
    for J := 0 to 7 do
      PayloadBits[I * 8 + J] := (FrameBytes[I] shr (7 - J)) and 1;
  Result := True;
end;

constructor TAISReceiver.Create(AOnMessage: TAISMessageEvent);
begin
  inherited Create;
  FOnMessage := AOnMessage;
  Reset;
end;

procedure TAISReceiver.Reset;
begin
  FPhaseA := 0;
  FPhaseB := 0;
  FPreviousIA := 0;
  FPreviousQA := 0;
  FPreviousIB := 0;
  FPreviousQB := 0;
  FAccumulatorIA := 0;
  FAccumulatorQA := 0;
  FAccumulatorIB := 0;
  FAccumulatorQB := 0;
  FDecimationCount := 0;
  FSymbolCountA := 0;
  FSymbolCountB := 0;
  FSymbolAccumulatorA := 0;
  FSymbolAccumulatorB := 0;
  FLastLevelA := False;
  FLastLevelB := False;
  SetLength(FNRZIA, 0);
  SetLength(FNRZIB, 0);
end;

procedure TAISReceiver.EmitHDLCFrame(const NRZIBits: TAISBitArray);
var
  Payload: TBytes;
  Msg: TAISMessage;
begin
  if AISHDLCPayload(NRZIBits, Payload) and
     DecodeAISBits(Payload, Length(Payload), Msg) and Assigned(FOnMessage) then
    FOnMessage(Msg);
end;

procedure TAISReceiver.ProcessChannel(const Value: Double; var Accumulator: Double;
  var SymbolCount: Integer; var Level: Boolean; var Bits: TAISBitArray);
var
  NewLevel: Boolean;
  LengthBefore: Integer;
  Payload: TBytes;
begin
  Accumulator := Accumulator + Value;
  Inc(SymbolCount);
  if SymbolCount < AIS_SYMBOL_SAMPLES then Exit;
  NewLevel := Accumulator >= 0;
  LengthBefore := Length(Bits);
  SetLength(Bits, LengthBefore + 1);
  Bits[LengthBefore] := Ord(NewLevel);
  if (Length(Bits) >= 200) and EndsWithHDLCFlag(Bits) then
  begin
    if AISHDLCPayload(Bits, Payload) then
    begin
      EmitHDLCFrame(Bits);
      SetLength(Bits, 0);
    end
    else if Length(Bits) >= 2048 then
      SetLength(Bits, 0);
  end;
  Level := NewLevel;
  Accumulator := 0;
  SymbolCount := 0;
end;

procedure TAISReceiver.ProcessIQ(const Buffer: PByte; const ByteCount: Integer);
var
  I: Integer;
  InI, InQ, CosA, SinA, CosB, SinB: Double;
  IA, QA, IB, QB, DiscA, DiscB: Double;
  PhaseStep: Double;
begin
  if (ByteCount < 2) or Odd(ByteCount) then Exit;
  PhaseStep := 2 * Pi * AIS_CHANNEL_OFFSET / AIS_SAMPLE_RATE;
  for I := 0 to (ByteCount div 2) - 1 do
  begin
    InI := Buffer[I * 2] - 127;
    InQ := Buffer[I * 2 + 1] - 127;
    CosA := Cos(FPhaseA); SinA := Sin(FPhaseA);
    CosB := Cos(FPhaseB); SinB := Sin(FPhaseB);
    IA := InI * CosA - InQ * SinA;
    QA := InI * SinA + InQ * CosA;
    IB := InI * CosB + InQ * SinB;
    QB := -InI * SinB + InQ * CosB;
    FAccumulatorIA := FAccumulatorIA + IA;
    FAccumulatorQA := FAccumulatorQA + QA;
    FAccumulatorIB := FAccumulatorIB + IB;
    FAccumulatorQB := FAccumulatorQB + QB;
    FPhaseA := FPhaseA + PhaseStep;
    FPhaseB := FPhaseB - PhaseStep;
    if FPhaseA > 2 * Pi then FPhaseA := FPhaseA - 2 * Pi;
    if FPhaseB < 0 then FPhaseB := FPhaseB + 2 * Pi;
    Inc(FDecimationCount);
    if FDecimationCount = AIS_DECIMATION then
    begin
      DiscA := ArcTan2(FAccumulatorQA * FPreviousIA - FAccumulatorIA * FPreviousQA,
                       FAccumulatorIA * FPreviousIA + FAccumulatorQA * FPreviousQA);
      DiscB := ArcTan2(FAccumulatorQB * FPreviousIB - FAccumulatorIB * FPreviousQB,
                       FAccumulatorIB * FPreviousIB + FAccumulatorQB * FPreviousQB);
      FPreviousIA := FAccumulatorIA; FPreviousQA := FAccumulatorQA;
      FPreviousIB := FAccumulatorIB; FPreviousQB := FAccumulatorQB;
      FAccumulatorIA := 0; FAccumulatorQA := 0;
      FAccumulatorIB := 0; FAccumulatorQB := 0;
      FDecimationCount := 0;
      ProcessChannel(DiscA, FSymbolAccumulatorA, FSymbolCountA, FLastLevelA, FNRZIA);
      ProcessChannel(DiscB, FSymbolAccumulatorB, FSymbolCountB, FLastLevelB, FNRZIB);
    end;
  end;
end;

end.
