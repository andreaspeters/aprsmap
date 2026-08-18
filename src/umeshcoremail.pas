unit umeshcoremail;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, umeshcore;

function StoreMeshCoreMessageFile(const MailDirectory, OwnCall: String;
  const Msg: TMeshCoreMessage; out FileName: String): Boolean;

implementation

uses
  DateUtils, md5;

function MeshCoreMessageID(const Msg: TMeshCoreMessage): String;
var
  Identity: String;
begin
  Identity := IntToStr(Ord(Msg.Kind)) + #0 + Msg.Sender + #0 +
    IntToStr(Msg.Channel) + #0 + IntToStr(Msg.TimeStamp) + #0 + Msg.Text;
  Result := MD5Print(MD5String(Identity));
end;

function StoreMeshCoreMessageFile(const MailDirectory, OwnCall: String;
  const Msg: TMeshCoreMessage; out FileName: String): Boolean;
var
  F: TextFile;
  TempFileName: String;
  MessageTime: TDateTime;
  Opened: Boolean;
begin
  Result := False;
  FileName := '';
  if (Trim(MailDirectory) = '') or (Trim(Msg.Text) = '') then Exit;
  if not ForceDirectories(MailDirectory) then Exit;

  FileName := IncludeTrailingPathDelimiter(MailDirectory) + 'meshcore_' +
    MeshCoreMessageID(Msg) + '.txt';
  if FileExists(FileName) then Exit(True);

  if Msg.TimeStamp > 0 then
    MessageTime := UnixToDateTime(Msg.TimeStamp)
  else
    MessageTime := Now;

  TempFileName := FileName + '.tmp';
  Opened := False;
  AssignFile(F, TempFileName);
  try
    Rewrite(F);
    Opened := True;
    WriteLn(F, Format('ToCall: %s', [MeshCoreMailRecipient(Msg, OwnCall)]));
    WriteLn(F, Format('FromCall: %s', [Msg.Sender]));
    WriteLn(F, Format('DateStr: %s', [DateToStr(MessageTime)]));
    WriteLn(F, Format('TimeStr: %s', [TimeToStr(MessageTime)]));
    WriteLn(F, Format('MType: %s', [MeshCoreMailType(Msg.Kind)]));
    WriteLn(F, 'Message:');
    WriteLn(F, Msg.Text);
    CloseFile(F);
    Opened := False;
    if RenameFile(TempFileName, FileName) then
      Result := True
    else if FileExists(FileName) then
    begin
      DeleteFile(TempFileName);
      Result := True;
    end;
  except
    if Opened then CloseFile(F);
    DeleteFile(TempFileName);
    Result := False;
  end;
end;

end.
