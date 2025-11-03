program aprsmap;

{$APPTYPE GUI}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, umain, uresize, ureadpipe, utypes, uaprs, uini, uigate,
  usettings, uinfo, umodes, ulastseen
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='APRSMap';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.CreateForm(TFSettings, FSettings);
  Application.CreateForm(TFInfo, FInfo);
  Application.CreateForm(TFLastSeen, FLastSeen);
  Application.Run;
end.

