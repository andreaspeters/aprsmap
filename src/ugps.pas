unit ugps;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, IdTCPClient,
  fpjson, jsonparser, ugpsd;

type

  { TFGPS }

  TFGPS = class(TForm)
  private

  public

  end;

var
  FGPS: TFGPS;

implementation

{$R *.lfm}

end.

