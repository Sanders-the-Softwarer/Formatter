unit Utils;

interface

uses SysUtils, Windows;

procedure _Debug(const Msg: string; const Params: array of const);

implementation

procedure _Debug(const Msg: string; const Params: array of const);
var S: string;
begin
  try
    S := Format(Msg, Params);
  except
    S := Msg;
  end;
  OutputDebugString(@S[1]);
end;

end.

