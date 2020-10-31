////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                        ���������������  ������������                       //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Utils;

interface

uses Classes, SysUtils, Windows, Forms;

{ ����� � ���������� ������� }
procedure _Debug(const Msg: string; const Params: array of const);

implementation

var
  IsDebug: boolean;

{ ����� � ���������� ������� }
procedure _Debug(const Msg: string; const Params: array of const);
var S: string;
begin
  if not IsDebug then exit;
  try
    S := Format(Msg, Params);
  except
    S := Msg;
  end;
  OutputDebugString(@S[1]);
end;

initialization
  IsDebug := SameText(ExtractFileName(Application.ExeName), 'DebugTool.exe');
end.

