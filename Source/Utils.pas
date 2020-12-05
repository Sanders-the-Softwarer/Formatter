////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                        Вспомогательные  подпрограммы                       //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Utils;

interface

uses Classes, SysUtils, Windows, Forms;

const
  { Названия стандартных линеек для выравнивания }
  LEFT_RULER             = '$left$';
  RIGHT_COMMENT          = '$right-comment$';
  SPECIAL_COMMENT_START  = '$spec-comment-start$';
  SPECIAL_COMMENT_FINISH = '$spec-comment-finish$';
  CONCAT_DELIM_RULER     = '$concat-delim$';

{ Индикация отладочного режима }
function GetIsDebug: boolean;

{ Вывод в отладочную консоль }
procedure _Debug(const Msg: string; const Params: array of const);

implementation

var
  IsDebug: boolean;

{ Индикация отладочного режима }
function GetIsDebug: boolean;
begin
  Result := IsDebug;
end;

{ Вывод в отладочную консоль }
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

