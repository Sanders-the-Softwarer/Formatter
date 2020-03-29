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

uses Classes, SysUtils, Windows;

{ Вывод в отладочную консоль }
procedure _Debug(const Msg: string; const Params: array of const);

implementation

{ Вывод в отладочную консоль }
procedure _Debug(const Msg: string; const Params: array of const);
{$IfDef DEBUG_OUTPUT}
var S: string;
begin
  try
    S := Format(Msg, Params);
  except
    S := Msg;
  end;
  OutputDebugString(@S[1]);
{$Else}
begin
{$EndIf}
end;

end.

