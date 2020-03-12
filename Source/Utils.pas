////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                        Вспомогательные  подпрограммы                       //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Utils;

interface

uses Classes, SysUtils, Windows;

type
  { Класс для списков ключевых слов }
  TKeywords = class(TStringList)
  public
    constructor Create(AKeywords: array of string); overload;
    constructor Create(AParent: TKeywords; AKeywords: array of string); overload;
  end;

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

{ TKeywords }

constructor TKeywords.Create(AKeywords: array of string);
begin
  Create(nil, AKeywords);
end;

constructor TKeywords.Create(AParent: TKeywords; AKeywords: array of string);
var i: integer;
begin
  inherited Create;
  Sorted := true;
  Duplicates := dupIgnore;
  CaseSensitive := false;
  if Assigned(AParent) then AddStrings(AParent);
  for i := Low(AKeywords) to High(AKeywords) do Add(AKeywords[i]);
end;

end.

