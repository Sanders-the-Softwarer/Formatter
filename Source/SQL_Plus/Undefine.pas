////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                         Команда UNDEFINE (SQL*Plus)                        //
//                                                                            //
//               Copyright(c) 2019-2021 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Undefine;

interface

uses Tokens, Statements, Printer, SQLPlus;

type

  { Команда undefine }
  TUndefine = class(TSQLPlusStatement)
  strict private
    _Undefine: TEpithet;
    _Variables: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses Commons;

{ TUndefine }

function TUndefine.InternalParse: boolean;
begin
  Result := true;
  _Undefine := Keyword('undef[ine]');
  if not Assigned(_Undefine) then exit(false);
  TSingleLine<TRawList<TQualifiedIdent>>.Parse(Self, Source, _Variables);
end;

procedure TUndefine.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintRulerItems('cmd', [_Undefine]);
  APrinter.PrintRulerItems('var', [_Variables]);
end;

end.
