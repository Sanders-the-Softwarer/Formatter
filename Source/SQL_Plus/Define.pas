////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                          Команда DEFINE (SQL*Plus)                         //
//                                                                            //
//               Copyright(c) 2019-2021 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Define;

interface

uses SQLPlus, Printer, Tokens, Statements;

type

  { Команда define }
  TDefine = class(TSQLPlusStatement)
  strict private
    _Define, _Target: TEpithet;
    _Eq: TTerminal;
    _Value: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses Expressions;

{ TDefine }

function TDefine.InternalParse: boolean;
begin
  Result := true;
  _Define := Keyword('def[ine]');
  if not Assigned(_Define) then exit(false);
  _Target := Identifier;
  _Eq := Terminal('=');
  TExpression.Parse(Self, Source, _Value);
  inherited;
end;

procedure TDefine.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintRulerItems('cmd', [_Define]);
  APrinter.PrintRulerItems('target', [_Target]);
  APrinter.PrintRulerItems('value', [_Eq, _Value]);
  inherited;
end;

end.
