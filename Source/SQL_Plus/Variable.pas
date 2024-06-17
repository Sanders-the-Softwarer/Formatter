////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                         Команда VARIABLE (SQL*Plus)                        //
//                                                                            //
//               Copyright(c) 2019-2021 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Variable;

interface

uses Tokens, Statements, Printer, SQLPlus;

type

  { Команда variable }
  TVariable = class(TSQLPlusStatement)
  strict private
    _Variable, _VarName, _Type, _Size: TEpithet;
    _N: TNumber;
    _Eq, _OpenBracket, _CloseBracket: TTerminal;
    _Value: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses Commons, Expressions;

{ TVariable }

function TVariable.InternalParse: boolean;
begin
  Result := true;
  _Variable := Keyword('var[iable]');
  if not Assigned(_Variable) then exit(false);
  _VarName := Identifier;
  _Type := Keyword(['number', 'char', 'nchar', 'varchar2', 'nvarchar2', 'clob', 'nclob', 'refcursor', 'binary_float', 'binary_double']);
  if Assigned(_Type) then _OpenBracket := Terminal('(');
  if Assigned(_OpenBracket) then _N := Number;
  if Assigned(_N) then _Size := Keyword(['char', 'byte']);
  if Assigned(_OpenBracket) then _CloseBracket := Terminal(')');
  _Eq := Terminal('=');
  if Assigned(_Eq) then TExpression.Parse(Self, Source, _Value);
end;

procedure TVariable.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintRulerItems('var', [_Variable]);
  APrinter.PrintRulerItems('ident', [_VarName]);
  APrinter.PrintRulerItems('type', [_Type, _OpenBracket, _N, _Size, _CloseBracket]);
  APrinter.PrintRulerItems('eq', [_Eq, _Value]);
end;

end.
