////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                                Оператор EXIT                               //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Exit_;

interface

uses Statements, Tokens, Printer, PLSQL;

type
  { Оператор exit }
  TExit = class(TPLSQLStatement)
  strict private
    _Exit, _When: TEpithet;
    _Condition: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses Expressions;

{ TExit }

function TExit.InternalParse: boolean;
begin
  Result := true;
  _Exit := Keyword('exit');
  if not Assigned(_Exit) then exit(false);
  _When := Keyword('when');
  if Assigned(_When) then TExpression.Parse(Self, Source, _Condition);
  { Чтобы не путать с sqlplus-ным when, потребуем либо when, либо точку с запятой }
  if not inherited and not Assigned(_When) then exit(false);
end;

procedure TExit.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Exit, _When, _Condition]);
  inherited;
end;

end.
