///////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                           Команда EXIT (SQL*Plus)                          //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Exit_SQLPlus;

interface

uses Commons, Statements, Tokens, Printer, SQLPlus;

type
  TExit = class(TSQLPlusStatement)
  strict private
    _Exit, _Action: TEpithet;
    _Expr: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    class function Priority: integer; override;
  end;

implementation

uses Parser, Expressions;

{ TExit }

class function TExit.Priority: integer;
begin
  Result := LOWER_PRIORITY;
end;

function TExit.InternalParse: boolean;
begin
  Result := true;
  _Exit := Keyword(['exit', 'quit']);
  if not Assigned(_Exit) then exit(false);
  TExpression.Parse(Self, Source, _Expr);
  _Action := Keyword(['commit', 'rollback']);
  inherited;
end;

procedure TExit.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Exit, _Expr, _Action]);
  inherited;
end;

end.
