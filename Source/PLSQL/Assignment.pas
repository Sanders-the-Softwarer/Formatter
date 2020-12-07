////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                            Оператор присваивания                           //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Assignment;

interface

uses Statements, Tokens, Printer, PLSQL;

type
  { Присваивание }
  TAssignment = class(TPLSQLStatement)
  strict private
    _Target: TStatement;
    _Assignment: TTerminal;
    _Expression: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses Expressions, Commons;

{ TAssignment }

function TAssignment.InternalParse: boolean;
begin
  TQualifiedIndexedIdent.Parse(Self, Source, _Target);
  _Assignment := Terminal(':=');
  Result := Assigned(_Target) and Assigned(_Assignment);
  if not Result then exit;
  TExpression.Parse(Self, Source, _Expression);
  inherited;
end;

procedure TAssignment.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Target, _Assignment, _Expression]);
  inherited;
end;

end.
