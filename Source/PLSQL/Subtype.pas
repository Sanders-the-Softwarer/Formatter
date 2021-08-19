////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                             Декларация  SUBTYPE                            //
//                                                                            //
//               Copyright(c) 2019-2021 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Subtype;

interface

uses Statements, Tokens, Printer, PLSQL;

type
  { Определение подтипа }
  TSubtype = class(TPLSQLStatement)
  strict private
    _Subtype, _Name, _Is, _Character, _Set, _Charset, _Range, _Not, _Null: TEpithet;
    _DotDot: TTerminal;
    _BaseType, _RangeFrom, _RangeTo: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses Expressions, Commons;

{ TSubtype }

function TSubtype.InternalParse: boolean;
begin
  Result := true;
  _Subtype := Keyword('subtype');
  if not Assigned(_Subtype) then exit(false);
  _Name := Identifier;
  _Is := Keyword('is');
  TTypeRef.Parse(Self, Source, _BaseType);
  _Character := Keyword('character');
  if Assigned(_Character) then
  begin
    _Set := Keyword('set');
    _Charset := Identifier;
  end;
  _Range := Keyword('range');
  if Assigned(_Range) then
  begin
    TExpression.Parse(Self, Source, _RangeFrom);
    _DotDot := Terminal('..');
    TExpression.Parse(Self, Source, _RangeTo);
  end;
  _Not := Keyword('not');
  _Null := Keyword('null');
  inherited;
end;

procedure TSubtype.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_SubType, _Name, _Is, _BaseType, _Range, _RangeFrom, _DotDot, _RangeTo, _Character, _Set, _CharSet, _Not, _Null]);
  inherited;
end;

initialization
  DeclarationParser.Add(TSubtype);

end.
