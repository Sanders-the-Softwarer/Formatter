////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                               Функция LISTAGG                              //
//                                                                            //
//               Copyright(c) 2019-2021 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit ListAgg;

interface

uses Tokens, Statements, Printer, Commons;

type
  { Специальный синтаксис функции listagg }
  TListAgg = class(TSpecialFunction)
  strict private
    _Expression, _Delimiter: TStatement;
    _Comma: TTerminal;
    _On, _Overflow, _Truncate, _Without, _Count: TEpithet;
    _OverflowTag: TLiteral;
  strict protected
    function ParseFunction: TEpithet; override;
    function InternalParse2: boolean; override;
    procedure InternalPrintSelf2(APrinter: TPrinter); override;
  end;

implementation

uses DML, Expressions;

{ TListAgg }

function TListAgg.ParseFunction: TEpithet;
begin
  Result := Keyword('listagg');
end;

function TListAgg.InternalParse2: boolean;
begin
  Result := true;
  TExpression.Parse(Self, Source, _Expression);
  _Comma := Terminal(',');
  if Assigned(_Comma) then TExpression.Parse(Self, Source, _Delimiter);
  _On := Keyword('on');
  _Overflow := Keyword('overflow');
  _Truncate := Keyword('truncate');
  _OverflowTag := Literal;
  _Without := Keyword('without');
  _Count := Keyword('count');
end;

procedure TListAgg.InternalPrintSelf2(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Expression, _Comma, _Delimiter, _On, _Overflow, _Truncate, _OverflowTag, _Without, _Count]);
end;

initialization
  SelectSpecFunctionParser.Add(TListAgg);

end.

