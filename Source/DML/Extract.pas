////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                               Функция EXTRACT                              //
//                                                                            //
//               Copyright(c) 2019-2021 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Extract;

interface

uses Tokens, Statements, Printer, Commons;

type
  { Специальный синтаксис функции extract }
  TExtract = class(TSpecialFunction)
  strict private
    _What, _From: TEpithet;
    _Expr: TStatement;
  strict protected
    function ParseFunction: TEpithet; override;
    function InternalParse2: boolean; override;
    procedure InternalPrintSelf2(APrinter: TPrinter); override;
  end;

implementation

uses DML, Expressions;

{ TExtract }

function TExtract.ParseFunction: TEpithet;
begin
  Result := Keyword('extract');
end;

function TExtract.InternalParse2: boolean;
begin
  _What := Identifier;
  _From := Keyword('from');
  Result := Assigned(_From);
  if Result then TExpression.Parse(Self, Source, _Expr);
end;

procedure TExtract.InternalPrintSelf2(APrinter: TPrinter);
begin
  APrinter.PrintItems([_What, _From, _Expr]);
end;

initialization
  SelectSpecFunctionParser.Add(TExtract);

end.

