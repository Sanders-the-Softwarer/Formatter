////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//   Синтаксические конструкции арифметических-логических-прочих выражений    //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Expressions;

interface

uses SysUtils, System.Generics.Collections, Tokens, Parser, Printers_;

type

  { Выражение }
  TExpression = class(TStatement)
  strict private
    _Number: TNumber;
    _Literal: TLiteral;
    _Identifier: TIdent;
    _KeywordValue: TKeyword;
    _FunctionCall: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function Name: string; override;
  end;

  { Вызов функции }
  TFunctionCall = class(TStatement)
  strict private
    _Name: TIdent;
    _OpenBracket: TTerminal;
    _Arguments: TStatement;
    _CloseBracket: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function Name: string; override;
  end;

  { Аргументы вызова подпрограммы }
  TArguments = class(TStatementList)
  strict protected
    function ParseStatement(out AResult: TStatement): boolean; override;
    function ParseBreak: boolean; override;
  end;

  { Аргумент функции }
  TArgument = class(TStatement)
  strict private
    _Ident: TIdent;
    _Assignment: TTerminal;
    _Expression: TStatement;
    _Comma: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function Name: string; override;
  end;

implementation

{ TExpression }

function TExpression.InternalParse: boolean;
begin
  Result := false;
  _Number := Number;
  if Assigned(_Number) then exit(true);
  _Literal := Literal;
  if Assigned(_Literal) then exit(true);
  _KeywordValue := Keyword(['null', 'false', 'true']);
  if Assigned(_KeywordValue) then exit(true);
  if TFunctionCall.Parse(Self, Source, _FunctionCall) then exit(true);
  _Identifier := Identifier;
  if Assigned(_Identifier) then exit(true);
end;

function TExpression.Name: string;
begin
  Result := '< expression >';
end;

procedure TExpression.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Number);
  APrinter.PrintItem(_Literal);
  APrinter.PrintItem(_Identifier);
  APrinter.PrintItem(_KeywordValue);
  APrinter.PrintItem(_FunctionCall);
end;

{ TFunctionCall }

function TFunctionCall.InternalParse: boolean;
var _Arg: TStatement;
begin
  _Name := Identifier;
  _OpenBracket := Terminal('(');
  if not Assigned(_Name) or not Assigned(_OpenBracket) then exit(false);
  TArguments.Parse(Self, Source, _Arguments);
  _CloseBracket := Terminal(')');
end;

procedure TFunctionCall.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Name);
  APrinter.Space;
  APrinter.PrintItem(_OpenBracket);
  APrinter.PrintItem(_Arguments);
  APrinter.PrintItem(_CloseBracket);
end;

function TFunctionCall.Name: string;
begin
  Result := '<function call>';
end;

{ TArgument }

function TArgument.InternalParse: boolean;
var P: integer;
begin
  P := Source.Mark;
  _Ident := Identifier;
  _Assignment := Terminal('=>');
  if not Assigned(_Ident) or not Assigned(_Assignment) then
  begin
    FreeAndNil(_Ident);
    FreeAndNil(_Assignment);
    Source.Restore(P);
  end;
  Result := TExpression.Parse(Self, Source, _Expression);
  if Result then _Comma := Terminal(',');
end;

function TArgument.Name: string;
begin
  Result := '< argument >';
end;

procedure TArgument.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Ident);
  APrinter.Space;
  APrinter.PrintItem(_Assignment);
  APrinter.Space;
  APrinter.PrintItem(_Expression);
  APrinter.PrintItem(_Comma);
  if Assigned(_Comma) then APrinter.Space;
end;

{ TArguments }

function TArguments.ParseStatement(out AResult: TStatement): boolean;
begin
  Result := TArgument.Parse(Self, Source, AResult);
end;

function TArguments.ParseBreak: boolean;
begin
  Result := Any([Terminal(';'), Terminal(')')]);
end;

end.
