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
    _Name: TStatement;
    _OpenBracket: TTerminal;
    _Arguments: TList<TStatement>;
    _CloseBracket: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure PrintSelf(APrinter: TPrinter); override;
    function Name: string; override;
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
  Result := 'expression';
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

procedure TFunctionCall.AfterConstruction;
begin
  inherited;
  _Arguments := TList<TStatement>.Create;
end;

procedure TFunctionCall.BeforeDestruction;
begin
  inherited;
  FreeAndNil(_Arguments);
end;

function TFunctionCall.InternalParse: boolean;
var _Arg: TStatement;
begin
  Result := TQualifiedIdentifier.Parse(Self, Source, _Name);
  if not Result then exit;
  _OpenBracket := Terminal('(');
  if not Assigned(_OpenBracket) then exit;
  repeat
    _CloseBracket := Terminal(')');
    if Assigned(_CloseBracket) then break;
    if TArgument.Parse(Self, Source, _Arg) or
       TUnexpectedToken.Parse(Self, Source, _Arg)
      then _Arguments.Add(_Arg)
      else exit;
  until false;
end;

function TFunctionCall.Name: string;
begin
  Result := 'function call';
end;

procedure TFunctionCall.PrintSelf(APrinter: TPrinter);
var i: integer;
begin
  APrinter.PrintItem(_Name);
  APrinter.Space;
  APrinter.PrintItem(_OpenBracket);
  for i := 0 to _Arguments.Count - 1 do APrinter.PrintItem(_Arguments[i]);
  APrinter.PrintItem(_CloseBracket);
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
  Result := 'argument';
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

end.
