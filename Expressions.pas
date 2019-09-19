﻿////////////////////////////////////////////////////////////////////////////////
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

uses SysUtils, Math, System.Generics.Collections, Tokens, Parser, Printers_;

type

  { Выражение }
  TExpression = class(TStatement)
  strict private
    _OpenBracket: TTerminal;
    _PrefixOperation: TTerminal;
    _Term: TStatement;
    _InfixOperation: TTerminal;
    _InfixOperation2: TKeyword;
    _Rest: TStatement;
    _PostfixOperation: TKeyword;
    _CloseBracket: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function Name: string; override;
  end;

  { Элемент выражения }
  TTerm = class(TStatement)
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
    function MultiLine: boolean;
  end;

  { Аргументы вызова подпрограммы }
  TArguments = class(TStatementList)
  strict protected
    function ParseStatement(out AResult: TStatement): boolean; override;
    function ParseBreak: boolean; override;
  public
    function Name: string; override;
    function MaxIdentLen: integer;
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
    function IdentLen: integer;
  end;

implementation

{ TExpression }

function TTerm.InternalParse: boolean;
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

function TTerm.Name: string;
begin
  Result := '< term >';
end;

procedure TTerm.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Number);
  APrinter.PrintItem(_Literal);
  APrinter.PrintItem(_Identifier);
  APrinter.PrintItem(_KeywordValue);
  APrinter.PrintItem(_FunctionCall);
end;

{ TExpression }

function TExpression.InternalParse: boolean;
begin
  { Выражение распознано, если распознан первый аргумент }
  _OpenBracket := Terminal('(');
  _PrefixOperation := Terminal('-');
  if not TTerm.Parse(Self, Source, _Term) then exit(false);
  Result := true;
  _InfixOperation := Terminal(['+', '-', '*', '/', '||', '=', '<', '>', '<=', '>=', '<>', '!=', '^=']);
  if not Assigned(_InfixOperation) then
    _InfixOperation2 := Keyword(['and', 'or', 'xor', 'like', 'not like']);
  if Assigned(_InfixOperation) or Assigned(_InfixOperation2)
    then TExpression.Parse(Self, Source, _Rest)
    else _PostfixOperation := Keyword(['is null', 'is not null']);
  if Assigned(_OpenBracket) then _CloseBracket := Terminal(')');
end;

function TExpression.Name: string;
begin
  Result := '< expression >';
end;

procedure TExpression.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_OpenBracket);
  APrinter.PrintItem(_PrefixOperation);
  APrinter.PrintItem(_Term);
  APrinter.Space;
  APrinter.PrintItem(_InfixOperation);
  APrinter.PrintItem(_InfixOperation2);
  APrinter.Space;
  APrinter.PrintItem(_PostfixOperation);
  APrinter.PrintItem(_Rest);
  APrinter.SupressSpace;
  APrinter.PrintItem(_CloseBracket);
end;

{ TFunctionCall }

function TFunctionCall.InternalParse: boolean;
begin
  _Name := Identifier;
  _OpenBracket := Terminal('(');
  if not Assigned(_Name) or not Assigned(_OpenBracket) then exit(false);
  TArguments.Parse(Self, Source, _Arguments);
  _CloseBracket := Terminal(')');
  Result := true;
end;

procedure TFunctionCall.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Name);
  APrinter.SpaceOrNextLine(MultiLine);
  APrinter.Indent;
  APrinter.PrintItem(_OpenBracket);
  APrinter.SpaceOrNextLine(MultiLine);
  APrinter.SupressSpace;
  APrinter.Indent;
  APrinter.PrintItem(_Arguments);
  APrinter.Undent;
  APrinter.SpaceOrNextLine(MultiLine);
  APrinter.SupressSpace;
  APrinter.PrintItem(_CloseBracket);
  APrinter.Undent;
end;

function TFunctionCall.Name: string;
begin
  Result := '<function call>';
end;

function TFunctionCall.MultiLine: boolean;
begin
  Result := Assigned(_Arguments) and ((_Arguments as TStatementList).Count > Settings.ArgumentSingleLineParamLimit);
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
    _Ident := nil;
    _Assignment := nil;
    Source.Restore(P);
  end;
  Result := TExpression.Parse(Self, Source, _Expression);
  if Result then _Comma := Terminal(',');
end;

function TArgument.Name: string;
begin
  Result := '< argument >';
end;

function TArgument.IdentLen: integer;
begin
  if Assigned(_Ident) and TFunctionCall(Parent.Parent).MultiLine
    then Result := Length(_Ident.Value)
    else Result := 0;
end;

procedure TArgument.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PaddingFrom;
  APrinter.PrintItem(_Ident);
  if Assigned(_Ident) then
  begin
    APrinter.PaddingTo((Parent as TArguments).MaxIdentLen);
    APrinter.Space;
  end;
  APrinter.PrintItem(_Assignment);
  if Assigned(_Assignment) then APrinter.Space;
  APrinter.PrintItem(_Expression);
  APrinter.PrintItem(_Comma);
  if Assigned(_Comma) then APrinter.SpaceOrNextLine(TFunctionCall(Parent.Parent).MultiLine);
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

function TArguments.Name: string;
begin
  Result := '< arguments >';
end;

function TArguments.MaxIdentLen: integer;
var i: integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if Item(i) is TArgument then
      Result := Math.Max(Result, TArgument(Item(i)).IdentLen);
end;

end.
