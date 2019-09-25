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

uses SysUtils, Math, Tokens, Statements, Printers_, Attributes;

type

  { Элемент выражения }
  TTerm = class(TStatement)
  strict private
    _Not: TKeyword;
    _Prefix: TTerminal;
    _Number: TNumber;
    _Literal: TLiteral;
    _LValue: TStatement;
    _Suffix: TTerminal;
    _KeywordValue: TKeyword;
    _Select: TStatement;
    _FunctionCall: TStatement;
    _OpenBracket: TTerminal;
    _Expression: TStatement;
    _CloseBracket: TTerminal;
    _Case: TStatement;
    _Cast: TStatement;
    _OuterJoin: TTerminal;
    _Postfix: TKeyword;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Выражение }
  TExpression = class(TStatementList<TTerm>)
  strict private
    class var AllowComma: boolean;
  strict protected
    function ParseDelimiter(out AResult: TToken): boolean; override;
    function ParseBreak: boolean; override;
    function MultiLine: boolean; override;
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
    function MultiLine: boolean;
  end;

  { Понятие компилятора LVALUE (переменная, элемент таблицы и прочее, что может быть присвоено }

  TLValueItem = class(TStatement)
  strict private
    _Ident: TIdent;
    _FunctionCall: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  TLValue = class(TStatementList<TLValueItem>)
  strict protected
    function ParseDelimiter(out AResult: TToken): boolean; override;
    function ParseBreak: boolean; override;
    procedure PrintDelimiter(APrinter: TPrinter; ADelimiter: TToken); override;
    function MultiLine: boolean; override;
  end;

implementation

uses DML, PLSQL;

type

    { Аргумент вызова подпрограммы }
  TArgument = class(TStatement)
  strict private
    _Ident: TIdent;
    _Assignment: TTerminal;
    _Expression: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Аргументы вызова подпрограммы }
  TArguments = class(TCommaList<TArgument>)
  strict protected
    function ParseBreak: boolean; override;
  public
    function MultiLine: boolean; override;
  end;

  { Выражение case }
  TCase = class(TStatement)
  strict private
    _Case: TKeyword;
    _Sections: TStatement;
    _End: TKeyword;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Секция выражения case }
  TCaseSection = class(TStatement)
  strict private
    _When: TKeyword;
    _Condition: TStatement;
    _Then: TKeyword;
    _Else: TKeyword;
    _Value: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function StatementName: string; override;
  end;

  { Секции выражения case }
  TCaseSections = class(TStatementList<TCaseSection>)
  strict protected
    function ParseBreak: boolean; override;
    function MultiLine: boolean; override;
  end;

  { Выражение cast }
  TCast = class(TStatement)
  strict private
    _Cast: TKeyword;
    _OpenBracket: TTerminal;
    _Expression: TStatement;
    _As: TKeyword;
    _TypeRef: TStatement;
    _CloseBracket: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

{ TExpression }

function TTerm.InternalParse: boolean;
begin
  Result := false;
  { Префиксы }
  _Not := Keyword('not');
  _Prefix := Terminal('-');
  try
    { Слагаемое может быть числом }
    _Number := Number;
    if Assigned(_Number) then exit(true);
    { Литералом }
    _Literal := Literal;
    if Assigned(_Literal) then exit(true);
    { Ключевым словом null, true или false }
    _KeywordValue := Keyword(['null', 'false', 'true']);
    if Assigned(_KeywordValue) then exit(true);
    { Идентификатором или подобным выражением }
    TLValue.Parse(Self, Source, _LValue);
    if Assigned(_LValue) then
    begin
      _Suffix := Terminal('%rowcount');
      exit(true);
    end;
    { Вложенным запросом }
    if TInnerSelect.Parse(Self, Source, _Select) then exit(true);
    { Выраженим в скобках }
    _OpenBracket := Terminal('(');
    if Assigned(_OpenBracket) then
    begin
      TExpression.Parse(Self, Source, _Expression);
      _CloseBracket := Terminal(')');
      exit(true);
    end;
    { Выражением case }
    if TCase.Parse(Self, Source, _Case) then exit(true);
    { Выражением cast }
    if TCast.Parse(Self, Source, _Cast) then exit(true);
  finally
    { Суффиксы }
    _OuterJoin := Terminal('(+)');
    _Postfix := Keyword(['is null', 'is not null']);
  end;
end;

procedure TTerm.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Not);
  if Assigned(_Not) then APrinter.Space;
  APrinter.PrintItem(_Prefix);
  APrinter.PrintItem(_Number);
  APrinter.PrintItem(_Literal);
  APrinter.PrintItem(_LValue);
  APrinter.PrintItem(_Suffix);
  APrinter.PrintItem(_KeywordValue);
  APrinter.PrintItem(_FunctionCall);
  APrinter.PrintItem(_Select);
  APrinter.PrintItem(_OpenBracket);
  APrinter.PrintItem(_Expression);
  APrinter.SupressSpace;
  APrinter.PrintItem(_CloseBracket);
  APrinter.PrintItem(_Case);
  APrinter.Space;
  APrinter.PrintItem(_OuterJoin);
  APrinter.Space;
  APrinter.PrintItem(_Postfix);
  APrinter.SupressSpace;
end;

{ TExpression }

function TExpression.ParseDelimiter(out AResult: TToken): boolean;
begin
  AResult := Terminal(['+', '-', '*', '/', '||', '=', '<', '>', '<=', '>=', '<>', '!=', '^=']);
  if not Assigned(AResult) then AResult := Keyword(['like', 'not like', 'in', 'and', 'or', 'between']);
  if not Assigned(AResult) and AllowComma then AResult := Terminal(',');
  Result := Assigned(AResult);
  AllowComma := Assigned(AResult) and ((AResult.Value = ',') or (AResult.Value = 'in'));
end;

function TExpression.ParseBreak: boolean;
begin
  Result := true;
end;

function TExpression.MultiLine: boolean;
begin
  Result := false;
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
  APrinter.SupressSpace;
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

function TFunctionCall.MultiLine: boolean;
begin
  Result := Assigned(_Arguments) and ((_Arguments as TCommaList<TArgument>).Count > Settings.ArgumentSingleLineParamLimit);
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
end;

procedure TArgument.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Ident);
  if Assigned(_Ident) then
  begin
    APrinter.Ruler('argument', TArguments(Parent).MultiLine and Settings.AlignCallArguments);
    APrinter.Space;
  end;
  APrinter.PrintItem(_Assignment);
  if Assigned(_Assignment) then APrinter.Space;
  APrinter.PrintItem(_Expression);
end;

{ TArguments }

function TArguments.ParseBreak: boolean;
begin
  Result := Any([Terminal([';', ')'])]) or (Source.Next is TKeyword);
end;

function TArguments.MultiLine: boolean;
begin
  Result := (Parent as TFunctionCall).MultiLine;
end;

{ TCase }

function TCase.InternalParse: boolean;
begin
  _Case := Keyword('case');
  if not Assigned(_Case) then exit(false);
  TCaseSections.Parse(Self, Source, _Sections);
  _End := Keyword('end');
  Result := true;
end;

procedure TCase.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Case);
  APrinter.Space;
  APrinter.PrintItem(_Sections);
  APrinter.Space;
  APrinter.PrintItem(_End);
end;

{ TCaseSection }

function TCaseSection.InternalParse: boolean;
begin
  _When := Keyword('when');
  if Assigned(_When) then
    begin
      TExpression.Parse(Self, Source, _Condition);
      _Then := Keyword('then');
    end
  else
    _Else := Keyword('else');
  if not Assigned(_When) and not Assigned(_Else) then exit(false);
  TExpression.Parse(Self, Source, _Value);
  Result := true;
end;

function TCaseSection.StatementName: string;
begin
  if Assigned(_When) then
    Result := '< when >'
  else if Assigned(_Else) then
    Result := '< else >'
  else
    Result := inherited;
end;

procedure TCaseSection.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_When);
  APrinter.Space;
  APrinter.PrintItem(_Condition);
  APrinter.Space;
  APrinter.PrintItem(_Then);
  APrinter.PrintItem(_Else);
  APrinter.Space;
  APrinter.PrintItem(_Value);
end;

{ TCaseSections }

function TCaseSections.ParseBreak: boolean;
begin
  Result := not Assigned(Keyword(['when', 'else']));
end;

function TCaseSections.MultiLine: boolean;
begin
  Result := false;
end;

{ TCast }

function TCast.InternalParse: boolean;
begin
  _Cast := Keyword('cast');
  if not Assigned(_Cast) then exit(false);
  _OpenBracket := Terminal('(');
  TExpression.Parse(Self, Source, _Expression);
  _As := Keyword('as');
  TTypeRef.Parse(Self, Source, _TypeRef);
  _CloseBracket := Terminal(')');
  Result := true;
end;

procedure TCast.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Cast);
  APrinter.PrintItem(_OpenBracket);
  APrinter.PrintItem(_Expression);
  APrinter.Space;
  APrinter.PrintItem(_As);
  APrinter.Space;
  APrinter.PrintItem(_TypeRef);
  APrinter.PrintItem(_CloseBracket);
end;

{ TLValueItem }

function TLValueItem.InternalParse: boolean;
begin
  TFunctionCall.Parse(Self, Source, _FunctionCall);
  if not Assigned(_FunctionCall) then _Ident := Identifier;
  Result := Assigned(_FunctionCall) or Assigned(_Ident);
end;

procedure TLValueItem.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_FunctionCall);
  APrinter.PrintItem(_Ident);
end;

{ TLValue }

function TLValue.ParseDelimiter(out AResult: TToken): boolean;
begin
  AResult := Terminal('.');
  Result  := Assigned(AResult);
end;

function TLValue.ParseBreak: boolean;
begin
  Result := not (NextToken is TIdent);
end;

procedure TLValue.PrintDelimiter(APrinter: TPrinter; ADelimiter: TToken);
begin
  APrinter.PrintItem(ADelimiter);
end;

function TLValue.MultiLine: boolean;
begin
  Result := false;
end;

end.
