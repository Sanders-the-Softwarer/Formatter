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

{ ----- Примечания -------------------------------------------------------------

  Выражения рассматриваются как последовательности элементов (term),
  разделённых знаками бинарных операций. Их представлением является
  контейнер TStatementList<TTerm>, в котором операции выполняют роль
  разделителей. Каждый элемент может быть одним из многих различных типов
  (идентификатор, число, подзапрос и так далее), а кроме того снабжён
  кучей префиксных и постфиксных модификаторов, описывающих унарный минус,
  суффикс %rowcount и прочие подобные вещи.

------------------------------------------------------------------------------ }

interface

uses SysUtils, Math, Tokens, Statements, Printers_, Attributes;

type

  { Элемент выражения }
  TTerm = class(TStatement)
  strict private
    _Not: TKeyword;
    _Prefix: TTerminal;
    _Distinct: TKeyword;
    _Star: TTerminal;
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
    _Exists: TStatement;
    _OuterJoin: TTerminal;
    _Postfix: TKeyword;
  strict protected
    function InternalParse: boolean; override;
    function AllowStar: boolean;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Выражение }
  TExpression = class(TStatementList<TTerm>)
  strict protected
    function ParseDelimiter(out AResult: TObject): boolean; override;
    function ParseBreak: boolean; override;
  public
    function MultiLine: boolean; override;
  end;

  { Вложенное выражение }
  TInnerExpression = class(TExpression)
  strict protected
    function ParseDelimiter(out AResult: TObject): boolean; override;
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
    function Ident: string;
  end;

  TLValue = class(TStatementList<TLValueItem>)
  strict protected
    function ParseDelimiter(out AResult: TObject): boolean; override;
    function ParseBreak: boolean; override;
    procedure PrintDelimiter(APrinter: TPrinter; ADelimiter: TObject); override;
  public
    function StatementName: string; override;
    function MultiLine: boolean; override;
  end;

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
  [Aligned]
  TArguments = class(TCommaList<TArgument>)
  strict protected
    function ParseBreak: boolean; override;
  public
    function MultiLine: boolean; override;
  end;

implementation

uses DML, PLSQL;

type

  { Выражение case }
  TCase = class(TStatement)
  strict private
    _Case: TKeyword;
    _Expression: TStatement;
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

  { Условие exists }
  TExists = class(TInnerSelect)
  strict private
    _Exists: TKeyword;
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
  _Prefix := Terminal(['-', '+']);
  if Assigned(_Prefix) then _Prefix.OpType := otUnary;
  _Distinct := Keyword(['distinct', 'unique', 'all']);
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
    { Выражением case }
    if TCase.Parse(Self, Source, _Case) then exit(true);
    { Выражением cast }
    if TCast.Parse(Self, Source, _Cast) then exit(true);
    { Выражением exists }
    if TExists.Parse(Self, Source, _Exists) then exit(true);
    { Идентификатором или подобным выражением }
    TLValue.Parse(Self, Source, _LValue);
    if Assigned(_LValue) then
    begin
      _Suffix := Terminal(['%rowcount', '%found', '%notfound', '%isopen']);
      exit(true);
    end;
    { Вложенным запросом }
    if TInnerSelect.Parse(Self, Source, _Select) then exit(true);
    { Выраженим в скобках }
    _OpenBracket := Terminal('(');
    if Assigned(_OpenBracket) then
    begin
      TInnerExpression.Parse(Self, Source, _Expression);
      _CloseBracket := Terminal(')');
      exit(true);
    end;
    { В запросах может быть также звёздочкой }
    if AllowStar then _Star := Terminal('*');
    if Assigned(_Star) then exit(true);
  finally
    { Суффиксы }
    _OuterJoin := Terminal('(+)');
    _Postfix := Keyword(['is null', 'is not null']);
  end;
end;

function TTerm.AllowStar: boolean;
var S: TStatement;
begin
  Result := false;
  S := Self;
  while S <> nil do
    if S is TSelect
      then exit(true)
      else S := S.Parent;
end;

procedure TTerm.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Not, _Distinct, _Prefix, _Number, _Literal, _LValue, _Suffix, _KeywordValue, _Case, _Cast, _Exists, _FunctionCall]);
  if Assigned(_Select) then
  begin
    APrinter.NextLine;
    APrinter.PrintItem(_Select);
    APrinter.NextLine;
  end;
  APrinter.PrintItems([_OpenBracket, _Expression, _CloseBracket, _Star, _OuterJoin, _Postfix]);
end;

{ TExpression }

function TExpression.ParseDelimiter(out AResult: TObject): boolean;
begin
  AResult := Terminal(['+', '-', '*', '/', '||', '=', '<', '>', '<=', '>=', '<>', '!=', '^=']);
  if not Assigned(AResult) then
    AResult := Keyword(['like', 'not like', 'in', 'not in', 'and', 'or', 'between',
                        'multiset except', 'multiset except all', 'multiset except distinct',
                        'multiset intersect', 'multiset intersect all', 'multiset intersect distinct',
                        'multiset union', 'multiset union all', 'multiset union distinct']);
  Result := Assigned(AResult);
  if AResult is TTerminal then TTerminal(AResult).OpType := otBinary;
end;

function TExpression.ParseBreak: boolean;
begin
  Result := true;
end;

function TExpression.MultiLine: boolean;
begin
  Result := false;
end;

{ TInnerExpression }

function TInnerExpression.ParseDelimiter(out AResult: TObject): boolean;
begin
  AResult := Terminal(',');
  Result  := Assigned(AResult) or inherited ParseDelimiter(AResult);
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
  APrinter.Indent;
  APrinter.PrintItem(_Arguments);
  APrinter.Undent;
  APrinter.SpaceOrNextLine(MultiLine);
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
    APrinter.Ruler('argument', TArguments(Parent).MultiLine and Settings.AlignVariables);
  APrinter.PrintItem(_Assignment);
  APrinter.PrintItem(_Expression);
end;

{ TArguments }

function TArguments.ParseBreak: boolean;
begin
  Result := Any([Terminal([';', ')'])]) or (Source.Next is TKeyword);
end;

function TArguments.MultiLine: boolean;
begin
  Result := (Self.Count > Settings.ArgumentSingleLineParamLimit);
end;

{ TCase }

function TCase.InternalParse: boolean;
begin
  _Case := Keyword('case');
  if not Assigned(_Case) then exit(false);
  TExpression.Parse(Self, Source, _Expression);
  TCaseSections.Parse(Self, Source, _Sections);
  _End := Keyword('end');
  Result := true;
end;

procedure TCase.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Case, _Expression, _Sections, _End]);
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
    Result := 'when'
  else if Assigned(_Else) then
    Result := 'else'
  else
    Result := '';
end;

procedure TCaseSection.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_When, _Condition, _Then, _Else, _Value]);
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
  APrinter.PrintItems([_Cast, _OpenBracket, _Expression, _As, _TypeRef, _CloseBracket]);
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
  APrinter.PrintItems([_FunctionCall, _Ident]);
end;

function TLValueItem.Ident: string;
begin
  if Assigned(_Ident)
    then Result := _Ident.Value
    else Result := '';
end;

{ TLValue }

function TLValue.ParseDelimiter(out AResult: TObject): boolean;
begin
  AResult := Terminal('.');
  Result  := Assigned(AResult);
end;

function TLValue.ParseBreak: boolean;
begin
  Result := true;
end;

procedure TLValue.PrintDelimiter(APrinter: TPrinter; ADelimiter: TObject);
begin
  APrinter.PrintItem(ADelimiter);
end;

function TLValue.MultiLine: boolean;
begin
  Result := false;
end;

function TLValue.StatementName: string;
begin
  if (Count = 1) and (Item(0) is TLValueItem)
    then Result := TLValueItem(Item(0)).Ident
    else Result := '';
end;

{ TExists }

function TExists.InternalParse: boolean;
begin
  Result  := true;
  _Exists := Keyword('exists');
  if Assigned(_Exists)
    then inherited
    else Result := false;
end;

procedure TExists.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Exists);
  APrinter.Indent;
  APrinter.NextLine;
  inherited;
  APrinter.Undent;
end;

end.
