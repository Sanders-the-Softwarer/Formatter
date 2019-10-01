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

uses Classes, SysUtils, Math, Tokens, Statements, Printers_, Attributes;

type

  { Элемент выражения }
  TTerm = class(TStatement)
  strict private
    _Prefix: TToken;
    _Number: TNumber;
    _Literal: TLiteral;
    _SQLStatement: TStatement;
    _LValue: TStatement;
    _Suffix: TTerminal;
    _KeywordValue: TEpithet;
    _Select: TStatement;
    _OpenBracket: TTerminal;
    _Expression: TStatement;
    _CloseBracket: TTerminal;
    _Case: TStatement;
    _Cast: TStatement;
    _OuterJoin: TTerminal;
    _Postfix: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    function ParseSQLStatement: TStatement; virtual;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Выражение }
  TExpression = class(TStatementList<TTerm>)
  strict private
    class var FlagCreatedRight: boolean;
  strict protected
    function ParseDelimiter(out AResult: TObject): boolean; override;
    function ParseBreak: boolean; override;
    function GetKeywords: TStrings; override;
  public
    function MultiLine: boolean; override;
  public
    class procedure CreatedRight;
    procedure AfterConstruction; override;
  end;

  { Квалифицированный идентификатор }
  TQualifiedIdent = class(TStatement)
  strict private
    _Dot: TTerminal;
    _Name: TEpithet;
    _Next: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function StatementName: string; override;
  end;

  { Вызов функции }
  TFunctionCall = class(TStatement)
  strict private
    _Name: TStatement;
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
    _Ident: TEpithet;
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
    _Ident: TEpithet;
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

uses Parser, DML, PLSQL;

var
  Keywords: TStringList;

type

  { Выражение case }
  TCase = class(TStatement)
  strict private
    _Case: TEpithet;
    _Expression: TStatement;
    _Sections: TStatement;
    _End: TEpithet;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Секция выражения case }
  TCaseSection = class(TStatement)
  strict private
    _When: TEpithet;
    _Condition: TStatement;
    _Then: TEpithet;
    _Else: TEpithet;
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
    _Cast: TEpithet;
    _OpenBracket: TTerminal;
    _Expression: TStatement;
    _As: TEpithet;
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
  _Prefix := Keyword(['not', 'prior', 'distinct', 'unique', 'all']);
  if not Assigned(_Prefix) then
  begin
    _Prefix := Terminal(['-', '+']);
    if Assigned(_Prefix) then TTerminal(_Prefix).OpType := otUnary;
  end;
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
    { SQL-выражением }
    _SQLStatement := ParseSQLStatement;
    if Assigned(_SQLStatement) then exit(true);
    { Выражением case }
    if TCase.Parse(Self, Source, _Case) then exit(true);
    { Выражением cast }
    if TCast.Parse(Self, Source, _Cast) then exit(true);
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
      TParser.ParseExpression(Self, Source, _Expression);
      _CloseBracket := Terminal(')');
      exit(true);
    end;
  finally
    { Суффиксы }
    _OuterJoin := Terminal('(+)');
    _Postfix := Keyword(['is null', 'is not null']);
  end;
end;

function TTerm.ParseSQLStatement: TStatement;
begin
  Result := nil; { предназначен для перекрытия в TSQLTerm }
end;

procedure TTerm.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Prefix, _Number, _Literal, _SQLStatement, _LValue, _Suffix, _KeywordValue, _Case, _Cast]);
  if Assigned(_Select) then
  begin
    APrinter.NextLine;
    APrinter.PrintItem(_Select);
    APrinter.NextLine;
  end;
  APrinter.PrintItems([_OpenBracket, _Expression, _CloseBracket, _OuterJoin, _Postfix]);
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
  if not Assigned(AResult) and (Self.Parent is TTerm) then
    AResult := Terminal(',');
  Result := Assigned(AResult);
  if AResult is TTerminal then TTerminal(AResult).OpType := otBinary;
end;

function TExpression.ParseBreak: boolean;
begin
  Result := true;
end;

function TExpression.GetKeywords: TStrings;
begin
  Result := Keywords;
end;

function TExpression.MultiLine: boolean;
begin
  Result := false;
end;

class procedure TExpression.CreatedRight;
begin
  FlagCreatedRight := true;
end;

procedure TExpression.AfterConstruction;
begin
  inherited;
  if FlagCreatedRight
    then FlagCreatedRight := false
    else raise Exception.Create('Expression must be created using TParser.ParseExpression, do it!');
end;

{ TQualifiedIdent }

function TQualifiedIdent.InternalParse: boolean;
begin
  if Parent is TQualifiedIdent then _Dot := Terminal('.');
  _Name := Identifier;
  Result := Assigned(_Name) and (Assigned(_Dot) = (Parent is TQualifiedIdent));
  if Result then TQualifiedIdent.Parse(Self, Source, _Next);
end;

procedure TQualifiedIdent.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Dot, _Name, _Next]);
end;

function TQualifiedIdent.StatementName: string;
begin
  if Assigned(_Dot)
    then Result := _Dot.Value
    else Result := '';
  if Assigned(_Name) then
    Result := Result + _Name.Value;
  if Assigned(_Next) then
    Result := Result + _Next.StatementName;
end;

{ TFunctionCall }

function TFunctionCall.InternalParse: boolean;
begin
  TQualifiedIdent.Parse(Self, Source, _Name);
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
  Result := TParser.ParseExpression(Self, Source, _Expression);
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
  Result := Any([Terminal([';', ')'])]);
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
  TParser.ParseExpression(Self, Source, _Expression);
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
      TParser.ParseExpression(Self, Source, _Condition);
      _Then := Keyword('then');
    end
  else
    _Else := Keyword('else');
  if not Assigned(_When) and not Assigned(_Else) then exit(false);
  TParser.ParseExpression(Self, Source, _Value);
  Result := true;
end;

function TCaseSection.StatementName: string;
begin
  Result := Concat([_When, _Else]);
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
  TParser.ParseExpression(Self, Source, _Expression);
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

initialization
  Keywords := TStringList.Create;
  Keywords.Sorted := true;
  Keywords.Duplicates := dupIgnore;
  Keywords.CaseSensitive := false;
  Keywords.Add('case');
  Keywords.Add('when');

finalization
  FreeAndNil(Keywords);

end.
