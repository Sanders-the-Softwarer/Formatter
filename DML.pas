////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                       Синтаксические конструкции DML                       //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit DML;

interface

uses Classes, SysUtils, Math, Tokens, Statements, Printers_, Attributes, Expressions;

type

  { Общий предок DML-операторов }
  TDML = class(TSemicolonStatement)
  strict protected
    function GetKeywords: TStrings; override;
  end;

  { Расширение понятия операнда для SQL-выражений }
  TSqlTerm = class(TTerm)
  strict protected
    function ParseSQLStatement: TStatement; override;
  end;

  { SQL-выражение }
  TSqlExpression = class(TExpression)
  strict protected
    function ParseStatement(out AResult: TStatement): boolean; override;
    function GetKeywords: TStrings; override;
  end;

  { Оператор select }
  TSelect = class(TDML)
  strict private
    _With: TStatement;
    _Select: TEpithet;
    _Mode: TEpithet;
    _Fields: TStatement;
    _Into: TEpithet;
    _IntoFields: TStatement;
    _From: TEpithet;
    _Tables: TStatement;
    _Where: TStatement;
    _StartWith: TStatement;
    _ConnectBy: TStatement;
    _GroupBy: TStatement;
    _Having: TStatement;
    _OrderBy: TStatement;
    _AdditionalSelect: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    procedure Match(AFields: TStatement);
  end;

  { Оператор insert }
  TInsert = class(TDML)
  strict private
    _Insert: TEpithet;
    _Into: TEpithet;
    _Table: TStatement;
    _OpenBracket: TTerminal;
    _Fields: TStatement;
    _CloseBracket: TTerminal;
    _Select: TStatement;
    _Values: TEpithet;
    _OpenBracket2: TTerminal;
    _ValueList: TStatement;
    _CloseBracket2: TTerminal;
    _Returning: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор update }
  TUpdate = class(TDML)
  strict private
    _Update: TEpithet;
    _Table: TStatement;
    _Set: TEpithet;
    _Assignments: TStatement;
    _Where: TStatement;
    _Returning: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор delete }
  TDelete = class(TDML)
  strict private
    _Delete: TEpithet;
    _From: TEpithet;
    _Table: TStatement;
    _Where: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор merge }
  TMerge = class(TDML)
  strict private
    _Merge: TEpithet;
    _Into: TEpithet;
    _DestSelect: TStatement;
    _DestTable: TEpithet;
    _DestAlias: TEpithet;
    _Using: TEpithet;
    _SourceSelect: TStatement;
    _SourceTable: TEpithet;
    _SourceAlias: TEpithet;
    _On: TEpithet;
    _Condition: TStatement;
    _Sections: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Вложенный запрос }
  TInnerSelect = class(TStatement)
  strict private
    _OpenBracket: TTerminal;
    _Select: TStatement;
    _CloseBracket: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Идентификатор как поле в insert, select into итп }
  TIdentField = class(TStatement)
  strict private
    _FieldName: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function StatementName: string; override;
  end;

  { Список идентификаторов }

  TIdentFields = class(TCommaList<TIdentField>)
  strict protected
    function ParseBreak: boolean; override;
  end;

  { Список выражений }

  TExpressionField = class(TStatement)
  strict private
    _Expression: TStatement;
    _Match: TIdentField;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override; final;
    procedure PrintSelfAfter(APrinter: TPrinter); virtual;
    property Match: TIdentField read _Match write _Match;
  end;

  [Aligned]
  TExpressionFields = class(TCommaList<TExpressionField>)
  public
    procedure Match(AFields: TIdentFields);
  end;

implementation

uses Parser;

var
  Keywords, SqlExpressionKeywords: TStringList;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                         Общие элементы конструкций                         //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

type

  { Выражение со звёздочкой }
  TAsterisk = class(TStatement)
  strict private
    _Prefix: TStatement;
    _Dot, _Star: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Условие exists }
  TExists = class(TInnerSelect)
  strict private
    _Exists: TEpithet;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция within group }
  TWithinGroup = class(TStatement)
  strict private
    _Within, _Group: TEpithet;
    _OrderBy: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция over }
  TOver = class(TStatement)
  strict private
    _Over: TEpithet;
    _OpenBracket: TTerminal;
    _Partition, _By: TEpithet;
    _PartitionFields: TStatement;
    _OrderBy: TStatement;
    _CloseBracket: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция keep }
  TKeep = class(TStatement)
  strict private
    _Keep: TEpithet;
    _OpenBracket: TTerminal;
    _Rank: TEpithet;
    _FirstOrLast: TEpithet;
    _OrderBy: TStatement;
    _CloseBracket: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Вызов функции с различными select-ными прибамбасами }
  TSelectFunctionCall = class(TStatement)
  strict private
    _FunctionCall: TStatement;
    _WithinGroup: TStatement;
    _Keep: TStatement;
    _Over: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Спецфункция listagg }
  TListagg = class(TStatement)
  strict private
    _ListAgg: TEpithet;
    _OpenBracket: TTerminal;
    _Expression: TStatement;
    _Comma: TTerminal;
    _Delimiter: TLiteral;
    _On, _Overflow, _Truncate: TEpithet;
    _OverflowTag: TLiteral;
    _Without, _Count: TEpithet;
    _CloseBracket: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Строка выражения order by }
  TOrderByItem = class(TStatement)
  strict private
    _Expression: TStatement;
    _Nulls, _Position, _Direction: TEpithet;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Выражение order by }
  TOrderBy = class(TCommaList<TOrderByItem>)
  strict private
    _Order, _Siblings, _By: TEpithet;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция where }
  TWhere = class(TStatement)
  strict private
    _Where: TEpithet;
    _Condition: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция returning }
  TReturning = class(TStatement)
  strict private
    _Returning: TEpithet;
    _ReturningFields: TStatement;
    _Into: TEpithet;
    _Targets: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Указание таблицы - в select, merge, delete и т. п. }
  TTableRef = class(TStatement)
  strict private
    _Select: TStatement;
    _Table: TEpithet;
    _OpenBracket: TTerminal;
    _TableName: TStatement;
    _CloseBracket: TTerminal;
    _Alias: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    function ParseTableExpression(out AResult: TStatement): boolean; virtual;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function StatementName: string; override;
  end;

{ TDML }

function TDML.GetKeywords: TStrings;
begin
  Result := Keywords;
end;

{ TSQLTerm }

function TSQLTerm.ParseSQLStatement: TStatement;
begin
  Result := nil;
  if not Assigned(Result) then TAsterisk.Parse(Self, Source, Result);
  if not Assigned(Result) then TExists.Parse(Self, Source, Result);
  if not Assigned(Result) then TSelectFunctionCall.Parse(Self, Source, Result);
  if not Assigned(Result) then TListagg.Parse(Self, Source, Result);
end;

{ TSQLExpression }

function TSQLExpression.ParseStatement(out AResult: TStatement): boolean;
begin
  Result := TSQLTerm.Parse(Self, Source, AResult);
end;

function TSQLExpression.GetKeywords: TStrings;
begin
  Result := SqlExpressionKeywords;
  if Assigned(Result) then exit;
  Result := TStringList.Create;
  Result.Assign(Keywords);
  Result.AddStrings(inherited);
  SqlExpressionKeywords := TStringList(Result);
end;

{ TAsterisk }

function TAsterisk.InternalParse: boolean;
begin
  TQualifiedIdent.Parse(Self, Source, _Prefix);
  _Dot := Terminal('.');
  _Star := Terminal('*');
  Result := Assigned(_Star) and (Assigned(_Dot) = Assigned(_Prefix));
end;

procedure TAsterisk.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Prefix, _Dot, _Star]);
end;

{ TExists }

function TExists.InternalParse: boolean;
begin
  _Exists := Keyword('exists');
  Result := Assigned(_Exists);
  if Result then inherited;
end;

procedure TExists.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Exists);
  APrinter.Indent;
  APrinter.NextLine;
  inherited;
  APrinter.Undent;
end;

{ TWithinGroup }

function TWithinGroup.InternalParse: boolean;
begin
  _Within := Keyword('within');
  if not Assigned(_Within) then exit(false);
  _Group  := Keyword('group');
  TBracketedStatement<TOrderBy>.Parse(Self, Source, _OrderBy);
  Result := true;
end;

procedure TWithinGroup.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Within, _Group]);
  APrinter.PrintIndented(_OrderBy);
end;

{ TOver }

function TOver.InternalParse: boolean;
begin
  _Over := Keyword('over');
  if not Assigned(_Over) then exit(false);
  _OpenBracket := Terminal('(');
  _Partition   := Keyword('partition');
  if Assigned(_Partition) then _By := Keyword('by');
  if Assigned(_Partition) then TIdentFields.Parse(Self, Source, _PartitionFields);
  TOrderBy.Parse(Self, Source, _OrderBy);
  _CloseBracket := Terminal(')');
  Result := true;
end;

procedure TOver.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Over);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_OpenBracket);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItems([_Partition, _By]);
  APrinter.PrintIndented(_PartitionFields);
  APrinter.PrintItem(_OrderBy);
  APrinter.NextLine;
  APrinter.Undent;
  APrinter.PrintItem(_CloseBracket);
  APrinter.Undent;
end;

{ TKeep }

function TKeep.InternalParse: boolean;
begin
  _Keep := Keyword('keep');
  if not Assigned(_Keep) then exit(false);
  _OpenBracket := Terminal('(');
  _Rank := Identifier;
  _FirstOrLast := Keyword(['first', 'last']);
  TOrderBy.Parse(Self, Source, _OrderBy);
  _CloseBracket := Terminal(')');
  Result := true;
end;

procedure TKeep.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Keep, _NextLine, _Indent,
                              _OpenBracket, _NextLine, _Indent,
                                            _Rank, _NextLine,
                                            _FirstOrLast, _NextLine,
                                            _OrderBy, _NextLine, _Undent,
                              _CloseBracket, _Undent]);
end;

{ TAnalyticFunctionCall }

function TSelectFunctionCall.InternalParse: boolean;
begin
  TListAgg.Parse(Self, Source, _FunctionCall);
  if not Assigned(_FunctionCall) then TFunctionCall.Parse(Self, Source, _FunctionCall);
  if not Assigned(_FunctionCall) then exit(false);
  TWithinGroup.Parse(Self, Source, _WithinGroup);
  TKeep.Parse(Self, Source, _Keep);
  TOver.Parse(Self, Source, _Over);
  Result := Assigned(_WithinGroup) or Assigned(_Keep) or Assigned(_Over);
end;

procedure TSelectFunctionCall.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_FunctionCall);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItems([_WithinGroup, _NextLine, _Keep, _NextLine, _Over]);
  APrinter.Undent;
end;

{ TListagg }

function TListagg.InternalParse: boolean;
begin
  _ListAgg := Identifier;
  if not Assigned(_ListAgg) or not SameText(_ListAgg.Value, 'listagg') then exit(false);
  _OpenBracket := Terminal('(');
  TParser.ParseExpression(Self, Source, _Expression);
  _Comma := Terminal(',');
  if Assigned(_Comma) then _Delimiter := Literal;
  _On := Keyword('on');
  _Overflow := Keyword('overflow');
  _Truncate := Keyword('truncate');
  _OverflowTag := Literal;
  _Without := Keyword('without');
  _Count := Identifier; { должен быть count, но нет смысла это проверять }
  _CloseBracket := Terminal(')');
  Result := true;
end;

procedure TListagg.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_ListAgg, _OpenBracket, _Expression, _Comma, _Delimiter,
    _On, _Overflow, _Truncate, _OverflowTag, _Without, _Count, _CloseBracket]);
end;

{ TIdentField }

function TIdentField.InternalParse: boolean;
begin
  Result := TQualifiedIdent.Parse(Self, Source, _FieldName);
end;

function TIdentField.StatementName: string;
begin
  Result := _FieldName.StatementName;
end;

procedure TIdentField.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_FieldName);
end;

{ TIdentFields }

function TIdentFields.ParseBreak: boolean;
begin
  Result := Any([Terminal([')', ';']), Keyword('*')]);
end;

{ TOrderByItem }

function TOrderByItem.InternalParse: boolean;
begin
  if not TParser.ParseExpression(Self, Source, _Expression) then exit(false);
  _Direction := Keyword(['asc', 'desc']);
  _Nulls := Keyword('nulls');
  _Position := Keyword(['first', 'last']);
  Result := true;
end;

procedure TOrderByItem.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Expression, _Direction, _Nulls, _Position]);
end;

{ TOrderBy }

function TOrderBy.InternalParse: boolean;
begin
  _Order    := Keyword('order');
  _Siblings := Keyword('siblings');
  _By       := Keyword('by');
  if not Assigned(_Order) then exit(false);
  Result := inherited InternalParse;
end;

procedure TOrderBy.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Order, _Siblings, _By]);
  APrinter.NextLine;
  APrinter.Indent;
  inherited;
  APrinter.Undent;
end;

{ TExpressionField }

function TExpressionField.InternalParse: boolean;
begin
  Result := TParser.ParseExpression(Self, Source, _Expression);
end;

procedure TExpressionField.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Expression);
  PrintSelfAfter(APrinter);
  if not Assigned(_Match) then exit;
  APrinter.Ruler('match', Settings.AlignSpecialComments);
  APrinter.PrintSpecialComment('=> ' + TIdentField(_Match).Name);
end;

procedure TExpressionField.PrintSelfAfter(APrinter: TPrinter);
begin
  { ничего не делаем }
end;

{ TExpressionFields }

procedure TExpressionFields.Match(AFields: TIdentFields);
var
  Count, i: integer;
begin
  if not Assigned(Self) or not Assigned(AFields) then exit;
  Count := Math.Min(Self.Count, AFields.Count);
  if Count < Settings.MatchParamLimit then exit;
  for i := 0 to Count - 1 do
    if (Self.Item(i) is TExpressionField) and (AFields.Item(i) is TIdentField) then
      TExpressionField(Self.Item(i)).Match := TIdentField(AFields.Item(i));
end;

{ TWhere }

function TWhere.InternalParse: boolean;
begin
  _Where := Keyword('where');
  TParser.ParseExpression(Self, Source, _Condition);
  Result := Assigned(_Where);
end;

procedure TWhere.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Where, _IndentNextLine, _Condition, _Undent]);
end;

{ TReturning }

function TReturning.InternalParse: boolean;
begin
  _Returning := Keyword('returning');
  if not Assigned(_Returning) then exit(false);
  TExpressionFields.Parse(Self, Source, _ReturningFields);
  _Into := Keyword('into');
  TIdentFields.Parse(Self, Source, _Targets);
  Result := true;
end;

procedure TReturning.PrintSelf(APrinter: TPrinter);
begin
  TExpressionFields(_ReturningFields).Match(TIdentFields(_Targets));
  APrinter.NextLine;
  APrinter.PrintItem(_Returning);
  APrinter.PrintIndented(_ReturningFields);
  APrinter.PrintItem(_Into);
  APrinter.PrintIndented(_Targets);
end;

{ TTableClause }

function TTableRef.InternalParse: boolean;
begin
  if not TInnerSelect.Parse(Self, Source, _Select) then
  begin
    _Table := Keyword('table');
    if Assigned(_Table) then _OpenBracket := Terminal('(');
    ParseTableExpression(_TableName);
    if not Assigned(_Table) and not Assigned(_TableName) then exit(false);
    if Assigned(_Table) then _CloseBracket := Terminal(')');
  end;
  _Alias := Identifier;
  Result := true;
end;

function TTableRef.ParseTableExpression(out AResult: TStatement): boolean;
begin
  Result := TQualifiedIdent.Parse(Self, Source, _TableName);
end;

function TTableRef.StatementName: string;
begin
  Result := Concat([_Table, _TableName, _Select]);
  if Assigned(_Alias) and (Result <> '') then Result := Result + ' as ' + _Alias.Value;
end;

procedure TTableRef.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Select, _Table, _OpenBracket, _TableName, _CloseBracket, _Alias]);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                               Оператор SELECT                              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

type

  { Запрос в with }
  TWithItem = class(TStatement)
  strict private
    _Alias: TEpithet;
    _As: TEpithet;
    _Select: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function StatementName: string; override;
  end;

  { Конструкция with }
  TWith = class(TCommaList<TWithItem>)
  strict private
    _With: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    function ParseBreak: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Поле в select }
  TSelectField = class(TExpressionField)
  strict private
    _As: TEpithet;
    _Alias: TEpithet;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelfAfter(APrinter: TPrinter); override;
    function StatementName: string; override;
  end;

  { Список полей в select }
  [Aligned]
  TSelectFields = class(TExpressionFields)
  strict protected
    function ParseStatement(out AResult: TStatement): boolean; override;
  end;

  { Указание таблицы в select }
  TSelectTableRef = class(TTableRef)
  strict private
    _Lateral: TEpithet;
    _On: TEpithet;
    _JoinCondition: TStatement;
    _Using: TEpithet;
    _Fields: TStatement;
  strict protected
    function InternalParse: boolean; override;
    function ParseTableExpression(out AResult: TStatement): boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Список таблиц в select }
  TSelectTables = class(TCommaList<TSelectTableRef>)
  strict protected
    function ParseDelimiter(out AResult: TObject): boolean; override;
    procedure PrintDelimiter(APrinter: TPrinter; ADelimiter: TObject); override;
  end;

  { Выражение group by }
  TGroupBy = class(TCommaList<TExpressionField>)
  strict private
    _Group, _By: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    function ParseBreak: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Выражение having }
  THaving = class(TStatement)
  strict private
    _Having: TEpithet;
    _Condition: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Выражение start with }
  TStartWith = class(TStatement)
  strict private
    _Start, _With: TEpithet;
    _Condition: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

    { Выражение start with }
  TConnectBy = class(TStatement)
  strict private
    _Connect, _By: TEpithet;
    _Condition: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Следующий запрос в цепочке union all }
  TAdditionalSelect = class(TSelect)
  strict private
    _SetOperation: TEpithet;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

{ TSelect }

function TSelect.InternalParse: boolean;
begin
  TWith.Parse(Self, Source, _With);
  _Select := Keyword('select');
  { Чтобы распознать конструкцию, надо увидеть либо with, либо select }
  if not Assigned(_With) and not Assigned(_Select) then exit(false);
  _Mode := Keyword(['distinct', 'unique', 'all']);
  TSelectFields.Parse(Self, Source, _Fields);
  _Into := Keyword(['into', 'bulk collect into']);
  TIdentFields.Parse(Self, Source, _IntoFields);
  _From := Keyword('from');
  TSelectTables.Parse(Self, Source, _Tables);
  TWhere.Parse(Self, Source, _Where);
  TStartWith.Parse(Self, Source, _StartWith);
  TConnectBy.Parse(Self, Source, _ConnectBy);
  TGroupBy.Parse(Self, Source, _GroupBy);
  THaving.Parse(Self, Source, _Having);
  TOrderBy.Parse(Self, Source, _OrderBy);
  TAdditionalSelect.Parse(Self, Source, _AdditionalSelect);
  inherited;
  Result := true;
end;

procedure TSelect.PrintSelf(APrinter: TPrinter);
begin
  TExpressionFields(_Fields).Match(TIdentFields(_IntoFields));
  APrinter.PrintItems([_With,
                       _Select,    _Mode,           _IndentNextLine,
                                   _Fields,         _UndentNextLine,
                       _Into,      _IndentNextLine,
                                   _IntoFields,     _UndentNextLine,
                       _From,      _IndentNextLine,
                                   _Tables,         _Undent]);
  APrinter.NextLineIf([_Where]);
  APrinter.NextLineIf([_StartWith]);
  APrinter.NextLineIf([_ConnectBy]);
  APrinter.NextLineIf([_GroupBy]);
  APrinter.NextLineIf([_Having]);
  APrinter.NextLineIf([_OrderBy]);
  APrinter.NextLineIf([_AdditionalSelect]);
  inherited;
end;

procedure TSelect.Match(AFields: TStatement);
begin
  if Assigned(Self) and (_Fields is TExpressionFields) and (AFields is TIdentFields) then
    TExpressionFields(_Fields).Match(AFields as TIdentFields);
end;

{ TInnerSelect }

function TInnerSelect.InternalParse: boolean;
begin
  _OpenBracket := Terminal('(');
  TSelect.Parse(Self, Source, _Select);
  _CloseBracket := Terminal(')');
  Result := Assigned(_OpenBracket) and Assigned(_Select);
end;

procedure TInnerSelect.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_OpenBracket);
  APrinter.PrintIndented(_Select);
  APrinter.PrintItem(_CloseBracket);
end;

{ TWithItem }

function TWithItem.InternalParse: boolean;
begin
  _Alias := Identifier;
  _As    := Keyword('as');
  TInnerSelect.Parse(Self, Source, _Select);
  Result := Assigned(_Alias);
end;

procedure TWithItem.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Alias, _As, _Select]);
end;

function TWithItem.StatementName: string;
begin
  Result := Concat([_Alias]);
end;

{ TWith }

function TWith.InternalParse: boolean;
begin
  _With := Keyword('with');
  if not Assigned(_With) then exit(false);
  inherited;
  Result := true;
end;

function TWith.ParseBreak: boolean;
begin
  Result := Any([Keyword('select'), Terminal([')', ';'])]);
end;

procedure TWith.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_With);
  APrinter.NextLine;
  APrinter.Indent;
  inherited;
  APrinter.Undent;
end;

{ TSelectField }

function TSelectField.InternalParse: boolean;
begin
  Result := inherited InternalParse;
  if not Result then exit;
  _As := Keyword('as');
  _Alias := Identifier;
end;

function TSelectField.StatementName: string;
begin
  Result := Concat([_Alias]);
end;

procedure TSelectField.PrintSelfAfter(APrinter: TPrinter);
begin
  if Assigned(_As) then
  begin
    APrinter.Ruler('as', Settings.AlignFields);
    APrinter.PrintItem(_As);
  end;
  if Assigned(_Alias) then
  begin
    APrinter.Ruler('alias', Settings.AlignFields);
    APrinter.PrintItem(_Alias);
  end;
end;

{ TSelectFields }

function TSelectFields.ParseStatement(out AResult: TStatement): boolean;
begin
  Result := TSelectField.Parse(Self, Source, AResult);
end;

{ TSelectTableRef }

function TSelectTableRef.InternalParse: boolean;
begin
  _Lateral := Keyword('lateral');
  Result := inherited InternalParse;
  if Result then _On := Keyword('on');
  if Assigned(_On) then TParser.ParseExpression(Self, Source, _JoinCondition);
  if not Assigned(_On) then _Using := Keyword('using');
  if Assigned(_Using) then TSingleLine<TBracketedStatement<TIdentFields>>.Parse(Self, Source, _Fields);
end;

function TSelectTableRef.ParseTableExpression(out AResult: TStatement): boolean;
begin
  Result := TQualifiedIndexedIdent.Parse(Self, Source, AResult);
end;

procedure TSelectTableRef.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Lateral);
  inherited;
  APrinter.PrintIndented([_On, _JoinCondition, _Using, _Fields]);
end;

{ TSelectTables }

function TSelectTables.ParseDelimiter(out AResult: TObject): boolean;
begin
  Result := inherited ParseDelimiter(AResult);
  if not Result then
  begin
    AResult := Keyword(['join', 'inner join', 'full join', 'full outer join', 'left join', 'left outer join', 'right join', 'right outer join', 'cross apply', 'outer apply']);
    Result  := Assigned(AResult);
  end;
end;

procedure TSelectTables.PrintDelimiter(APrinter: TPrinter; ADelimiter: TObject);
begin
  if ADelimiter is TEpithet then
    begin
      APrinter.PrintIndented(ADelimiter);
      APrinter.NextLine;
    end
  else
    inherited;
end;

{ TGroupBy }

function TGroupBy.InternalParse: boolean;
begin
  _Group := Keyword('group');
  _By    := Keyword('by');
  if not Assigned(_Group) then exit(false);
  Result := inherited InternalParse;
end;

function TGroupBy.ParseBreak: boolean;
begin
  Result := Any([Terminal([';', ')']), Keyword('*')]);
end;

procedure TGroupBy.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Group);
  APrinter.PrintItem(_By);
  APrinter.NextLine;
  APrinter.Indent;
  inherited PrintSelf(APrinter);
  APrinter.NextLine;
  APrinter.Undent;
end;

{ THaving }

function THaving.InternalParse: boolean;
begin
  _Having := Keyword('having');
  if not Assigned(_Having) then exit(false);
  TParser.ParseExpression(Self, Source, _Condition);
  Result := true;
end;

procedure THaving.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Having, _IndentNextLine, _Condition, _Undent]);
end;


{ TStartWith }

function TStartWith.InternalParse: boolean;
begin
  _Start := Keyword('start');
  if not Assigned(_Start) then exit(false);
  _With  := Keyword('with');
  TParser.ParseExpression(Self, Source, _Condition);
  Result := true;
end;

procedure TStartWith.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Start, _With, _IndentNextLine, _Condition, _Undent]);
end;

{ TConnectBy }

function TConnectBy.InternalParse: boolean;
begin
  _Connect := Keyword('connect');
  if not Assigned(_Connect) then exit(false);
  _By := Keyword('by');
  TParser.ParseExpression(Self, Source, _Condition);
  Result := true;
end;

procedure TConnectBy.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Connect, _By, _IndentNextLine, _Condition, _Undent]);
end;

{ TAdditionalSelect }

function TAdditionalSelect.InternalParse: boolean;
begin
  _SetOperation := Keyword(['union', 'union all', 'intersect', 'minus']);
  Result := Assigned(_SetOperation) and inherited InternalParse;
end;

procedure TAdditionalSelect.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_SetOperation);
  APrinter.NextLine;
  inherited;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                               Оператор INSERT                              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TInsert.InternalParse: boolean;
begin
  Result := true;
  _Insert := Keyword('insert');
  if not Assigned(_Insert) then exit(false);
  _Into := Keyword('into');
  TTableRef.Parse(Self, Source, _Table);
  _OpenBracket := Terminal('(');
  TIdentFields.Parse(Self, Source, _Fields);
  _CloseBracket := Terminal(')');
  if TSelect.Parse(Self, Source, _Select) then exit;
  _Values := Keyword('values');
  _OpenBracket2 := Terminal('(');
  TExpressionFields.Parse(Self, Source, _ValueList);
  _CloseBracket2 := Terminal(')');
  TReturning.Parse(Self, Source, _Returning);
  inherited;
end;

procedure TInsert.PrintSelf(APrinter: TPrinter);
begin
  (_Select as TSelect).Match(_Fields as TIdentFields);
  (_ValueList as TExpressionFields).Match(_Fields as TIdentFields);
  APrinter.PrintItem(_Insert);
  APrinter.NextLine;
  APrinter.PrintItem(_Into);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_Table);
  APrinter.NextLine;
  APrinter.PrintItem(_OpenBracket);
  APrinter.PrintIndented(_Fields);
  APrinter.PrintItem(_CloseBracket);
  APrinter.Undent;
  APrinter.NextLine;
  APrinter.PrintItem(_Select);
  APrinter.PrintItem(_Values);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_OpenBracket2);
  APrinter.PrintIndented(_ValueList);
  APrinter.PrintItem(_CloseBracket2);
  APrinter.Undent;
  APrinter.NextLine;
  APrinter.PrintItem(_Returning);
  inherited;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                               Оператор UPDATE                              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

type

  { Присвоение в update }
  TUpdateAssignment = class(TStatement)
  strict private
    _Target: TStatement;
    _Assignment: TTerminal;
    _Value: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function StatementName: string; override;
  end;

  { Список присвоений в update }
  [Aligned]
  TUpdateAssignments = class(TCommaList<TUpdateAssignment>)
  strict protected
    function ParseBreak: boolean; override;
  end;

{ TUpdate }

function TUpdate.InternalParse: boolean;
begin
  _Update := Keyword('update');
  if not Assigned(_Update) then exit(false);
  TTableRef.Parse(Self, Source, _Table);
  _Set := Keyword('set');
  TUpdateAssignments.Parse(Self, Source, _Assignments);
  TWhere.Parse(Self, Source, _Where);
  TReturning.Parse(Self, Source, _Returning);
  inherited;
  Result := true;
end;

procedure TUpdate.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Update);
  APrinter.PrintIndented(_Table);
  APrinter.NextLine;
  APrinter.PrintItem(_Set);
  APrinter.PrintIndented(_Assignments);
  APrinter.PrintItem(_Where);
  APrinter.PrintItem(_Returning);
  inherited;
end;

{ TUpdateAssignment }

function TUpdateAssignment.InternalParse: boolean;
begin
  TQualifiedIdent.Parse(Self, Source, _Target);
  _Assignment := Terminal('=');
  Result := Assigned(_Target) and Assigned(_Assignment);
  if Result then TParser.ParseExpression(Self, Source, _Value);
end;

function TUpdateAssignment.StatementName: string;
begin
  if Assigned(_Target)
    then Result := _Target.StatementName
    else Result := '';
end;

procedure TUpdateAssignment.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Target);
  APrinter.Ruler('assignment');
  APrinter.PrintItem(_Assignment);
  APrinter.PrintItem(_Value);
end;

{ TUpdateAssignments }

function TUpdateAssignments.ParseBreak: boolean;
begin
  Result := Any([Terminal(';'), Keyword(['*'])]);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                               Оператор DELETE                              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TDelete.InternalParse: boolean;
begin
  _Delete := Keyword('delete');
  _From   := Keyword('from');
  if not Assigned(_Delete) or not Assigned(_From) then exit(false);
  TTableRef.Parse(Self, Source, _Table);
  TWhere.Parse(Self, Source, _Where);
  inherited;
  Result := true;
end;

procedure TDelete.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Delete);
  APrinter.NextLine;
  APrinter.PrintItem(_From);
  APrinter.PrintIndented(_Table);
  APrinter.NextLine;
  APrinter.PrintItem(_Where);
  inherited;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                               Оператор MERGE                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

type

  { Секция в merge }
  TMergeSection = class(TStatement)
  strict private
    _Section: TEpithet;
    _DML: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    function StatementName: string; override;
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Секции операторов в merge }
  TMergeSections = class(TStatementList<TMergeSection>)
  strict protected
    function ParseBreak: boolean; override;
  end;

{ TMerge }

function TMerge.InternalParse: boolean;
begin
  _Merge := Keyword('merge');
  _Into  := Keyword('into');
  if not Assigned(_Merge) or not Assigned(_Into) then exit(false);
  if not TInnerSelect.Parse(Self, Source, _DestSelect) then _DestTable := Identifier;
  _DestAlias := Identifier;
  _Using := Keyword('using');
  if not TInnerSelect.Parse(Self, Source, _SourceSelect) then _SourceTable := Identifier;
  _SourceAlias := Identifier;
  _On    := Keyword('on');
  TParser.ParseExpression(Self, Source, _Condition);
  TMergeSections.Parse(Self, Source, _Sections);
  inherited;
  Result := true;
end;

procedure TMerge.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Merge);
  APrinter.NextLine;
  APrinter.PrintItem(_Into);
  APrinter.PrintIndented([_DestSelect, _DestTable, _DestAlias]);
  APrinter.NextLine;
  APrinter.PrintItem(_Using);
  APrinter.PrintIndented([_SourceSelect, _SourceTable, _SourceAlias]);
  APrinter.NextLine;
  APrinter.PrintItem(_On);
  APrinter.PrintIndented(_Condition);
  APrinter.NextLine;
  APrinter.PrintItem(_Sections);
  inherited;
end;

{ TMergeSection }

function TMergeSection.InternalParse: boolean;
begin
  _Section := Keyword(['when matched then', 'when not matched then']);
  if not Assigned(_Section) then exit(false);
  Result := true;
  if not TInsert.Parse(Self, Source, _DML) and
     not TUpdate.Parse(Self, Source, _DML) and
     not TUnexpectedToken.Parse(Self, Source, _DML) then { ничего не делаем } ;
end;

function TMergeSection.StatementName: string;
begin
  Result := Concat([_Section]);
end;

procedure TMergeSection.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Section);
  APrinter.PrintIndented(_DML);
end;

{ TMergeSections }

function TMergeSections.ParseBreak: boolean;
begin
  Result := not Assigned(Keyword(['when matched then', 'when not matched then']));
end;

initialization
  Keywords := TStringList.Create;
  Keywords.Sorted := true;
  Keywords.Duplicates := dupIgnore;
  Keywords.CaseSensitive := false;
  Keywords.Add('comment');
  Keywords.Add('connect');
  Keywords.Add('create');
  Keywords.Add('delete');
  Keywords.Add('from');
  Keywords.Add('group');
  Keywords.Add('having');
  Keywords.Add('insert');
  Keywords.Add('intersect');
  Keywords.Add('into');
  Keywords.Add('join');
  Keywords.Add('merge');
  Keywords.Add('minus');
  Keywords.Add('on');
  Keywords.Add('order');
  Keywords.Add('select');
  Keywords.Add('set');
  Keywords.Add('start');
  Keywords.Add('union');
  Keywords.Add('update');
  Keywords.Add('using');
  Keywords.Add('values');
  Keywords.Add('where');

finalization
  FreeAndNil(Keywords);
  FreeAndNil(SqlExpressionKeywords);

end.
