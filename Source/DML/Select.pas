///////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                                Команда SELECT                              //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Select;

interface

uses Statements, Tokens, Printer, DML;

type

  { Собственно, SQL-запрос }
  TSelect = class(TDML)
  strict private
    _SubQuery, _ForUpdate: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
    function InternalGetMatchSource: TBaseStatementList; override;
  end;

implementation

uses Commons, Parser, PLSQL, Expressions;

type

  { Встраиваемый запрос }
  TSubQuery = class(TStatement)
  strict private
    _Main, _OrderBy, _RowLimit, _NextQuery: TStatement;
    _Union: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
    function InternalGetMatchSource: TBaseStatementList; override;
  end;

  { Блок запроса данных }
  TQueryBlock = class(TStatement)
  strict private
    _With, _Select, _Distinct, _From: TEpithet;
    _WithList, _SelectList, _FromList, _Where, _ConnectBy, _GroupBy,
      _Model, _Into: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
    procedure MatchChildren; override;
    function InternalGetMatchSource: TBaseStatementList; override;
  end;

  { Выражение for update }
  TForUpdate = class(TStatement)
  strict private
    _For, _Update, _Of, _Mode: TEpithet;
    _Columns: TStatement;
    _WaitTime: TNumber;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Выражение order by }
  TOrderBy = class(TStatement)
  strict private
    _OrderBy: TEpithet;
    _List: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Элемент списка order by }
  TOrderByItem = class(TStatement)
  strict private
    _Expr: TStatement;
    _Direction, _Nulls: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Выражение row limit }
  TRowLimit = class(TStatement)
  strict private
    _Offset, _OffsetRows, _Fetch, _First, _Percent, _FetchRows,
      _Only, _With, _Ties: TEpithet;
    _OffsetValue, _FetchValue: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Элемент списка with }
  TWithItem = class(TStatement)
  strict private
    _Body: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { subquery factoring clause }
  TFactoring = class(TStatement)
  strict private
    _Name, _As: TEpithet;
    _Columns, _SubQuery, _Search, _Cycle: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { search clause в factoring }
  TFactoringSearch = class(TStatement)
  strict private
    _Search, _Depth, _First, _By, _Set: TEpithet;
    _Items, _Column: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { cycle clause в factoring }
  TFactoringCycle = class(TStatement)
  strict private
    _Cycle, _Set, _Alias, _To, _Default: TEpithet;
    _CycleValue, _NoCycleValue: TLiteral;
    _Columns: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Поле в select или аналогичное выражение с алиасом }
  TAliasedExpression = class(TStatement)
  strict private
    _Expression: TStatement;
    _As, _Alias: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Список полей в select }
  TAliasedExpressions = class(TCommaList<TAliasedExpression>)
  strict protected
    function Aligned: TAlignMode; override;
  end;

  { Ссылка на таблицу во from }
  TTableReference = class(TStatement)
  strict private
    _Only, _Alias: TEpithet;
    _QueryTableExpression, _Containers, _Flashback, _Pivot, _Unpivot,
      _RowPattern, _SubQuery, _JoinedTable: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция where }
  TWhere = class(TStatement)
  strict private
    _Where: TEpithet;
    _Condition: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция start with / connect by }
  TConnectBy = class(TStatement)
  strict private
    _StartWith, _ConnectBy: TEpithet;
    _StartCondition, _ConnectCondition: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция group by / having }
  TGroupBy = class(TStatement)
  strict private
    _GroupBy, _Having: TEpithet;
    _Groups, _Condition: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Элемент группировки }
  TGroupItem = class(TStatement)
  strict private
    _Expr: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция cube либо rollup }
  TCubeRollup = class(TStatement)
  strict private
    _Name: TEpithet;
    _Expr: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция grouping sets }
  TGroupingSets = class(TStatement)
  strict private
    _GroupingSets: TEpithet;
    _Items: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция model }
  TModel = class(TStatement)
  strict private
    _Model: TEpithet;
    _CellReference, _ReturnRows, _ReferenceModels, _MainModel: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция query table expression }
  TQueryTableExpression = class(TStatement)
  strict private
    _Body: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция table collection expression }
  TTableCollectionExpression = class(TStatement)
  strict private
    _Table: TEpithet;
    _Value: TStatement;
    _Outer: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция containers }
  TContainers = class(TStatement)
  strict private
    _Containers: TEpithet;
    _Value: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция lateral }
  TLateral = class(TStatement)
  strict private
    _Lateral: TEpithet;
    _OpenBracket, _CloseBracket: TTerminal;
    _SubQuery, _Restrictions: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция subquery restrictions }
  TSubqueryRestrictions = class(TStatement)
  strict private
    _With, _Constraint, _Name: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция flashback }
  TFlashback = class(TStatement)
  strict private
    _Versions, _Between, _Scn, _Timestamp, _And, _Period, _For, _ValidTimeColumn,
      _As, _Of: TEpithet;
    _Expr1, _Expr2: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция pivot }
  TPivot = class(TStatement)
  strict private
    _Pivot, _Xml: TEpithet;
    _Rest: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Внутренняя часть конструкции pivot }
  TPivotRest = class(TStatement)
  strict private
    _Aggregates, _PivotFor, _PivotIn: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция pivot for }
  TPivotFor = class(TStatement)
  strict private
    _For: TEpithet;
    _Expr: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция pivot in }
  TPivotIn = class(TStatement)
  strict private
    _In: TEpithet;
    _Rest: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Внутренняя часть конструкции pivot in }
  TPivotInRest = class(TStatement)
  strict private
    _Body: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция unpivot }
  TUnpivot = class(TStatement)
  strict private
    _Unpivot, _Include, _Nulls: TEpithet;
    _Rest: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Внутренняя часть конструкции unpivot }
  TUnpivotRest = class(TStatement)
  strict private
    _Expr, _For, _In: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Упоминание таблицы во from }
  TTableViewExpression = class(TStatement)
  strict private
    _Name, _PartitionExtension, _Sample, _Hierarchies: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Вторая и последующие таблицы в выражении join }
  TJoinedTable = class(TStatement)
  strict private
    _Partition1, _Partition2, _Natural, _Join, _On, _Using: TEpithet;
    _TableRef, _Condition, _Columns, _Next, _PartitionExpr1, _PartitionExpr2: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция row pattern }
  TRowPattern = class(TStatement)
  strict private
    _MatchRecognize: TEpithet;
    _Body: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция HIERARCHIES }
  THierarchies = class(TStatement)
  strict private
    _Hierarchies: TEpithet;
    _List: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция partition extension }
  TPartitionExtension = class(TStatement)
  strict private
    _Partition: TEpithet;
    _Value: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция sample }
  TSample = class(TStatement)
  strict private
    _Sample, _Block, _Seed: TEpithet;
    _Percent, _Value: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция into }
  TInto = class(TStatement)
  strict private
    _Into: TEpithet;
    _Targets: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
    function InternalGetMatchTarget: TBaseStatementList; override;
  end;

(*
  { Внутренняя часть конструкции row pattern }
  TRowPatternInternal = class(TStatement)
  strict private
    _Pattern, _Define: TEpithet;
    _PartitionBy, _OrderBy, _Measures, _RowsPerMatch, _SkipTo, _RowPattern,
      _Subset, _Definitions: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция cell reference в model }
  TCellReference = class(TStatement)
  strict private
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция return rows в model }
  TReturnRows = class(TStatement)
  strict private
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция reference model в model }
  TReferenceModel = class(TStatement)
  strict private
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция main model в model }
  TMainModel = class(TStatement)
  strict private
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

(*

  { Конструкция  }
  T = class(TStatement)
  strict private
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

*)

{ TSelect }

function TSelect.InternalParse: boolean;
begin
  Result := TSubquery.Parse(Self, Source, _SubQuery);
  if Result then TForUpdate.Parse(Self, Source, _ForUpdate);
  inherited;
end;

procedure TSelect.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_SubQuery, _NextLine, _ForUpdate]);
  inherited;
end;

function TSelect.InternalGetMatchSource: TBaseStatementList;
begin
  Result := _SubQuery.GetMatchSource;
end;

{ TForUpdate }

function TForUpdate.InternalParse: boolean;
begin
  Result := true;
  _For := Keyword('for');
  _Update := Keyword('update');
  if not Assigned(_For) or not Assigned(_Update) then exit(false);
  _Of := Keyword('of');
  if Assigned(_Of) then TIdentFields.Parse(Self, Source, _Columns);
  _Mode := Keyword(['wait', 'nowait', 'skip locked']);
  if Assigned(_Mode) and (_Mode.Value = 'wait') then _WaitTime := Number;
end;

procedure TForUpdate.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_For, _Update, _Of]);
  APrinter.NextLineIf([_Indent, _Columns, _UndentNextLine]);
  APrinter.PrintItems([_Mode, _WaitTime]);
end;

{ TSubQuery }

function TSubQuery.InternalGetMatchSource: TBaseStatementList;
begin
  Result := _Main.GetMatchSource;
end;

function TSubQuery.InternalParse: boolean;
begin
  Result := TBracketedStatement<TSubQuery>.Parse(Self, Source, _Main) or
            TQueryBlock.Parse(Self, Source, _Main);
  _Union := Keyword(['union all', 'union', 'intersect', 'minus']);
  if Assigned(_Union) then TSubQuery.Parse(Self, Source, _NextQuery);
  TOrderBy.Parse(Self, Source, _OrderBy);
  TRowLimit.Parse(Self, Source, _RowLimit);
end;

procedure TSubQuery.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Main, _NextLine,
                       _OrderBy, _NextLine,
                       _RowLimit, _NextLine,
                       _Union, _NextLine,
                       _NextQuery]);
end;

{ TOrderBy }

function TOrderBy.InternalParse: boolean;
begin
  _OrderBy := Keyword(['order by', 'order siblings by']);
  Result := Assigned(_OrderBy);
  if Result then TCommaList<TOrderByItem>.Parse(Self, Source, _List);
end;

procedure TOrderBy.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_OrderBy, _IndentNextLine, _List, _Undent]);
end;

{ TOrderByItem }

function TOrderByItem.InternalParse: boolean;
begin
  Result := TExpression.Parse(Self, Source, _Expr);
  if not Result then exit;
  _Direction := Keyword(['asc', 'desc']);
  _Nulls := Keyword(['nulls first', 'nulls last']);
end;

procedure TOrderByItem.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Expr, _Direction, _Nulls]);
end;

{ TRowLimit }

function TRowLimit.InternalParse: boolean;
begin
  _Offset := Keyword('offset');
  if Assigned(_Offset) then
  begin
    TExpression.Parse(Self, Source, _OffsetValue);
    _OffsetRows := Keyword(['row', 'rows']);
  end;
  _Fetch := Keyword('fetch');
  if Assigned(_Fetch) then
  begin
    _First := Keyword(['first', 'next']);
    TExpression.Parse(Self, Source, _FetchValue);
    _Percent := Keyword('percent');
    _FetchRows := Keyword(['row', 'rows']);
    _Only := Keyword('only');
    _With := Keyword('with');
    _Ties := Keyword('ties');
  end;
  Result := Assigned(_Offset) or Assigned(_Fetch);
end;

procedure TRowLimit.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Offset, _IndentNextLine,
                                _OffsetValue, _OffsetRows, _UndentNextLine,
                       _Fetch,  _IndentNextLine,
                                _First, _FetchValue, _Percent, _FetchRows,
                                _Only, _With, _Ties, _Undent]);
end;

{ TQueryBlock }

function TQueryBlock.InternalParse: boolean;
begin
  Result := true;
  _With := Keyword('with');
  if Assigned(_With) then TCommaList<TWithItem>.Parse(Self, Source, _WithList);
  _Select := Keyword('select');
  if not Assigned(_Select) then exit(false);
  _Distinct := Keyword(['distinct', 'unique', 'all']);
  TAliasedExpressions.Parse(Self, Source, _SelectList);
  TInto.Parse(Self, Source, _Into);
  _From := Keyword('from');
  TCommaList<TOptionalBracketedStatement<TTableReference>>.Parse(Self, Source, _FromList);
  TWhere.Parse(Self, Source, _Where);
  TConnectBy.Parse(Self, Source, _ConnectBy);
  TGroupBy.Parse(Self, Source, _GroupBy);
  TModel.Parse(Self, Source, _Model);
end;

procedure TQueryBlock.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_With,      _IndentNextLine,
                                   _WithList,       _UndentNextLine,
                       _Select,    _Distinct,       _IndentNextLine,
                                   _SelectList,     _UndentNextLine,
                       _Into,
                       _From,      _IndentNextLine,
                                   _FromList,       _UndentNextLine,
                       _Where,     _NextLine,
                       _ConnectBy, _NextLine,
                       _GroupBy,   _NextLine,
                       _Model]);
end;

procedure TQueryBlock.MatchChildren;
begin
  Match(_SelectList, _Into);
end;

function TQueryBlock.InternalGetMatchSource: TBaseStatementList;
begin
  Result := _SelectList.GetMatchSource;
end;

{ TWithItem }

function TWithItem.InternalParse: boolean;
begin
  Result := TSubroutine.Parse(Self, Source, _Body) or
            TFactoring.Parse(Self, Source, _Body);
end;

procedure TWithItem.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Body);
end;

{ TFactoring }

function TFactoring.InternalParse: boolean;
begin
  Result := true;
  _Name := Identifier;
  TBracketedStatement<TIdentFields>.Parse(Self, Source, _Columns);
  _As := Keyword('as');
  if not Assigned(_Name) or not Assigned(_As) then exit(false);
  TBracketedStatement<TSubQuery>.Parse(Self, Source, _SubQuery);
  TFactoringSearch.Parse(Self, Source, _Search);
  TFactoringCycle.Parse(Self, Source, _Cycle);
end;

procedure TFactoring.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Name, _Columns,  _As, _IndentNextLine,
                              _SubQuery,      _NextLine,
                              _Search,        _NextLine,
                              _Cycle,         _UndentNextLine]);
end;

{ TAliasedExpression }

function TAliasedExpression.InternalParse: boolean;
begin
  Result := TExpression.Parse(Self, Source, _Expression);
  if not Result then exit;
  _As := Keyword('as');
  _Alias := Identifier;
end;

procedure TAliasedExpression.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintRulerItems('expression', [_Expression]);
  APrinter.PrintRulerItems('as', [_As]);
  APrinter.PrintRulerItems('alias', [_Alias]);
end;

{ TWhere }

function TWhere.InternalParse: boolean;
begin
  _Where := Keyword('where');
  Result := Assigned(_Where);
  if Result then TExpression.Parse(Self, Source, _Condition);
  if _Condition is TExpression then TExpression(_Condition).IsWhereExpression := true;
end;

procedure TWhere.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Where, _IndentNextLine, _Condition, _Undent]);
end;

{ TConnectBy }

function TConnectBy.InternalParse: boolean;
begin
  Result := true;
  { start with может быть до connect by }
  _StartWith := Keyword('start with');
  if Assigned(_StartWith) then TExpression.Parse(Self, Source, _StartCondition);
  { Теперь сам connect by }
  _ConnectBy := Keyword(['connect by', 'connect by nocycle']);
  if not Assigned(_ConnectBy) then exit(false);
  TExpression.Parse(Self, Source, _ConnectCondition);
  { И start with может быть после connect by }
  if Assigned(_StartWith) then exit;
  _StartWith := Keyword('start with');
  if Assigned(_StartWith) then TExpression.Parse(Self, Source, _StartCondition);
end;

procedure TConnectBy.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_StartWith, _IndentNextLine,
                                   _StartCondition, _UndentNextLine,
                       _ConnectBy, _IndentNextLine,
                                   _ConnectCondition, _UndentNextLine]);
end;

{ TGroupBy }

function TGroupBy.InternalParse: boolean;
begin
  Result := true;
  _GroupBy := Keyword('group by');
  if not Assigned(_GroupBy) then exit(false);
  TCommaList<TGroupItem>.Parse(Self, Source, _Groups);
  _Having := Keyword('having');
  if Assigned(_Having) then TExpression.Parse(Self, Source, _Condition);
end;

procedure TGroupBy.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_GroupBy, _IndentNextLine,
                                 _Groups,         _UndentNextLine,
                       _Having,  _IndentNextLine,
                                 _Condition,      _UndentNextLine]);
end;

{ TGroupItem }

function TGroupItem.InternalParse: boolean;
begin
  Result := TCubeRollup.Parse(Self, Source, _Expr) or
            TGroupingSets.Parse(Self, Source, _Expr) or
            TExpression.Parse(Self, Source, _Expr);
end;

procedure TGroupItem.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Expr);
end;

{ TCubeRollup }

function TCubeRollup.InternalParse: boolean;
begin
  _Name := Keyword(['cube', 'rollup']);
  Result := Assigned(_Name);
  if Result then TExpression.Parse(Self, Source, _Expr);
end;

procedure TCubeRollup.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Name, _Expr]);
end;

{ TGroupingSets }

function TGroupingSets.InternalParse: boolean;
begin
  _GroupingSets := Keyword('grouping sets');
  Result := Assigned(_GroupingSets);
  if Result then TBracketedStatement<TCommaList<TGroupItem>>.Parse(Self, Source, _Items);
end;

procedure TGroupingSets.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_GroupingSets, _IndentNextLine, _Items, _UndentNextLine]);
end;

{ TFactoringSearch }

function TFactoringSearch.InternalParse: boolean;
begin
  Result := true;
  _Search := Keyword('search');
  if not Assigned(_Search) then exit(false);
  _Depth := Keyword(['depth', 'breadth']);
  _First := Keyword('first');
  _By    := Keyword('by');
  TCommaList<TOrderByItem>.Parse(Self, Source, _Items);
  _Set   := Keyword('set');
  TQualifiedIdent.Parse(Self, Source, _Column);
end;

procedure TFactoringSearch.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Search, _Depth, _IndentNextLine,
                                _First, _By,             _IndentNextLine,
                                        _Items,          _UndentNextLine,
                                _Set,   _IndentNextLine,
                                        _Column,         _Undent, _UndentNextLine]);
end;

{ TFactoringCycle }

function TFactoringCycle.InternalParse: boolean;
begin
  Result := true;
  _Cycle := Keyword('cycle');
  if not Assigned(_Cycle) then exit(false);
  TSingleLine<TIdentFields>.Parse(Self, Source, _Columns);
  _Set   := Keyword('set');
  _Alias := Identifier;
  _To    := Keyword('to');
  _CycleValue := Literal;
  _Default := Keyword('default');
  _NoCycleValue := Literal;
end;

procedure TFactoringCycle.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Cycle, _Columns, _Set, _Alias, _To, _CycleValue,
                       _Default, _NoCycleValue]);
end;

{ TTableReference }

function TTableReference.InternalParse: boolean;
begin
  _Only := Keyword('only');
  if Assigned(_Only) then
    Result := TBracketedStatement<TQueryTableExpression>.Parse(Self, Source, _QueryTableExpression)
  else
    Result := TContainers.Parse(Self, Source, _Containers) or
              TQueryTableExpression.Parse(Self, Source, _QueryTableExpression) or
              TOptionalBracketedStatement<TSubQuery>.Parse(Self, Source, _SubQuery);
  if not Result then exit;
  if Assigned(_QueryTableExpression) then
  begin
    TFlashback.Parse(Self, Source, _Flashback);
    TPivot.Parse(Self, Source, _Pivot);
    TUnpivot.Parse(Self, Source, _Unpivot);
    TRowPattern.Parse(Self, Source, _RowPattern);
  end;
  _Alias := Identifier;
  TJoinedTable.Parse(Self, Source, _JoinedTable);
end;

procedure TTableReference.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Only, _QueryTableExpression, _Containers, _SubQuery, _Indent]);
  APrinter.NextLineIf(_Flashback);
  APrinter.NextLineIf([_Pivot, _Unpivot, _RowPattern]);
  APrinter.PrintItem(_Alias);
  APrinter.Undent;
  APrinter.NextLineIf([_JoinedTable]);
end;

{ TQueryTableExpression }

function TQueryTableExpression.InternalParse: boolean;
begin
  Result := TTableCollectionExpression.Parse(Self, Source, _Body) or
            TLateral.Parse(Self, Source, _Body) or
            TTableViewExpression.Parse(Self, Source, _Body);
end;

procedure TQueryTableExpression.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Body]);
end;

{ TContainers }

function TContainers.InternalParse: boolean;
begin
  _Containers := Keyword('containers');
  Result := Assigned(_Containers);
  if Result then TSingleLine<TBracketedStatement<TQualifiedIdent>>.Parse(Self, Source, _Value);
end;

procedure TContainers.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Containers, _Value]);
end;

{ TTableCollectionExpression }

function TTableCollectionExpression.InternalParse: boolean;
begin
  _Table := Keyword('table');
  Result := Assigned(_Table);
  if not Result then exit;
  TExpression.Parse(Self, Source, _Value);
  _Outer := Terminal('(+)');
end;

procedure TTableCollectionExpression.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Table, _Value, _Outer]);
end;

{ TLateral }

function TLateral.InternalParse: boolean;
begin
  _Lateral := Keyword('lateral');
  Result := Assigned(_Lateral);
  if not Result then exit;
  _OpenBracket := Terminal('(');
  TSubQuery.Parse(Self, Source, _SubQuery);
  TSubQueryRestrictions.Parse(Self, Source, _Restrictions);
  _CloseBracket := Terminal(')');
end;

procedure TLateral.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Lateral, _IndentNextLine,
                                 _OpenBracket,    _IndentNextLine,
                                                  _SubQuery,       _NextLine,
                                                  _Restrictions,   _UndentNextLine,
                                 _CloseBracket,   _Undent]);
end;

{ TModel }

function TModel.InternalParse: boolean;
begin
  Result := true;
  _Model := Keyword('model');
  if not Assigned(_Model) then exit(false);
  (*
  TCellReference.Parse(Self, Source, _CellReference);
  TReturnRows.Parse(Self, Source, _ReturnRows);
  TStrictStatementList<TReferenceModel>.Parse(Self, Source, _ReferenceModels);
  TMainModel.Parse(Self, Source, _MainModel);
  *)
end;

procedure TModel.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Model, _CellReference, _ReturnRows, _ReferenceModels, _MainModel]);
end;

{ TSubqueryRestrictions }

function TSubqueryRestrictions.InternalParse: boolean;
begin
  Result := true;
  _With := Keyword(['with check option', 'with read only']);
  if not Assigned(_With) then exit(false);
  _Constraint := Keyword('constraint');
  _Name := Identifier;
end;

procedure TSubqueryRestrictions.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_With, _Constraint, _Name]);
end;

{ TFlashback }

function TFlashback.InternalParse: boolean;
begin
  _Versions := Keyword('versions');
  if Assigned(_Versions) then
  begin
    _Period := Keyword('period');
    _For := Keyword('for');
    if Assigned(_Period) or Assigned(_For) then _ValidTimeColumn := Identifier;
    _Between := Keyword('between');
    _Scn := Keyword('scn');
    _Timestamp := Keyword('timestamp');
    TExpression.Parse(Self, Source, _Expr1);
    _And := Keyword('and');
    TExpression.Parse(Self, Source, _Expr2);
    exit(true);
  end;
  _As := Keyword('as');
  if Assigned(_As) then
  begin
    _Of := Keyword('of');
    _Scn := Keyword('scn');
    _Timestamp := Keyword('timestamp');
    _Period := Keyword('period');
    _For := Keyword('for');
    if Assigned(_Period) or Assigned(_For) then _ValidTimeColumn := Identifier;
    TExpression.Parse(Self, Source, _Expr1);
    exit(true);
  end;
  Result := false;
end;

procedure TFlashback.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Versions, _As, _Of, _Period, _For, _ValidTimeColumn,
    _Between, _Scn, _Timestamp, _Expr1, _And, _Expr2]);
end;

{ TPivot }

function TPivot.InternalParse: boolean;
begin
  Result := true;
  _Pivot := Keyword('pivot');
  if not Assigned(_Pivot) then exit(false);
  _Xml   := Keyword('xml');
  TBracketedStatement<TPivotRest>.Parse(Self, Source, _Rest);
end;

procedure TPivot.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Pivot, _Xml, _Rest]);
end;

{ TPivotRest }

function TPivotRest.InternalParse: boolean;
begin
  Result := TAliasedExpressions.Parse(Self, Source, _Aggregates);
  if not Result then exit;
  TPivotFor.Parse(Self, Source, _PivotFor);
  TPivotIn.Parse(Self, Source, _PivotIn);
end;

procedure TPivotRest.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Aggregates, _PivotFor, _PivotIn]);
end;

{ TPivotFor }

function TPivotFor.InternalParse: boolean;
begin
  Result := true;
  _For := Keyword('for');
  if not Assigned(_For) then exit(false);
  TExpression.Parse(Self, Source, _Expr);
end;

procedure TPivotFor.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_For, _Expr]);
end;

{ TPivotIn }

function TPivotIn.InternalParse: boolean;
begin
  _In := Keyword('in');
  Result := Assigned(_In);
  if Result then TPivotInRest.Parse(Self, Source, _Rest);
end;

procedure TPivotIn.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_In, _Rest]);
end;

{ TPivotInRest }

function TPivotInRest.InternalParse: boolean;
begin
  Result := TSubQuery.Parse(Self, Source, _Body) or
            TAliasedExpressions.Parse(Self, Source, _Body);
end;

procedure TPivotInRest.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Body);
end;

{ TUnpivot }

function TUnpivot.InternalParse: boolean;
begin
  Result := true;
  _Unpivot := Keyword('unpivot');
  if not Assigned(_Unpivot) then exit(false);
  _Include := Keyword(['include', 'exclude']);
  _Nulls := Keyword('nulls');
  TBracketedStatement<TUnpivotRest>.Parse(Self, Source, _Rest);
end;

procedure TUnpivot.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Unpivot, _Include, _Nulls, _Rest]);
end;

{ TUnpivotRest }

function TUnpivotRest.InternalParse: boolean;
begin
  Result := true;
  TExpression.Parse(Self, Source, _Expr);
  TPivotFor.Parse(Self, Source, _For);
  TPivotIn.Parse(Self, Source, _In);
end;

procedure TUnpivotRest.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Expr, _For, _In]);
end;

{ TRowPattern }

function TRowPattern.InternalParse: boolean;
begin
  _MatchRecognize := Keyword('match_recognize');
  Result := Assigned(_MatchRecognize);
  (*
  if Result then TBracketedStatement<TRowPatternInternal>.Parse(Self, Source, _Body);
  *)
end;

procedure TRowPattern.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_MatchRecognize, _Body]);
end;

{ TTableViewExpression }

function TTableViewExpression.InternalParse: boolean;
begin
  Result := false;
  if not TQualifiedIndexedIdent.Parse(Self, Source, _Name) then exit;
  THierarchies.Parse(Self, Source, _Hierarchies);
  TPartitionExtension.Parse(Self, Source, _PartitionExtension);
  TSample.Parse(Self, Source, _Sample);
  Result := true;
end;

procedure TTableViewExpression.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Name, _PartitionExtension, _Hierarchies, _Sample]);
end;

{ THierarchies }

function THierarchies.InternalParse: boolean;
begin
  Result := false;
  _Hierarchies := Keyword('hierarchies');
  if not Assigned(_Hierarchies) then exit;
  Result := TSingleLine<TBracketedStatement<TCommaList<TQualifiedIdent>>>.Parse(Self, Source, _List);
end;

procedure THierarchies.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Hierarchies, _List]);
end;

{ TPartitionExtension }

function TPartitionExtension.InternalParse: boolean;
begin
  _Partition := Keyword(['partition', 'partition for', 'subpartition', 'subpartition for']);
  Result := Assigned(_Partition) and
            TSingleLine<TBracketedStatement<TCommaList<TExpression>>>.Parse(Self, Source, _Value);
end;

procedure TPartitionExtension.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Partition, _Value]);
end;

{ TSample }

function TSample.InternalParse: boolean;
begin
  _Sample := Keyword('sample');
  if not Assigned(_Sample) then exit(false);
  _Block := Keyword('block');
  TExpression.Parse(Self, Source, _Percent);
  _Seed := Keyword('seed');
  TExpression.Parse(Self, Source, _Value);
  Result := true;
end;

procedure TSample.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Sample, _Block, _Percent, _Seed, _Value]);
end;

{ TJoinedTable }

function TJoinedTable.InternalParse: boolean;
begin
  Result := true;
  _Partition1 := Keyword('partition by');
  if Assigned(_Partition1) then TExpression.Parse(Self, Source, _PartitionExpr1);
  _Natural := Keyword('natural');
  _Join := Keyword(['join', 'inner join', 'cross join', 'cross apply', 'outer apply', 'full join', 'left join', 'right join', 'full outer join', 'left outer join', 'right outer join']);
  if not Assigned(_Join) then exit(false);
  if not TTableCollectionExpression.Parse(Self, Source, _TableRef) then
    TTableReference.Parse(Self, Source, _TableRef);
  _Partition2 := Keyword('partition by');
  if Assigned(_Partition2) then TExpression.Parse(Self, Source, _PartitionExpr2);
  _On := Keyword('on');
  if Assigned(_On) then TExpression.Parse(Self, Source, _Condition);
  _Using := Keyword('using');
  if Assigned(_Using) then TSingleLine<TBracketedStatement<TIdentFields>>.Parse(Self, Source, _Columns);
  TJoinedTable.Parse(Self, Source, _Next);
end;

procedure TJoinedTable.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.Indent;
  APrinter.NextLineIf([_Partition1, _PartitionExpr1]);
  APrinter.PrintItems([_NextLine,
                       _Natural, _Join, _UndentNextLine,
            _TableRef, _IndentNextLine]);
  APrinter.NextLineIf([_Partition2, _PartitionExpr2]);
  APrinter.PrintItems([_On, _Using, _IndentNextLine,
                            _Condition, _Columns, _UndentNextLine, _Undent,
                       _Next]);
end;

{ TInto }

function TInto.InternalParse: boolean;
begin
  _Into := Keyword(['into', 'bulk collect into']);
  Result := Assigned(_Into);
  if Result then TIdentFields.Parse(Self, Source, _Targets);
end;

procedure TInto.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Into, _IndentNextLine, _Targets, _Undent, _NextLine]);
end;

function TInto.InternalGetMatchTarget: TBaseStatementList;
begin
  Result := TBaseStatementList(_Targets);
end;

{ TAliasedExpressions }

function TAliasedExpressions.Aligned: TAlignMode;
begin
  Result := AlignMode(Settings.AlignFields);
end;

end.
