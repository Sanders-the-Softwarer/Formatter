////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                       Синтаксические конструкции DML                       //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit DML;

interface

uses Classes, SysUtils, Math, Tokens, Statements, Printer, Commons,
  Expressions, Utils;

type

  { Общий предок DML-операторов }
  TDML = class(TSemicolonStatement);

  { Расширение понятия операнда для SQL-выражений }
  TSqlTerm = class(TTerm)
  strict protected
    function ParseSQLStatement: TStatement; override;
  end;

  { SQL-выражение }
  TSqlExpression = class(TExpression)
  strict protected
    function ParseStatement(out AResult: TStatement): boolean; override;
    function ForcedLineBreaks: boolean; override;
  public
    function Aligned: boolean; override;
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
    _ForUpdate: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalMatch(AStatement: TStatement); override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор insert }
  TInsert = class(TDML)
  strict private
    _Insert, _Into, _Values: TEpithet;
    _Table, _Fields, _ValueList, _Returning: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalMatchChildren; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
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
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор delete }
  TDelete = class(TDML)
  strict private
    _Delete, _From: TEpithet;
    _Table, _Where, _Returning: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор merge }
  TMerge = class(TDML)
  strict private
    _Merge, _Into, _SourceAlias, _DestAlias, _Using, _On: TEpithet;
    _SourceSelect, _SourceTable, _DestSelect, _DestTable, _Condition, _Sections: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор commit }
  TCommit = class(TDML)
  strict private
    _Commit: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор rollback }
  TRollback = class(TDML)
  strict private
    _Rollback, _To, _Savepoint, _Name: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор savepoint }
  TSavepoint = class(TDML)
  strict private
    _Savepoint, _Name: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Вложенный запрос }
  TInnerSelect = class(TStatement)
  strict private
    _OpenBracket: TTerminal;
    _Select: TStatement;
    _CloseBracket: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Идентификатор как поле в insert, select into итп }
  TIdentField = class(TStatement)
  strict private
    _FieldName: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
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
    procedure InternalPrintSelf(APrinter: TPrinter); override; final;
  public
    procedure PrintSelfBefore(APrinter: TPrinter); virtual;
    procedure PrintSelfAfter(APrinter: TPrinter); virtual;
    property MatchedTo: TIdentField read _Match write _Match;
  end;

  TExpressionFields = class(TCommaList<TExpressionField>)
  strict protected
    procedure InternalMatch(AStatement: TStatement); override;
  public
    function Aligned: boolean; override;
  end;

implementation

uses Parser, Keywords;

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
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Условие exists }
  TExists = class(TInnerSelect)
  strict private
    _Exists: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция within group }
  TWithinGroup = class(TStatement)
  strict private
    _Within, _Group: TEpithet;
    _OrderBy: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция over }
  TOver = class(TStatement)
  strict private
    _Over: TEpithet;
    _OpenBracket: TTerminal;
    _PartitionBy: TEpithet;
    _PartitionFields: TStatement;
    _OrderBy: TStatement;
    _CloseBracket: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
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
    procedure InternalPrintSelf(APrinter: TPrinter); override;
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
    procedure InternalPrintSelf(APrinter: TPrinter); override;
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
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Строка выражения order by }
  TOrderByItem = class(TStatement)
  strict private
    _Expression: TStatement;
    _Nulls, _Position, _Direction: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Выражение order by }
  TOrderBy = class(TCommaList<TOrderByItem>)
  strict private
    _Order, _Siblings, _By: TEpithet;
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

  { Конструкция returning }
  TReturning = class(TStatement)
  strict private
    _Returning: TEpithet;
    _ReturningFields: TStatement;
    _Into: TEpithet;
    _Targets: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalMatchChildren; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция for update }
  TForUpdate = class(TStatement)
  strict private
    _For, _Update, _Of, _WaitMode, _Skip, _Locked: TEpithet;
    _WaitTime: TNumber;
    _Columns: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
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
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
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

function TSQLExpression.ForcedLineBreaks: boolean;
begin
  Result := true;
end;

function TSQLExpression.Aligned: boolean;
begin
  Result := true;
end;

{ TAsterisk }

function TAsterisk.InternalParse: boolean;
begin
  TQualifiedIdent.Parse(Self, Source, _Prefix);
  _Dot := Terminal('.');
  _Star := Terminal('*');
  Result := Assigned(_Star) and (Assigned(_Dot) = Assigned(_Prefix));
end;

procedure TAsterisk.InternalPrintSelf(APrinter: TPrinter);
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

procedure TExists.InternalPrintSelf(APrinter: TPrinter);
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

procedure TWithinGroup.InternalPrintSelf(APrinter: TPrinter);
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
  _PartitionBy := Keyword(['partition by', 'partition']);
  if Assigned(_PartitionBy) then TExpressionFields.Parse(Self, Source, _PartitionFields);
  TOrderBy.Parse(Self, Source, _OrderBy);
  _CloseBracket := Terminal(')');
  Result := true;
end;

procedure TOver.InternalPrintSelf(APrinter: TPrinter);
begin
  if Assigned(_PartitionBy) or Assigned(_OrderBy) then
    APrinter.PrintItems([_Over, _IndentNextLine,
                                _OpenBracket,    _IndentNextLine,
                                                 _PartitionBy,    _IndentNextLine,
                                                                  _PartitionFields, _UndentNextLine,
                                                 _OrderBy, _UndentNextLine,
                                _CloseBracket,   _Undent])
  else
    APrinter.PrintItems([_Over, _OpenBracket, _CloseBracket]);
end;

{ TKeep }

function TKeep.InternalParse: boolean;
begin
  _Keep := Keyword('keep');
  if not Assigned(_Keep) then exit(false);
  _OpenBracket := Terminal('(');
  _Rank := Keyword(['rank', 'dense_rank']);
  _FirstOrLast := Keyword(['first', 'last']);
  TOrderBy.Parse(Self, Source, _OrderBy);
  _CloseBracket := Terminal(')');
  Result := true;
end;

procedure TKeep.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Keep, _IndentNextLine,
                              _OpenBracket, _IndentNextLine,
                                            _Rank, _NextLine,
                                            _FirstOrLast, _NextLine,
                                            _OrderBy, _UndentNextLine,
                              _CloseBracket, _Undent]);
end;

{ TAnalyticFunctionCall }

function TSelectFunctionCall.InternalParse: boolean;
begin
  TListAgg.Parse(Self, Source, _FunctionCall);
  if not Assigned(_FunctionCall) then TQualifiedIndexedIdent.Parse(Self, Source, _FunctionCall);
  if not Assigned(_FunctionCall) then exit(false);
  TWithinGroup.Parse(Self, Source, _WithinGroup);
  TKeep.Parse(Self, Source, _Keep);
  TOver.Parse(Self, Source, _Over);
  Result := Assigned(_WithinGroup) or Assigned(_Keep) or Assigned(_Over);
end;

procedure TSelectFunctionCall.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_FunctionCall, _IndentNextLine, _WithinGroup, _NextLine, _Keep, _NextLine, _Over, _Undent]);
end;

{ TListagg }

function TListagg.InternalParse: boolean;
begin
  _ListAgg := Keyword('listagg');
  if not Assigned(_ListAgg) then exit(false);
  _OpenBracket := Terminal('(');
  TParser.ParseExpression(Self, Source, _Expression);
  _Comma := Terminal(',');
  if Assigned(_Comma) then _Delimiter := Literal;
  _On := Keyword('on');
  _Overflow := Keyword('overflow');
  _Truncate := Keyword('truncate');
  _OverflowTag := Literal;
  _Without := Keyword('without');
  _Count := Keyword('count');
  _CloseBracket := Terminal(')');
  Result := true;
end;

procedure TListagg.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_ListAgg, _OpenBracket, _Expression, _Comma, _Delimiter,
    _On, _Overflow, _Truncate, _OverflowTag, _Without, _Count, _CloseBracket]);
end;

{ TIdentField }

function TIdentField.InternalParse: boolean;
begin
  Result := TQualifiedIndexedIdent.Parse(Self, Source, _FieldName);
end;

function TIdentField.StatementName: string;
begin
  Result := _FieldName.StatementName;
end;

procedure TIdentField.InternalPrintSelf(APrinter: TPrinter);
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

procedure TOrderByItem.InternalPrintSelf(APrinter: TPrinter);
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

procedure TOrderBy.InternalPrintSelf(APrinter: TPrinter);
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

procedure TExpressionField.InternalPrintSelf(APrinter: TPrinter);
begin
  PrintSelfBefore(APrinter);
  APrinter.PrintItem(_Expression);
  PrintSelfAfter(APrinter);
  if Assigned(_Match) then
    (Parent as TExpressionFields).PrintSpecialCommentAfterDelimiter('=> ' + TIdentField(_Match).Name);
end;

procedure TExpressionField.PrintSelfBefore(APrinter: TPrinter);
begin
  { ничего не делаем }
end;

procedure TExpressionField.PrintSelfAfter(APrinter: TPrinter);
begin
  { ничего не делаем }
end;

{ TExpressionFields }

function TExpressionFields.Aligned: boolean;
begin
  Result := true;
end;

procedure TExpressionFields.InternalMatch(AStatement: TStatement);
var
  Count, i: integer;
  AFields: TIdentFields absolute AStatement;
begin
  if not (AStatement is TIdentFields) then exit;
  Count := Math.Min(Self.Count, AFields.Count);
  Assert(Settings <> nil);
  if Count < Settings.MatchParamLimit then exit;
  for i := 0 to Count - 1 do
    if (Self.Item(i) is TExpressionField) and (AFields.Item(i) is TIdentField) then
      TExpressionField(Self.Item(i)).MatchedTo := TIdentField(AFields.Item(i));
end;

{ TWhere }

function TWhere.InternalParse: boolean;
begin
  _Where := Keyword('where');
  TParser.ParseExpression(Self, Source, _Condition);
  Result := Assigned(_Where);
end;

procedure TWhere.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.NextLineIf([_Where, _IndentNextLine, _Condition, _Undent]);
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

procedure TReturning.InternalMatchChildren;
begin
  _ReturningFields.Match(_Targets);
end;

procedure TReturning.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.NextLineIf([_Returning, _IndentNextLine, _ReturningFields, _Undent]);
  APrinter.NextLineIf([_Into, _IndentNextLine, _Targets, _Undent]);
end;

{ TForUpdate }

function TForUpdate.InternalParse: boolean;
begin
  _For := Keyword('for');
  if not Assigned(_For) then exit(false);
  _Update := Keyword('update');
  _Of := Keyword('of');
  if Assigned(_Of) then TIdentFields.Parse(Self, Source, _Columns);
  _WaitMode := Keyword(['wait', 'nowait']);
  if Assigned(_WaitMode) then _WaitTime := Number;
  _Skip := Keyword('skip');
  _Locked := Keyword('locked');
  Result := true;
end;

procedure TForUpdate.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_For, _Update, _Of]);
  APrinter.NextLineIf([_Indent, _Columns,  _UndentNextLine]);
  APrinter.PrintItems([_WaitMode, _WaitTime, _Skip, _Locked]);
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

procedure TTableRef.InternalPrintSelf(APrinter: TPrinter);
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
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
  end;

  { Конструкция with }
  TWith = class(TCommaList<TWithItem>)
  strict private
    _With: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    function ParseBreak: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Поле в select }
  TSelectField = class(TExpressionField)
  strict private
    _As: TEpithet;
    _Alias: TEpithet;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelfBefore(APrinter: TPrinter); override;
    procedure PrintSelfAfter(APrinter: TPrinter); override;
    function StatementName: string; override;
  end;

  { Список полей в select }
  TSelectFields = class(TExpressionFields)
  strict protected
    function ParseStatement(out AResult: TStatement): boolean; override;
    function Aligned: boolean; override;
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
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Список таблиц в select }
  TSelectTables = class(TCommaList<TSelectTableRef>)
  strict protected
    function ParseDelimiter(out AResult: TObject): boolean; override;
    procedure PrintDelimiter(APrinter: TPrinter; ADelimiter: TObject; ALast: boolean); override;
  end;

  { Выражение group by }
  TGroupBy = class(TCommaList<TExpressionField>)
  strict private
    _Group, _By: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    function ParseBreak: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Выражение having }
  THaving = class(TStatement)
  strict private
    _Having: TEpithet;
    _Condition: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Выражение start with }
  TStartWith = class(TStatement)
  strict private
    _StartWith: TEpithet;
    _Condition: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

    { Выражение start with }
  TConnectBy = class(TStatement)
  strict private
    _ConnectBy: TEpithet;
    _Condition: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Следующий запрос в цепочке union all }
  TAdditionalSelect = class(TSelect)
  strict private
    _SetOperation: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
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
  TForUpdate.Parse(Self, Source, _ForUpdate);
  inherited;
  Result := true;
end;

procedure TSelect.InternalPrintSelf(APrinter: TPrinter);
begin
  TExpressionFields(_Fields).Match(TIdentFields(_IntoFields));
  APrinter.PrintItems([_With,
                       _Select,           _Mode,           _IndentNextLine,
                                          _Fields,         _UndentNextLine,
                       _Into,             _IndentNextLine,
                                          _IntoFields,     _UndentNextLine,
                       _From,             _IndentNextLine,
                                          _Tables,         _UndentNextLine,
                       _Where,            _NextLine,
                       _StartWith,        _NextLine,
                       _ConnectBy,        _NextLine,
                       _GroupBy,          _NextLine,
                       _Having,           _NextLine,
                       _OrderBy,          _NextLine,
                       _AdditionalSelect, _NextLine,
                       _ForUpdate]);
  inherited;
end;

procedure TSelect.InternalMatch(AStatement: TStatement);
begin
  _Fields.Match(AStatement);
end;

{ TInnerSelect }

function TInnerSelect.InternalParse: boolean;
begin
  _OpenBracket := Terminal('(');
  TSelect.Parse(Self, Source, _Select);
  _CloseBracket := Terminal(')');
  Result := Assigned(_OpenBracket) and Assigned(_Select);
end;

procedure TInnerSelect.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_OpenBracket, _IndentNextLine, _Select, _UndentNextLine, _CloseBracket]);
end;

{ TWithItem }

function TWithItem.InternalParse: boolean;
begin
  _Alias := Identifier;
  _As    := Keyword('as');
  TInnerSelect.Parse(Self, Source, _Select);
  Result := Assigned(_Alias);
end;

procedure TWithItem.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Alias, _As, _IndentNextLine, _Select, _Undent]);
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

procedure TWith.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_With, _IndentNextLine]);
  inherited;
  APrinter.PrintItems([_UndentNextLine]);
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

procedure TSelectField.PrintSelfBefore(APrinter: TPrinter);
begin
  APrinter.StartRuler(Settings.AlignFields);
end;

procedure TSelectField.PrintSelfAfter(APrinter: TPrinter);
begin
  APrinter.PrintRulerItems('as', [_As]);
  APrinter.PrintRulerItems('alias', [_Alias]);
end;

{ TSelectFields }

function TSelectFields.Aligned: boolean;
begin
  Result := true;
end;

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

procedure TSelectTableRef.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Lateral);
  inherited;
  APrinter.Indent;
  APrinter.NextLineIf([_On, _IndentNextLine, _JoinCondition, _Undent]);
  APrinter.NextLineIf([_Using, _IndentNextLine, _Fields, _Undent]);
  APrinter.Undent;
end;

{ TSelectTables }

function TSelectTables.ParseDelimiter(out AResult: TObject): boolean;
begin
  Result := inherited ParseDelimiter(AResult);
  if not Result then
  begin
    AResult := Keyword(['join', 'inner join', 'full join', 'full natural join', 'full outer join', 'left join', 'left natural join', 'left outer join', 'right join', 'right natural join', 'right outer join', 'cross apply', 'outer apply']);
    Result  := Assigned(AResult);
  end;
end;

procedure TSelectTables.PrintDelimiter(APrinter: TPrinter; ADelimiter: TObject; ALast: boolean);
begin
  if ADelimiter is TEpithet then
    begin
      APrinter.PrintIndented(ADelimiter);
      if not ALast then APrinter.NextLine;
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

procedure TGroupBy.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Group, _By, _IndentNextLine]);
  inherited;
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

procedure THaving.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Having, _IndentNextLine, _Condition, _Undent]);
end;

{ TStartWith }

function TStartWith.InternalParse: boolean;
begin
  Result := true;
  _StartWith := Keyword('start with');
  if not Assigned(_StartWith) then exit;
  TParser.ParseExpression(Self, Source, _Condition);
end;

procedure TStartWith.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_StartWith, _IndentNextLine, _Condition, _Undent]);
end;

{ TConnectBy }

function TConnectBy.InternalParse: boolean;
begin
  Result := true;
  _ConnectBy := Keyword('connect by');
  if not Assigned(_ConnectBy) then exit(false);
  TParser.ParseExpression(Self, Source, _Condition);
end;

procedure TConnectBy.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_ConnectBy, _IndentNextLine, _Condition, _Undent]);
end;

{ TAdditionalSelect }

function TAdditionalSelect.InternalParse: boolean;
begin
  _SetOperation := Keyword(['union', 'union all', 'intersect', 'minus']);
  Result := Assigned(_SetOperation) and inherited InternalParse;
end;

procedure TAdditionalSelect.InternalPrintSelf(APrinter: TPrinter);
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
  TBracketedStatement<TIdentFields>.Parse(Self, Source, _Fields);
  _Values := Keyword('values');
  if not Assigned(_Values) then TSelect.Parse(Self, Source, _ValueList);
  if not Assigned(_ValueList) then TBracketedStatement<TExpressionFields>.Parse(Self, Source, _ValueList);
  if not Assigned(_ValueList) then TParser.ParseExpression(Self, Source, _ValueList);
  TReturning.Parse(Self, Source, _Returning);
  inherited;
end;

procedure TInsert.InternalMatchChildren;
begin
  if not Assigned(_Fields) then exit;
  _ValueList.Match(TBracketedStatement<TIdentFields>(_Fields).InnerStatement);
end;

procedure TInsert.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Insert, _NextLine,
                       _Into,   _IndentNextLine,
                                _Table, _NextLine,
                                _Fields, _UndentNextLine,
                       _Values, _IndentNextLine]);
  if not Assigned(_Values) then APrinter.Undent;
  APrinter.PrintItem(_ValueList);
  if Assigned(_Values) then APrinter.Undent;
  APrinter.PrintItems([_NextLine, _Returning]);
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
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
  end;

  { Список присвоений в update }
  TUpdateAssignments = class(TCommaList<TUpdateAssignment>)
  strict protected
    function ParseBreak: boolean; override;
    function Aligned: boolean; override;
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

procedure TUpdate.InternalPrintSelf(APrinter: TPrinter);
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

procedure TUpdateAssignment.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.StartRuler(Settings.AlignFields);
  APrinter.PrintRulerItems('target', [_Target]);
  APrinter.PrintRulerItems('assignment', [_Assignment]);
  APrinter.PrintRulerItems('value', [_Value]);
end;

{ TUpdateAssignments }

function TUpdateAssignments.Aligned: boolean;
begin
  Result := true;
end;

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
  if not Assigned(_Delete) then exit(false);
  TTableRef.Parse(Self, Source, _Table);
  TWhere.Parse(Self, Source, _Where);
  TReturning.Parse(Self, Source, _Returning);
  inherited;
  Result := true;
end;

procedure TDelete.InternalPrintSelf(APrinter: TPrinter);
begin
  if Settings.AddFromToDelete and not Assigned(_From) then
    _From := TEpithet.Create('from', -1, -1);
  APrinter.PrintItems([_Delete, _NextLine,
                       _From,   _IndentNextLine,
                                _Table, _UndentNextLine,
                       _Where, _NextLine,
                       _Returning]);
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
    _Delete: TEpithet;
    _Where: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
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
  if not TInnerSelect.Parse(Self, Source, _DestSelect) then TQualifiedIdent.Parse(Self, Source, _DestTable);
  _DestAlias := Identifier;
  _Using := Keyword('using');
  if not TInnerSelect.Parse(Self, Source, _SourceSelect) then TQualifiedIdent.Parse(Self, Source, _SourceTable);
  _SourceAlias := Identifier;
  _On    := Keyword('on');
  TParser.ParseExpression(Self, Source, _Condition);
  TMergeSections.Parse(Self, Source, _Sections);
  inherited;
  Result := true;
end;

procedure TMerge.InternalPrintSelf(APrinter: TPrinter);
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
  if TInsert.Parse(Self, Source, _DML) or
     TUpdate.Parse(Self, Source, _DML) then
    begin
      if TDML(_DML).HasSemicolon then exit;
      _Delete := Keyword('delete');
      TWhere.Parse(Self, Source, _Where);
    end
  else
    TUnexpectedToken.Parse(Self, Source, _DML);
end;

function TMergeSection.StatementName: string;
begin
  Result := Concat([_Section]);
end;

procedure TMergeSection.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Section, _IndentNextLine, _DML]);
  APrinter.NextLineIf([_Delete]);
  APrinter.NextLineIf([_Where]);
  APrinter.Undent;
end;

{ TMergeSections }

function TMergeSections.ParseBreak: boolean;
begin
  Result := not Assigned(Keyword(['when matched then', 'when not matched then']));
end;

{ TCommit }

function TCommit.InternalParse: boolean;
begin
  _Commit := Keyword('commit');
  inherited;
  Result  := Assigned(_Commit);
end;

procedure TCommit.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Commit);
  inherited;
end;

{ TRollback }

function TRollback.InternalParse: boolean;
begin
  _Rollback := Keyword('rollback');
  _To := Keyword('to');
  _Savepoint := Keyword('savepoint');
  _Name := Identifier;
  inherited;
  Result := Assigned(_Rollback);
end;

procedure TRollback.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Rollback, _To, _Savepoint, _Name]);
  inherited;
end;

{ TSavepoint }

function TSavepoint.InternalParse: boolean;
begin
  _Savepoint := Keyword('savepoint');
  _Name := Identifier;
  inherited;
  Result := Assigned(_Savepoint);
end;

procedure TSavepoint.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Savepoint, _Name]);
  inherited;
end;

initialization
  Keywords.RegisterOrphan(TDML);
  Keywords.RegisterKeywords(TDML, ['comment', 'connect', 'create', 'delete',
    'for', 'from', 'group', 'having', 'insert', 'intersect', 'into', 'join',
    'merge', 'minus', 'on', 'order', 'returning', 'select', 'set', 'start',
    'union', 'update', 'using', 'values', 'where']);
  Keywords.RegisterKeywords(TIdentFields, ['of', 'on', 'or']);
end.
