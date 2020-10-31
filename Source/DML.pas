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

uses Classes, SysUtils, Math, Tokens, Streams, Statements, Printer, Commons,
  Expressions, Utils, Rulers;

type

  { Парсер DML-команд }
  DMLParser = class
  public
    class function Parse(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
  end;

  { Общий предок DML-операторов }
  TDML = class(TSemicolonStatement);

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
    function InternalGetMatchSource: TBaseStatementList; override;
  public
    function StatementName: string; override;
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
    procedure MatchChildren; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
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
    function ForcedLineBreaks: boolean; override;
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
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override; final;
  public
    procedure PrintSelfBefore(APrinter: TPrinter); virtual;
    procedure PrintSelfAfter(APrinter: TPrinter); virtual;
  end;

  TExpressionFields = class(TCommaList<TExpressionField>)
  strict protected
    function Aligned: TAlignMode; override;
  end;

implementation

uses Parser, Keywords, Select, Insert;

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
  TExists = class(TStatement)
  strict private
    _Exists: TEpithet;
    _Select: TStatement;
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
    _Nulls, _Direction: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Выражение order by }
  TOrderBy = class(TStatement)
  strict private
    _OrderBy: TEpithet;
    _Items: TStatement;
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
  if Result then TSelect.Parse(Self, Source, _Select);
end;

procedure TExists.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Exists, _IndentNextLine, _Select, _Undent]);
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
  TExpression.Parse(Self, Source, _Expression);
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
  Result := true;
  if not TExpression.Parse(Self, Source, _Expression) then exit(false);
  _Direction := Keyword(['asc', 'desc']);
  _Nulls := Keyword(['nulls', 'nulls first', 'nulls last']);
end;

procedure TOrderByItem.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Expression, _Direction, _Nulls]);
end;

{ TOrderBy }

function TOrderBy.InternalParse: boolean;
begin
  Result := true;
  _OrderBy := Keyword(['order by', 'order siblings by']);
  if not Assigned(_OrderBy) then exit(false);
  TCommaList<TOrderByItem>.Parse(Self, Source, _Items);
end;

procedure TOrderBy.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_OrderBy, _IndentNextLine, _Items, _Undent]);
end;

{ TExpressionField }

function TExpressionField.InternalParse: boolean;
begin
  Result := TExpression.Parse(Self, Source, _Expression);
end;

procedure TExpressionField.InternalPrintSelf(APrinter: TPrinter);
begin
  PrintSelfBefore(APrinter);
  APrinter.PrintItem(_Expression);
  PrintSelfAfter(APrinter);
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

function TExpressionFields.Aligned: TAlignMode;
begin
  Result := amNo; { нечего тут выравнивать, но нужно для спецкомментариев }
end;

{ TWhere }

function TWhere.InternalParse: boolean;
begin
  _Where := Keyword('where');
  TExpression.Parse(Self, Source, _Condition);
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

procedure TReturning.MatchChildren;
begin
  Match(_ReturningFields, _Targets);
end;

procedure TReturning.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.NextLineIf([_Returning, _IndentNextLine, _ReturningFields, _Undent]);
  APrinter.NextLineIf([_Into, _IndentNextLine, _Targets, _Undent]);
end;

{ TTableClause }

function TTableRef.InternalParse: boolean;
begin
  if not TBracketedStatement<TSelect>.Parse(Self, Source, _Select) then
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

function TTableRef.InternalGetMatchSource: TBaseStatementList;
begin
  Result := _Select.GetMatchSource;
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
    function Aligned: TAlignMode; override;
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
  if Result then TExpression.Parse(Self, Source, _Value);
end;

function TUpdateAssignment.StatementName: string;
begin
  if Assigned(_Target)
    then Result := _Target.StatementName
    else Result := '';
end;

procedure TUpdateAssignment.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintRulerItems('target', [_Target]);
  APrinter.PrintRulerItems('assignment', [_Assignment]);
  APrinter.PrintRulerItems('value', [_Value]);
end;

{ TUpdateAssignments }

function TUpdateAssignments.ParseBreak: boolean;
begin
  Result := Any([Terminal(';'), Keyword(['*'])]);
end;

function TUpdateAssignments.Aligned: TAlignMode;
begin
  Result := AlignMode(Settings.AlignFields);
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
  if not TBracketedStatement<TSelect>.Parse(Self, Source, _DestSelect) then TQualifiedIdent.Parse(Self, Source, _DestTable);
  _DestAlias := Identifier;
  _Using := Keyword('using');
  if not TBracketedStatement<TSelect>.Parse(Self, Source, _SourceSelect) then TQualifiedIdent.Parse(Self, Source, _SourceTable);
  _SourceAlias := Identifier;
  _On    := Keyword('on');
  TExpression.Parse(Self, Source, _Condition);
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

{ DMLParser }

class function DMLParser.Parse(AParent: TStatement;
  ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := TSelect.Parse(AParent, ASource, AResult) or
            TInsert.Parse(AParent, ASource, AResult) or
            TUpdate.Parse(AParent, ASource, AResult) or
            TDelete.Parse(AParent, ASource, AResult) or
             TMerge.Parse(AParent, ASource, AResult) or
            TCommit.Parse(AParent, ASource, AResult) or
            TRollback.Parse(AParent, ASource, AResult) or
            TSavepoint.Parse(AParent, ASource, AResult);
end;

initialization
  Keywords.RegisterOrphan(TDML);
  Keywords.RegisterKeywords(TDML, ['comment', 'connect', 'create', 'delete',
    'for', 'from', 'group', 'having', 'insert', 'intersect', 'into', 'join',
    'merge', 'minus', 'on', 'order', 'returning', 'select', 'set', 'start',
    'union', 'update', 'using', 'values', 'where']);
  Keywords.RegisterKeywords(TIdentFields, ['of', 'on', 'or']);
end.
