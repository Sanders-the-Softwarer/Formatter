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

uses Math, Tokens, Statements, PLSQL, Printers_, Attributes;

type

  { Оператор select }
  TSelect = class(TStatement)
  strict private
    _Select: TKeyword;
    _Star: TTerminal;
    _Fields: TStatement;
    _Into: TKeyword;
    _IntoFields: TStatement;
    _From: TKeyword;
    _Tables: TStatement;
    _Where: TStatement;
    _GroupBy: TStatement;
    _OrderBy: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    procedure Match(AFields: TStatement);
  end;

  { Оператор insert }
  TInsert = class(TStatement)
  strict private
    _Insert: TKeyword;
    _Into: TKeyword;
    _TableName: TIdent;
    _OpenBracket: TTerminal;
    _Fields: TStatement;
    _CloseBracket: TTerminal;
    _Select: TStatement;
    _Values: TKeyword;
    _OpenBracket2: TTerminal;
    _ValueList: TStatement;
    _CloseBracket2: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор update }
  TUpdate = class(TStatement)
  strict private
    _Update: TKeyword;
    _TableName: TIdent;
    _Alias: TIdent;
    _Set: TKeyword;
    _Assignments: TStatement;
    _Where: TStatement;
    _Returning: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор delete }
  TDelete = class(TStatement)
  strict private
    _Delete: TKeyword;
    _From: TKeyword;
    _Table: TStatement;
    _Where: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор merge }
  TMerge = class(TStatement)
  strict private
    _Merge: TKeyword;
    _Into: TKeyword;
    _DestSelect: TStatement;
    _DestTable: TIdent;
    _DestAlias: TIdent;
    _Using: TKeyword;
    _SourceSelect: TStatement;
    _SourceTable: TIdent;
    _SourceAlias: TIdent;
    _On: TKeyword;
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

implementation

uses Expressions;

type

  { Присвоение в update }
  TUpdateAssignment = class(TStatement)
  strict private
    _Target: TIdent;
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

  { Секция в merge }
  TMergeSection = class(TStatement)
  strict private
    _Section: TKeyword;
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

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                         Общие элементы конструкций                         //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

type

  { Идентификатор как поле в insert, select into итп }
  TIdentField = class(TStatement)
  strict private
    _FieldName: TIdent;
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

  { Выражение как поле в group by, insert values и т. п. }
  TExpressionField = class(TStatement)
  strict private
    _Expression: TStatement;
    _Match: TIdentField;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    property Match: TIdentField read _Match write _Match;
  end;

  { Список выражений }
  [Aligned]
  TExpressionFields = class(TCommaList<TExpressionField>)
  strict protected
    function ParseBreak: boolean; override;
  public
    procedure Match(AFields: TIdentFields);
  end;

  { Конструкция where }
  TWhere = class(TStatement)
  strict private
    _Where: TKeyword;
    _Condition: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция returning }
  TReturning = class(TStatement)
  strict private
    _Returning: TKeyword;
    _ReturningFields: TStatement;
    _Into: TKeyword;
    _Targets: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Указание таблицы - в select, merge, delete и т. п. }
  TTableClause = class(TStatement)
  strict private
    _Select: TStatement;
    _Table: TKeyword;
    _OpenBracket: TTerminal;
    _TableName: TIdent;
    _CloseBracket: TTerminal;
    _Alias: TIdent;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function StatementName: string; override;
  end;

{ TIdentField }

function TIdentField.InternalParse: boolean;
begin
  _FieldName := Identifier;
  Result := Assigned(_FieldName);
end;

function TIdentField.StatementName: string;
begin
  Result := _FieldName.Value;
end;

procedure TIdentField.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_FieldName);
end;

{ TIdentFields }

function TIdentFields.ParseBreak: boolean;
begin
  Result := not (NextToken is TIdent);
end;

{ TExpressionField }

function TExpressionField.InternalParse: boolean;
begin
  Result := TExpression.Parse(Self, Source, _Expression);
end;

procedure TExpressionField.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Expression);
  if Assigned(_Match) then
  begin
    APrinter.Ruler('match', Settings.AlignCommentInsert and Settings.CommentInsert);
    if Settings.CommentInsert then APrinter.PrintSpecialComment('=> ' + TIdentField(_Match).Name);
  end;
end;

{ TExpressionFields }

function TExpressionFields.ParseBreak: boolean;
begin
  Result := not (NextToken is TIdent);
end;

procedure TExpressionFields.Match(AFields: TIdentFields);
var i: integer;
begin
  if not Assigned(Self) or not Assigned(AFields) then exit;
  for i := 0 to Math.Min(Self.Count, AFields.Count) - 1 do
    if (Self.Item(i) is TExpressionField) and (AFields.Item(i) is TIdentField) then
      TExpressionField(Self.Item(i)).Match := TIdentField(AFields.Item(i));
end;

{ TWhere }

function TWhere.InternalParse: boolean;
begin
  _Where := Keyword('where');
  TExpression.Parse(Self, Source, _Condition);
  Result := Assigned(_Where);
end;

procedure TWhere.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Where);
  APrinter.PrintIndented(_Condition);
end;

{ TReturning }

function TReturning.InternalParse: boolean;
begin
  _Returning := Keyword('returning');
  if not Assigned(_Returning) then exit(false);
  TExpressionFields.Parse(Self, Source, _ReturningFields);
  _Into := Keyword('into');
  TIdentFields.Parse(Self, Source, _Targets);
  TExpressionFields(_ReturningFields).Match(TIdentFields(_Targets));
  Result := true;
end;

procedure TReturning.PrintSelf(APrinter: TPrinter);
begin
  APrinter.NextLine;
  APrinter.PrintItem(_Returning);
  APrinter.PrintIndented(_ReturningFields);
  APrinter.PrintItem(_Into);
  APrinter.PrintIndented(_Targets);
end;

{ TTableClause }

function TTableClause.InternalParse: boolean;
begin
  if not TInnerSelect.Parse(Self, Source, _Select) then
  begin
    _Table := Keyword('table');
    if Assigned(_Table) then _OpenBracket := Terminal('(');
    _TableName := Identifier;
    if not Assigned(_Table) and not Assigned(_TableName) then exit(false);
    if Assigned(_Table) then _CloseBracket := Terminal(')');
  end;
  _Alias := Identifier;
  Result := true;
end;

function TTableClause.StatementName: string;
begin
  if Assigned(_TableName) then
    Result := _TableName.Value
  else if Assigned(_Select) then
    Result := 'inner select'
  else
    Result := 'table';
  if Assigned(_Alias) then Result := _Alias.Value + ' => ' + Result;
end;

procedure TTableClause.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Select);
  APrinter.PrintItem(_Table);
  APrinter.PrintItem(_OpenBracket);
  APrinter.PrintItem(_TableName);
  APrinter.PrintItem(_CloseBracket);
  APrinter.Space;
  APrinter.PrintItem(_Alias);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                               Оператор SELECT                              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

type

  { Поле в select }
  TSelectField = class(TExpressionField)
  strict private
    _As: TKeyword;
    _Alias: TIdent;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function StatementName: string; override;
  end;

  { Список полей в select }
  TSelectFields = class(TExpressionFields)
  strict protected
    function ParseStatement(out AResult: TStatement): boolean; override;
  end;

  { Указание таблицы в select }
  TSelectTableClause = class(TTableClause)
  strict private
    _On: TKeyword;
    _JoinCondition: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Список таблиц в select }
  TSelectTables = class(TCommaList<TSelectTableClause>)
  strict protected
    function ParseDelimiter(out AResult: TToken): boolean; override;
    function ParseBreak: boolean; override;
  end;

  { Выражение group by }
  TGroupBy = class(TCommaList<TExpressionField>)
  strict private
    _GroupBy: TKeyword;
  strict protected
    function InternalParse: boolean; override;
    function ParseBreak: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Строка выражения order by }
  TOrderByItem = class(TStatement)
  strict private
    _Expression: TStatement;
    _Nulls: TKeyword;
    _Position: TKeyword;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Выражение order by }
  TOrderBy = class(TCommaList<TOrderByItem>)
  strict private
    _OrderBy: TKeyword;
  strict protected
    function InternalParse: boolean; override;
    function ParseBreak: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

{ TSelect }

function TSelect.InternalParse: boolean;
begin
  _Select := Keyword('select');
  if not Assigned(_Select) then exit(false);
  _Star := Terminal('*');
  if not Assigned(_Star) then TSelectFields.Parse(Self, Source, _Fields);
  _Into := Keyword('into');
  TIdentFields.Parse(Self, Source, _IntoFields);
  _From := Keyword('from');
  TSelectTables.Parse(Self, Source, _Tables);
  TWhere.Parse(Self, Source, _Where);
  TGroupBy.Parse(Self, Source, _GroupBy);
  TOrderBy.Parse(Self, Source, _OrderBy);
  Result := true;
  TExpressionFields(_Fields).Match(TIdentFields(_IntoFields));
end;

procedure TSelect.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Select);
  APrinter.PrintIndented(_Star);
  APrinter.PrintIndented(_Fields);
  APrinter.PrintItem(_Into);
  APrinter.PrintIndented(_IntoFields);
  APrinter.PrintItem(_From);
  APrinter.PrintIndented(_Tables);
  APrinter.PrintItem(_Where);
  APrinter.PrintItem(_GroupBy);
  APrinter.PrintItem(_OrderBy);
end;

procedure TSelect.Match(AFields: TStatement);
begin
  if (_Fields is TExpressionFields) and (AFields is TIdentFields) then
    TExpressionFields(_Fields).Match(AFields as TIdentFields);
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
  if Assigned(_Alias)
    then Result := '< ' + _Alias.Value + ' >'
    else Result := '< field >';
end;

procedure TSelectField.PrintSelf(APrinter: TPrinter);
begin
  inherited;
  APrinter.Space;
  APrinter.PrintItem(_As);
  APrinter.Space;
  APrinter.PrintItem(_Alias);
  APrinter.SupressSpace;
end;

{ TSelectFields }

function TSelectFields.ParseStatement(out AResult: TStatement): boolean;
begin
  Result := TSelectField.Parse(Self, Source, AResult);
end;

{ TSelectTableClause }

function TSelectTableClause.InternalParse: boolean;
begin
  Result := inherited InternalParse;
  if Result then _On := Keyword('on');
  if Assigned(_On) then TExpression.Parse(Self, Source, _JoinCondition);
end;

procedure TSelectTableClause.PrintSelf(APrinter: TPrinter);
begin
  inherited;
  if Assigned(_On) then
  begin
    APrinter.PrintIndented(_On);
    APrinter.Space;
    APrinter.PrintItem(_JoinCondition);
  end;
end;

{ TSelectTables }

function TSelectTables.ParseDelimiter(out AResult: TToken): boolean;
begin
  Result := inherited ParseDelimiter(AResult);
  if not Result then
  begin
    AResult := Keyword('full join');
    Result  := Assigned(AResult);
  end;
end;

function TSelectTables.ParseBreak: boolean;
begin
  Result := Any([Keyword(['where', 'having', 'group by', 'order by']), Terminal([')', ';'])]);
end;

{ TGroupBy }

function TGroupBy.InternalParse: boolean;
begin
  _GroupBy := Keyword('group by');
  if not Assigned(_GroupBy) then exit(false);
  Result := inherited InternalParse;
end;

function TGroupBy.ParseBreak: boolean;
begin
  Result := Any([Terminal([';', ')'])]) or (Source.Next is TKeyword);
end;

procedure TGroupBy.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_GroupBy);
  APrinter.NextLine;
  APrinter.Indent;
  inherited PrintSelf(APrinter);
  APrinter.NextLine;
  APrinter.Undent;
end;

{ TOrderByItem }

function TOrderByItem.InternalParse: boolean;
begin
  if not TExpression.Parse(Self, Source, _Expression) then exit(false);
  _Nulls := Keyword('nulls');
  _Position := Keyword(['first', 'last']);
  Result := true;
end;

procedure TOrderByItem.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Expression);
  APrinter.Space;
  APrinter.PrintItem(_Nulls);
  APrinter.Space;
  APrinter.PrintItem(_Position);
end;

{ TOrderBy }

function TOrderBy.InternalParse: boolean;
begin
  _OrderBy := Keyword('order by');
  if not Assigned(_OrderBy) then exit(false);
  Result := inherited InternalParse;
end;

function TOrderBy.ParseBreak: boolean;
begin
  Result := Any([Terminal([';', ')'])]) or (Source.Next is TKeyword);
end;

procedure TOrderBy.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_OrderBy);
  APrinter.NextLine;
  APrinter.Indent;
  inherited;
  APrinter.Undent;
end;

{ TInsert }

function TInsert.InternalParse: boolean;
begin
  Result := true;
  _Insert := Keyword('insert');
  if not Assigned(_Insert) then exit(false);
  _Into := Keyword('into');
  _TableName := Identifier;
  _OpenBracket := Terminal('(');
  TIdentFields.Parse(Self, Source, _Fields);
  _CloseBracket := Terminal(')');
  if TSelect.Parse(Self, Source, _Select) then
    (_Select as TSelect).Match(_Fields as TIdentFields)
  else
    begin
      _Values := Keyword('values');
      _OpenBracket2 := Terminal('(');
      TExpressionFields.Parse(Self, Source, _ValueList);
      _CloseBracket2 := Terminal(')');
      if Assigned(_ValueList) and Assigned(_Fields) then
        (_ValueList as TExpressionFields).Match(_Fields as TIdentFields);
    end;
end;

procedure TInsert.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Insert);
  APrinter.NextLine;
  APrinter.PrintItem(_Into);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_TableName);
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
end;

{ TUpdate }

function TUpdate.InternalParse: boolean;
begin
  _Update := Keyword('update');
  if not Assigned(_Update) then exit(false);
  _TableName := Identifier;
  _Alias := Identifier;
  _Set := Keyword('set');
  TUpdateAssignments.Parse(Self, Source, _Assignments);
  TWhere.Parse(Self, Source, _Where);
  TReturning.Parse(Self, Source, _Returning);
  Result := true;
end;

procedure TUpdate.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Update);
  APrinter.PrintIndented(_TableName);
  APrinter.Space;
  APrinter.PrintItem(_Alias);
  APrinter.NextLine;
  APrinter.PrintItem(_Set);
  APrinter.PrintIndented(_Assignments);
  APrinter.PrintItem(_Where);
  APrinter.PrintItem(_Returning);
end;

{ TDelete }

function TDelete.InternalParse: boolean;
begin
  _Delete := Keyword('delete');
  _From   := Keyword('from');
  if not Assigned(_Delete) or not Assigned(_From) then exit(false);
  TTableClause.Parse(Self, Source, _Table);
  TWhere.Parse(Self, Source, _Where);
  Result := true;
end;

procedure TDelete.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Delete);
  APrinter.NextLine;
  APrinter.PrintItem(_From);
  APrinter.PrintIndented(_Table);
  APrinter.PrintItem(_Where);
end;

{ TMerge }

function TMerge.InternalParse: boolean;
begin
  Result := true;
  _Merge := Keyword('merge');
  _Into  := Keyword('into');
  if not Assigned(_Merge) or not Assigned(_Into) then exit(false);
  if not TInnerSelect.Parse(Self, Source, _DestSelect) then _DestTable := Identifier;
  _DestAlias := Identifier;
  _Using := Keyword('using');
  if not TInnerSelect.Parse(Self, Source, _SourceSelect) then _SourceTable := Identifier;
  _SourceAlias := Identifier;
  _On    := Keyword('on');
  TExpression.Parse(Self, Source, _Condition);
  TMergeSections.Parse(Self, Source, _Sections);
end;

procedure TMerge.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Merge);
  APrinter.NextLine;
  APrinter.PrintItem(_Into);
  APrinter.NextLine;
  APrinter.PrintIndented(_DestSelect);
  APrinter.PrintIndented(_DestTable);
  APrinter.Space;
  APrinter.PrintItem(_DestAlias);
  APrinter.NextLine;
  APrinter.PrintItem(_Using);
  APrinter.PrintIndented(_SourceSelect);
  APrinter.PrintIndented(_SourceTable);
  APrinter.Space;
  APrinter.PrintItem(_SourceAlias);
  APrinter.NextLine;
  APrinter.PrintItem(_On);
  APrinter.PrintIndented(_Condition);
  APrinter.NextLine;
  APrinter.PrintItem(_Sections);
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

{ TUpdateAssignment }

function TUpdateAssignment.InternalParse: boolean;
begin
  _Target := Identifier;
  _Assignment := Terminal('=');
  Result := Assigned(_Target) and Assigned(_Assignment);
  if Result then TExpression.Parse(Self, Source, _Value);
end;

function TUpdateAssignment.StatementName: string;
begin
  if Assigned(_Target)
    then Result := _Target.Value
    else Result := '< update assignment >';
end;

procedure TUpdateAssignment.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Target);
  APrinter.Space;
  APrinter.Ruler('assignment');
  APrinter.PrintItem(_Assignment);
  APrinter.Space;
  APrinter.PrintItem(_Value);
end;

{ TUpdateAssignments }

function TUpdateAssignments.ParseBreak: boolean;
begin
  Result := Any([Terminal(';')]) or (Source.Next is TKeyword);
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
  Result := _Section.Value;
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

end.
