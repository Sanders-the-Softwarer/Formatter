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

uses Math, Tokens, Statements, Printers_, Attributes;

type

  { Оператор select }
  TSelect = class(TSemicolonStatement)
  strict private
    _With: TStatement;
    _Select: TKeyword;
    _Mode: TKeyword;
    _Fields: TStatement;
    _Into: TKeyword;
    _IntoFields: TStatement;
    _From: TKeyword;
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
  TInsert = class(TSemicolonStatement)
  strict private
    _Insert: TKeyword;
    _Into: TKeyword;
    _Table: TStatement;
    _OpenBracket: TTerminal;
    _Fields: TStatement;
    _CloseBracket: TTerminal;
    _Select: TStatement;
    _Values: TKeyword;
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
  TUpdate = class(TSemicolonStatement)
  strict private
    _Update: TKeyword;
    _Table: TStatement;
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
  TDelete = class(TSemicolonStatement)
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
  TMerge = class(TSemicolonStatement)
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

  TSLIdentFields = class(TIdentFields)
  public
    function MultiLine: boolean; override;
  end;

implementation

uses Expressions;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                         Общие элементы конструкций                         //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

type

  { Выражение как поле в group by, insert values и т. п. }
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
  TTableRef = class(TStatement)
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

{ TSLIdentFields }

function TSLIdentFields.MultiLine: boolean;
begin
  Result := false;
end;

{ TExpressionField }

function TExpressionField.InternalParse: boolean;
begin
  Result := TExpression.Parse(Self, Source, _Expression);
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

function TExpressionFields.ParseBreak: boolean;
begin
  Result := not (NextToken is TIdent);
end;

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
  TExpression.Parse(Self, Source, _Condition);
  Result := Assigned(_Where);
end;

procedure TWhere.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Where);
  APrinter.PrintIndented(_Condition);
  APrinter.NextLine;
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
    _TableName := Identifier;
    if not Assigned(_Table) and not Assigned(_TableName) then exit(false);
    if Assigned(_Table) then _CloseBracket := Terminal(')');
  end;
  _Alias := Identifier;
  Result := true;
end;

function TTableRef.StatementName: string;
begin
  if Assigned(_TableName) then
    Result := _TableName.Value
  else if Assigned(_Select) then
    Result := _Select.StatementName
  else
    Result := 'table';
  if Assigned(_Alias) then Result := _Alias.Value + ' => ' + Result;
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
    _Alias: TIdent;
    _As: TKeyword;
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
    _With: TKeyword;
  strict protected
    function InternalParse: boolean; override;
    function ParseBreak: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Поле в select }
  TSelectField = class(TExpressionField)
  strict private
    _As: TKeyword;
    _Alias: TIdent;
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
  TSelectTableClause = class(TTableRef)
  strict private
    _On: TKeyword;
    _JoinCondition: TStatement;
    _Using: TKeyword;
    _OpenBracket: TTerminal;
    _Fields: TStatement;
    _CloseBracket: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Список таблиц в select }
  TSelectTables = class(TCommaList<TSelectTableClause>)
  strict protected
    function ParseDelimiter(out AResult: TObject): boolean; override;
    procedure PrintDelimiter(APrinter: TPrinter; ADelimiter: TObject); override;
    function ParseBreak: boolean; override;
  end;

  { Выражение group by }
  TGroupBy = class(TCommaList<TExpressionField>)
  strict private
    _Group: TKeyword;
    _By: TKeyword;
  strict protected
    function InternalParse: boolean; override;
    function ParseBreak: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Выражение having }
  THaving = class(TStatement)
  strict private
    _Having: TKeyword;
    _Condition: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Выражение start with }
  TStartWith = class(TStatement)
  strict private
    _Start, _With: TKeyword;
    _Condition: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

    { Выражение start with }
  TConnectBy = class(TStatement)
  strict private
    _Connect, _By: TKeyword;
    _Condition: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Строка выражения order by }
  TOrderByItem = class(TStatement)
  strict private
    _Expression: TStatement;
    _Nulls: TKeyword;
    _Position: TKeyword;
    _Direction: TKeyword;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Выражение order by }
  TOrderBy = class(TCommaList<TOrderByItem>)
  strict private
    _Order: TKeyword;
    _By: TKeyword;
  strict protected
    function InternalParse: boolean; override;
    function ParseBreak: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Следующий запрос в цепочке union all }
  TAdditionalSelect = class(TSelect)
  strict private
    _SetOperation: TKeyword;
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
  if not Assigned(_Select) then exit(false);
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
  APrinter.PrintItems([_With, _Select, _Mode]);
  APrinter.PrintIndented(_Fields);
  APrinter.PrintItem(_Into);
  APrinter.PrintIndented(_IntoFields);
  APrinter.PrintItem(_From);
  APrinter.PrintIndented(_Tables);
  APrinter.PrintItems([_Where, _StartWith, _ConnectBy, _GroupBy, _Having, _OrderBy, _AdditionalSelect]);
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
  Result := _Alias.Value;
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
  if Assigned(_Alias)
    then Result := _Alias.Value
    else Result := '';
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

{ TSelectTableClause }

function TSelectTableClause.InternalParse: boolean;
begin
  Result := inherited InternalParse;
  if Result then _On := Keyword('on');
  if Assigned(_On) then TExpression.Parse(Self, Source, _JoinCondition);
  if not Assigned(_On) then _Using := Keyword('using');
  if Assigned(_Using) then
  begin
    _OpenBracket := Terminal('(');
    TSLIdentFields.Parse(Self, Source, _Fields);
    _CloseBracket := Terminal(')');
  end;
end;

procedure TSelectTableClause.PrintSelf(APrinter: TPrinter);
begin
  inherited;
  APrinter.PrintIndented([_On, _JoinCondition, _Using, _OpenBracket, _Fields, _CloseBracket]);
end;

{ TSelectTables }

function TSelectTables.ParseDelimiter(out AResult: TObject): boolean;
begin
  Result := inherited ParseDelimiter(AResult);
  if not Result then
  begin
    AResult := Keyword(['join', 'inner join', 'full join', 'full outer join', 'left join', 'left outer join', 'right join', 'right outer join']);
    Result  := Assigned(AResult);
  end;
end;

procedure TSelectTables.PrintDelimiter(APrinter: TPrinter; ADelimiter: TObject);
begin
  if ADelimiter is TKeyword then
    begin
      APrinter.PrintIndented(ADelimiter);
      APrinter.NextLine;
    end
  else
    inherited;
end;

function TSelectTables.ParseBreak: boolean;
begin
  Result := Any([Terminal([')', ';'])]) or (NextToken is TKeyword);
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
  Result := Any([Terminal([';', ')'])]) or (Source.Next is TKeyword);
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
  TExpression.Parse(Self, Source, _Condition);
  Result := true;
end;

procedure THaving.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Having);
  APrinter.PrintIndented(_Condition);
  APrinter.NextLine;
end;


{ TStartWith }

function TStartWith.InternalParse: boolean;
begin
  _Start := Keyword('start');
  if not Assigned(_Start) then exit(false);
  _With  := Keyword('with');
  TExpression.Parse(Self, Source, _Condition);
  Result := true;
end;

procedure TStartWith.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Start, _With]);
  APrinter.PrintIndented(_Condition);
end;

{ TConnectBy }

function TConnectBy.InternalParse: boolean;
begin
  _Connect := Keyword('connect');
  if not Assigned(_Connect) then exit;
  _By := Keyword('by');
  TExpression.Parse(Self, Source, _Condition);
  Result := true;
end;

procedure TConnectBy.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Connect, _By]);
  APrinter.PrintIndented(_Condition);
end;

{ TOrderByItem }

function TOrderByItem.InternalParse: boolean;
begin
  if not TExpression.Parse(Self, Source, _Expression) then exit(false);
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
  _Order := Keyword('order');
  _By    := Keyword('by');
  if not Assigned(_Order) then exit(false);
  Result := inherited InternalParse;
end;

function TOrderBy.ParseBreak: boolean;
begin
  Result := Any([Terminal([';', ')'])]) or (Source.Next is TKeyword);
end;

procedure TOrderBy.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Order, _By]);
  APrinter.NextLine;
  APrinter.Indent;
  inherited;
  APrinter.Undent;
  APrinter.NextLine;
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
  _Target := Identifier;
  _Assignment := Terminal('=');
  Result := Assigned(_Target) and Assigned(_Assignment);
  if Result then TExpression.Parse(Self, Source, _Value);
end;

function TUpdateAssignment.StatementName: string;
begin
  if Assigned(_Target)
    then Result := _Target.Value
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
  Result := Any([Terminal(';')]) or (Source.Next is TKeyword);
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
  TExpression.Parse(Self, Source, _Condition);
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
