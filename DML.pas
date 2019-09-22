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

{ ----- Примечание -------------------------------------------------------------

  В настоящей версии dml-операторы реализованы как список произвольных лексем,
  начинающихся с select/insert/update/delete/merge и заканчивающихся точкой с
  запятой. Это позволяет обработать их, встречая в PL/SQL коде и отложить их
  детальную спецификацию на будущее

------------------------------------------------------------------------------ }

interface

uses Math, Tokens, Statements, PLSQL, Printers_, Attributes;

type

  { Оператор select }
  TSelect = class(TOperator)
  strict private
    _Select: TKeyword;
    _Fields: TStatement;
    _Into: TKeyword;
    _IntoFields: TStatement;
    _From: TKeyword;
    _Tables: TStatement;
    _Where: TKeyword;
    _Condition: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор insert }
  TInsert = class(TOperator)
  strict private
    _Insert: TKeyword;
    _Into: TKeyword;
    _TableName: TIdent;
    _OpenBracket: TTerminal;
    _Fields: TStatement;
    _CloseBracket: TTerminal;
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
  TUpdate = class(TOperator)
  strict private
    _Update: TKeyword;
    _TableName: TIdent;
    _Alias: TIdent;
    _Set: TKeyword;
    _Assignments: TStatement;
    _Where: TKeyword;
    _Condition: TStatement;
    _Returning: TKeyword;
    _ReturningFields: TStatement;
    _Into: TKeyword;
    _Targets: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор delete }
  TDelete = class(TOperator)
  strict private
    _Delete: TKeyword;
    _From: TKeyword;
    _TableName: TIdent;
    _Where: TKeyword;
    _Condition: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор merge }
  TMerge = class(TOperator)
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
    function Name: string; override;
  end;

  { Поле в select }
  TSelectField = class(TStatement)
  strict private
    _Expression: TStatement;
    _As: TKeyword;
    _Alias: TIdent;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function Name: string; override;
  end;

  { Список полей в select }
  TSelectFields = class(TCommaList<TSelectField>)
  strict protected
    function ParseBreak: boolean; override;
  public
    function Name: string; override;
  end;

  { Идентификатор как поле в insert, select into итп }
  TIdentField = class(TStatement)
  strict private
    _FieldName: TIdent;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function Name: string; override;
    function FieldName: string;
  end;

  { Список идентификаторов }
  TIdentFields = class(TCommaList<TIdentField>)
  strict protected
    function ParseBreak: boolean; override;
  public
    function Name: string; override;
  end;

  { Значение в insert }
  TInsertValue = class(TStatement)
  strict private
    _Value: TStatement;
    _Match: TIdentField;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function Name: string; override;
    property Match: TIdentField read _Match write _Match;
  end;

  { Значения в insert }
  [Aligned]
  TInsertValues = class(TCommaList<TInsertValue>)
  strict protected
    function ParseBreak: boolean; override;
  public
    function Name: string; override;
    procedure Match(AFields: TIdentFields);
  end;

  { Таблица в select }
  TSelectTable = class(TStatement)
  strict private
    _Table: TKeyword;
    _OpenBracket: TTerminal;
    _TableName: TIdent;
    _CloseBracket: TTerminal;
    _Alias: TIdent;
  strict protected
    function InternalParse: boolean; override;
  public
    function Name: string; override;
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Список таблиц в select }
  TSelectTables = class(TCommaList<TSelectTable>)
  strict protected
    function ParseBreak: boolean; override;
  public
    function Name: string; override;
  end;

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
    function Name: string; override;
  end;

  { Список присвоений в update }
  [Aligned]
  TUpdateAssignments = class(TCommaList<TUpdateAssignment>)
  strict protected
    function ParseBreak: boolean; override;
  public
    function Name: string; override;
  end;

  { Секция в merge }
  TMergeSection = class(TStatement)
  strict private
    _Section: TKeyword;
    _DML: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    function Name: string; override;
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Секции операторов в merge }
  TMergeSections = class(TStatementList<TMergeSection>)
  strict protected
    function ParseBreak: boolean; override;
  public
    function Name: string; override;
  end;

implementation

uses Expressions;

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
  _Values := Keyword('values');
  _OpenBracket2 := Terminal('(');
  TInsertValues.Parse(Self, Source, _ValueList);
  _CloseBracket2 := Terminal(')');
  if (_ValueList is TInsertValues) and (_Fields is TIdentFields) then TInsertValues(_ValueList).Match(TIdentFields(_Fields));
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
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_Fields);
  APrinter.Undent;
  APrinter.NextLine;
  APrinter.PrintItem(_CloseBracket);
  APrinter.Undent;
  APrinter.NextLine;
  APrinter.PrintItem(_Values);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_OpenBracket2);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_ValueList);
  APrinter.NextLine;
  APrinter.Undent;
  APrinter.PrintItem(_CloseBracket2);
  APrinter.Undent;
end;

{ TUpdate }

function TUpdate.InternalParse: boolean;
begin
  Result := true;
  _Update := Keyword('update');
  if not Assigned(_Update) then exit(false);
  _TableName := Identifier;
  _Alias := Identifier;
  _Set := Keyword('set');
  TUpdateAssignments.Parse(Self, Source, _Assignments);
  _Where := Keyword('where');
  if Assigned(_Where) then TExpression.Parse(Self, Source, _Condition);
  _Returning := Keyword('returning');
  if Assigned(_Returning) then TIdentFields.Parse(Self, Source, _ReturningFields);
  _Into := Keyword('into');
  if Assigned(_Into) then TIdentFields.Parse(Self, Source, _Targets);
end;

procedure TUpdate.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Update);
  APrinter.Space;
  APrinter.PrintItem(_TableName);
  APrinter.Space;
  APrinter.PrintItem(_Alias);
  APrinter.Space;
  APrinter.PrintItem(_Set);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_Assignments);
  APrinter.Undent;
  APrinter.PrintItem(_Where);
  if Assigned(_Where) then APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_Condition);
  APrinter.NextLine;
  APrinter.Undent;
  APrinter.PrintItem(_Returning);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_ReturningFields);
  APrinter.NextLine;
  APrinter.Undent;
  APrinter.PrintItem(_Into);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_Targets);
  APrinter.NextLine;
  APrinter.Undent;
end;

{ TDelete }

function TDelete.InternalParse: boolean;
begin
  _Delete := Keyword('delete');
  _From   := Keyword('from');
  if not Assigned(_Delete) or not Assigned(_From) then exit(false);
  _TableName := Identifier;
  _Where  := Keyword('where');
  Result := true;
end;

procedure TDelete.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Delete);
  APrinter.NextLine;
  APrinter.PrintItem(_From);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_TableName);
  APrinter.NextLine;
  APrinter.Undent;
  APrinter.PrintItem(_Where);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_Condition);
  APrinter.Undent;
end;

{ TIdentFields }

function TIdentFields.Name: string;
begin
  Result := '< identifiers >';
end;

function TIdentFields.ParseBreak: boolean;
begin
  Result := not (NextToken is TIdent);
end;

{ TInsertValue }

function TInsertValue.InternalParse: boolean;
begin
  Result := TExpression.Parse(Self, Source, _Value);
end;

function TInsertValue.Name: string;
begin
  Result := '< insert value >';
end;

procedure TInsertValue.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Value);
  if Assigned(_Match) then
  begin
    APrinter.Ruler('match', Settings.AlignCommentInsert and Settings.CommentInsert);
    if Settings.CommentInsert then APrinter.PrintSpecialComment('=> ' + TIdentField(_Match).FieldName);
  end;
end;

{ TInsertValues }

function TInsertValues.Name: string;
begin
  Name := '< insert values >';
end;

function TInsertValues.ParseBreak: boolean;
begin
  Result := Any([Terminal([')', ';'])]) or (NextToken is TKeyword);
end;

procedure TInsertValues.Match(AFields: TIdentFields);
var i: integer;
begin
  for i := 0 to Math.Min(Self.Count, AFields.Count) - 1 do
    if (Item(i) is TInsertValue) and (AFields.Item(i) is TIdentField) then
      TInsertValue(Item(i)).Match := TIdentField(AFields.Item(i));
end;

{ TSelect }

function TSelect.InternalParse: boolean;
begin
  _Select := Keyword('select');
  if not Assigned(_Select) then exit(false);
  TSelectFields.Parse(Self, Source, _Fields);
  _Into := Keyword('into');
  TIdentFields.Parse(Self, Source, _IntoFields);
  _From := Keyword('from');
  TSelectTables.Parse(Self, Source, _Tables);
  _Where := Keyword('where');
  TExpression.Parse(Self, Source, _Condition);
  Result := true;
end;

procedure TSelect.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Select);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_Fields);
  APrinter.NextLine;
  APrinter.Undent;
  APrinter.PrintItem(_Into);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_IntoFields);
  APrinter.NextLine;
  APrinter.Undent;
  APrinter.PrintItem(_From);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_Tables);
  APrinter.NextLine;
  APrinter.Undent;
  APrinter.PrintItem(_Where);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_Condition);
  APrinter.NextLine;
  APrinter.Undent;
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
  APrinter.Space;
  APrinter.PrintItem(_Into);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_DestSelect);
  APrinter.PrintItem(_DestTable);
  APrinter.Space;
  APrinter.PrintItem(_DestAlias);
  APrinter.NextLine;
  APrinter.Undent;
  APrinter.PrintItem(_Using);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_SourceSelect);
  APrinter.PrintItem(_SourceTable);
  APrinter.Space;
  APrinter.PrintItem(_SourceAlias);
  APrinter.NextLine;
  APrinter.Undent;
  APrinter.PrintItem(_On);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_Condition);
  APrinter.NextLine;
  APrinter.Undent;
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

function TInnerSelect.Name: string;
begin
  Result := '< inner select >';
end;

procedure TInnerSelect.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_OpenBracket);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_Select);
  APrinter.Undent;
  APrinter.NextLine;
  APrinter.PrintItem(_CloseBracket);
end;

{ TSelectTable }

function TSelectTable.InternalParse: boolean;
begin
  _Table := Keyword('table');
  if Assigned(_Table) then _OpenBracket := Terminal('(');
  _TableName := Identifier;
  if Assigned(_Table) then _CloseBracket := Terminal(')');
  _Alias := Identifier;
  Result := Assigned(_Table) or Assigned(_TableName);
end;

function TSelectTable.Name: string;
begin
  if Assigned(_TableName)
    then Result := _TableName.Value
    else Result := 'table';
  if Assigned(_Alias) then Result := _Alias.Value + ' => ' + Result;
  Result := '< ' + Result + ' >';
end;

procedure TSelectTable.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Table);
  APrinter.PrintItem(_OpenBracket);
  APrinter.PrintItem(_TableName);
  APrinter.PrintItem(_CloseBracket);
  APrinter.Space;
  APrinter.PrintItem(_Alias);
end;

{ TSelectTables }

function TSelectTables.ParseBreak: boolean;
begin
  Result := Any([Keyword(['where', 'having', 'group by', 'order by']), Terminal(')')]);
end;

function TSelectTables.Name: string;
begin
  Result := '< source tables >';
end;

{ TSelectField }

function TSelectField.InternalParse: boolean;
begin
  Result := TExpression.Parse(Self, Source, _Expression);
  if not Result then exit;
  _As := Keyword('as');
  _Alias := Identifier;
end;

function TSelectField.Name: string;
begin
  if Assigned(_Alias)
    then Result := _Alias.Value
    else Result := '< field >';
end;

procedure TSelectField.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Expression);
  APrinter.Space;
  APrinter.PrintItem(_As);
  APrinter.Space;
  APrinter.PrintItem(_Alias);
  APrinter.SupressSpace;
end;

{ TSelectFields }

function TSelectFields.ParseBreak: boolean;
begin
  Result := Any([Terminal([';'])]) or (Source.Next is TKeyword);
end;

function TSelectFields.Name: string;
begin
  Result := '< field list >';
end;

{ TIdentField }

function TIdentField.InternalParse: boolean;
begin
  _FieldName := Identifier;
  Result := Assigned(_FieldName);
end;

function TIdentField.Name: string;
begin
  Result := '< ' + FieldName + ' >';
end;

function TIdentField.FieldName: string;
begin
  Result := _FieldName.Value;
end;

procedure TIdentField.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_FieldName);
end;

{ TUpdateAssignment }

function TUpdateAssignment.InternalParse: boolean;
begin
  _Target := Identifier;
  _Assignment := Terminal('=');
  Result := Assigned(_Target) and Assigned(_Assignment);
  if Result then TExpression.Parse(Self, Source, _Value);
end;

function TUpdateAssignment.Name: string;
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

function TUpdateAssignments.Name: string;
begin
  Result := '< update assignment >';
end;

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

function TMergeSection.Name: string;
begin
  Result := _Section.Value;
end;

procedure TMergeSection.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Section);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_DML);
  APrinter.NextLine;
  APrinter.Undent;
end;

{ TMergeSections }

function TMergeSections.Name: string;
begin
  Result := '< merge sections >';
end;

function TMergeSections.ParseBreak: boolean;
begin
  Result := not Assigned(Keyword(['when matched then', 'when not matched then']));
end;

end.
