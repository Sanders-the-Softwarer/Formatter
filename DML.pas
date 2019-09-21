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

uses Math, Tokens, Statements, PLSQL, Printers_;

type

  { Оператор select }
  TSelect = class(TOperator)
  strict private
    _Select: TKeyword;
    _Fields: TStatement;
    _Into: TKeyword;
    _Targets: TStatement;
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
    _Expressions: TStatement;
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
    _TableName: TIdent;
    _DestAlias: TIdent;
    _Using: TKeyword;
    _Source: TStatement;
    _SourceAlias: TIdent;
    _On: TKeyword;
    _Condition: TStatement;
    _WhenMatchedThen: TKeyword;
    _Update: TStatement;
    _WhenNotMatchedThen: TKeyword;
    _Insert: TStatement;
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

  { Список полей в select }
  TSelectFields = class(TCommaList)
  strict protected
    function ParseStatement(out AResult: TStatement): boolean; override;
    function ParseBreak: boolean; override;
  public
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

  { Таблица в select }
  TSelectTable = class(TStatement)
  strict private
    _TableName: TIdent;
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

  { Список присвоений в update }
  TUpdateAssignments = class(TCommaList)
  strict protected
    function ParseStatement(out AResult: TStatement): boolean; override;
    function ParseBreak: boolean; override;
  public
    function Name: string; override;
    function Aligned: boolean; override;
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

  { Список выражений через запятую }
  TFieldList = class(TStatementList)
  strict protected
    function ParseStatement(out  AResult: TStatement): boolean; override;
    function ParseBreak: boolean; override;
  public
    function Name: string; override;
    function Aligned: boolean; override;
  end;

  { Выражение для списка }
  TField = class(TStatement)
  strict private
    _Expression: TStatement;
    _As: TKeyword;
    _Alias: TIdent;
    _Comma: TTerminal;
    _Match: TField;
  strict protected
    function InternalParse: boolean; override;
  public
    property Match: TField read _Match write _Match;
    function Name: string; override;
    function Ident: string;
    procedure PrintSelf(APrinter: TPrinter); override;
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
  TFieldList.Parse(Self, Source, _Fields);
  _CloseBracket := Terminal(')');
  _Values := Keyword('values');
  _OpenBracket2 := Terminal('(');
  TFieldList.Parse(Self, Source, _Expressions);
  _CloseBracket2 := Terminal(')');
  inherited;
end;

procedure TInsert.PrintSelf(APrinter: TPrinter);
var i: integer;
begin
  if Settings.CommentInsert and Assigned(_Expressions) and Assigned(_Fields) then
    for i := 0 to TStatementList(_Fields).Count - 1 do
      if TStatementList(_Expressions).Count > i then
        (TStatementList(_Expressions).Item(i) as TField).Match := TStatementList(_Fields).Item(i) as TField;
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
  APrinter.PrintItem(_Expressions);
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
  APrinter.Undent;
end;

{ TDelete }

function TDelete.InternalParse: boolean;
begin
  _Delete := Keyword('delete');
  _From   := Keyword('from');
  _TableName := Identifier;
  _Where  := Keyword('where');
  TExpression.Parse(Self, Source, _Condition);
  inherited;
  Result := Assigned(_Delete) and Assigned(_From);
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

{ TFieldList }

function TFieldList.ParseStatement(out AResult: TStatement): boolean;
begin
  Result := TField.Parse(Self, Source, AResult);
end;

function TFieldList.Name: string;
begin
  Result := '< fields >';
end;

function TFieldList.Aligned: boolean;
begin
  Aligned := true;
end;

function TFieldList.ParseBreak: boolean;
begin
  Result := Any([Terminal([')', ';']), Keyword(['from'])]);
end;

{ TField }

function TField.InternalParse: boolean;
begin
  Result := true;
  if not TExpression.Parse(Self, Source, _Expression) then exit(false);
  _As := Keyword('as');
  _Alias := Identifier;
  _Comma := Terminal(',');
end;

function TField.Name: string;
begin
  if Assigned(_Alias)
    then Result := _Alias.Value
    else Result := '< field >';
end;

function TField.Ident: string;
begin
  if _Expression is TExpression
    then Result := TExpression(_Expression).Ident
    else Result := '';
end;

procedure TField.PrintSelf(APrinter: TPrinter);
var C: string;
begin
  APrinter.PrintItem(_Expression);
  APrinter.Ruler('expression');
  APrinter.Space;
  APrinter.PrintItem(_As);
  APrinter.Space;
  APrinter.PrintItem(_Alias);
  APrinter.SupressSpace;
  APrinter.PrintItem(_Comma);
  APrinter.Space;
  if _Match is TField then
  begin
    APrinter.Ruler('comment', Settings.AlignCommentInsert);
    C := TField(_Match).Ident;
    if C <> '' then APrinter.PrintSpecialComment('=> ' + C);
  end;
  APrinter.NextLine;
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

function TSelectFields.ParseStatement(out AResult: TStatement): boolean;
begin
  Result := TSelectField.Parse(Self, Source, AResult);
end;

function TSelectFields.ParseBreak: boolean;
begin
  Result := Any([Keyword(['from', 'into', 'where']), Terminal([';'])]);
end;

function TSelectFields.Name: string;
begin
  Result := '< field list >';
end;

{ TSelect }

function TSelect.InternalParse: boolean;
begin
  _Select := Keyword('select');
  if not Assigned(_Select) then exit(false);
  TSelectFields.Parse(Self, Source, _Fields);
  _Into := Keyword('into');
  if Assigned(_Into) then TSelectFields.Parse(Self, Source, _Targets);
  _From := Keyword('from');
  TSelectTables.Parse(Self, Source, _Tables);
  _Where := Keyword('where');
  TExpression.Parse(Self, Source, _Condition);
  Result := true;
end;

procedure TSelect.PrintSelf(APrinter: TPrinter);
begin
  { select }
  APrinter.PrintItem(_Select);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_Fields);
  APrinter.Undent;
  { into }
  if Assigned(_Into) then
  begin
    APrinter.PrintItem(_Into);
    APrinter.NextLine;
    APrinter.Indent;
    APrinter.PrintItem(_Targets);
    APrinter.Undent;
  end;
  { from }
  if Assigned(_From) then
  begin
    APrinter.PrintItem(_From);
    APrinter.NextLine;
    APrinter.Indent;
    APrinter.PrintItem(_Tables);
    APrinter.Undent;
  end;
  { where }
  if Assigned(_Where) then
  begin
    APrinter.PrintItem(_Where);
    APrinter.NextLine;
    APrinter.Indent;
    APrinter.PrintItem(_Condition);
    APrinter.Undent;
  end;
  APrinter.SupressNextLine;
end;

{ TUpdateAssignments }

function TUpdateAssignments.ParseStatement(out AResult: TStatement): boolean;
begin
  Result := TUpdateAssignment.Parse(Self, Source, AResult);
end;

function TUpdateAssignments.Name: string;
begin
  Result := '< update assignment >';
end;

function TUpdateAssignments.Aligned: boolean;
begin
  Result := true;
end;

function TUpdateAssignments.ParseBreak: boolean;
begin
  Result := Any([Terminal(';'), Keyword('where')]);
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

{ TMerge }

function TMerge.InternalParse: boolean;
begin
  Result := true;
  _Merge := Keyword('merge');
  _Into  := Keyword('into');
  if not Assigned(_Merge) or not Assigned(_Into) then exit(false);
  _TableName := Identifier;
  _DestAlias := Identifier;
  _Using := Keyword('using');
  TInnerSelect.Parse(Self, Source, _Source);
  _SourceAlias := Identifier;
  _On    := Keyword('on');
  TExpression.Parse(Self, Source, _Condition);
  _WhenMatchedThen := Keyword('when matched then');
  if Assigned(_WhenMatchedThen) then TUpdate.Parse(Self, Source, _Update);
  _WhenNotMatchedThen := Keyword('when not matched then');
  if Assigned(_WhenNotMatchedThen) then TInsert.Parse(Self, Source, _Update);
end;

procedure TMerge.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Merge);
  APrinter.Space;
  APrinter.PrintItem(_Into);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_TableName);
  APrinter.Space;
  APrinter.PrintItem(_DestAlias);
  APrinter.NextLine;
  APrinter.Undent;
  APrinter.PrintItem(_Using);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_Source);
  APrinter.Space;
  APrinter.PrintItem(_SourceAlias);
  APrinter.NextLine;
  APrinter.Undent;
  APrinter.PrintItem(_On);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_Condition);
end;

{ TInnerSelect }

function TInnerSelect.InternalParse: boolean;
begin
  _OpenBracket := Terminal('(');
  TSelect.Parse(Self, Source, _Select);
  _CloseBracket := Terminal(')');
  Result := Assigned(_OpenBracket) and Assigned(_Select) and Assigned(_CloseBracket);
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
  _TableName := Identifier;
  _Alias := Identifier;
  Result := Assigned(_TableName);
end;

function TSelectTable.Name: string;
begin
  Result := _TableName.Value;
  if Assigned(_Alias) then Result := _Alias.Value + ' => ' + Result;
end;

procedure TSelectTable.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_TableName);
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

end.
