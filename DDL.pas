////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                       �������������� ����������� DDL                       //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit DDL;

interface

uses Tokens, Statements, Printers_, Attributes, Streams;

type

  { ������� create [or replace] }
  TCreate = class(TStatement)
  strict private
    _Create, _Or, _Replace, _Force: TEpithet;
    _What: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function StatementName: string; override;
  end;

  { ������� drop }
  TDrop = class(TSemicolonStatement)
  strict private
    _Drop, _ObjectType, _Body, _ObjectName: TEpithet;
    _Unexpected: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function StatementName: string; override;
  end;

  { ������ view }
  TView = class(TStatement)
  strict private
    _View: TEpithet;
    _ViewName: TEpithet;
    _As: TEpithet;
    _Select: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function StatementName: string; override;
  end;

  { ������ table }
  TTable = class(TSemicolonStatement)
  strict private
    _Global, _Temporary, _Table, _TableName: TEpithet;
    _Items: TStatement;
    _Tablespace: TStatement;
    _On, _Commit, _DeleteOrPreserve, _Rows: TEpithet;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function StatementName: string; override;
  end;

  { ����� ����� ��� ��������� ������ ������� }
  TTableItem = class(TStatement)
  public
    class function Candidates: TArray<TStatementClass>; override;
  end;

  { �������� ���� ������� }
  TTableField = class(TTableItem)
  strict private
    _Name: TEpithet;
    _Type: TStatement;
    _Not, _Null: TEpithet;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { �������� constraint-� }
  TConstraint = class(TTableItem)
  strict private
    _Constraint, _ConstraintName: TEpithet;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    class function Candidates: TArray<TStatementClass>; override;
  end;

  { �������� primary key }
  TPrimaryKey = class(TConstraint)
  strict private
    _Primary, _Key: TEpithet;
    _Fields, _UsingIndex: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { �������� foreign key }
  TForeignKey = class(TConstraint)
  strict private
    _Foreign, _Key, _References, _TableName: TEpithet;
    _RefFields, _TargetFields: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { �������� check }
  TCheck = class(TConstraint)
  strict private
    _Check : TEpithet;
    _Condition: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { ��������� tablespace }
  TTablespace = class(TStatement)
  strict private
    _Tablespace, _TablespaceName: TEpithet;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { ��������� using index }
  TUsingIndex = class(TStatement)
  strict private
    _Using, _Index: TEpithet;
    _Tablespace: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { ������� comment }
  TComment = class(TSemicolonStatement)
  strict private
    _Comment, _On, _TableOrColumn: TEpithet;
    _Name: TStatement;
    _Is: TEpithet;
    _Text: TLiteral;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { ������ ������������ }
  [Aligned]
  TComments = class(TStatementList<TComment>)
  strict protected
    function ParseBreak: boolean; override;
  end;

implementation

uses Parser, DML, PLSQL, Expressions;

{ TCreateStatement }

function TCreate.InternalParse: boolean;
begin
  { ���� ���������� ����� create, �� ���������� ����������� }
  _Create := Keyword('create');
  if not Assigned(_Create) then exit(false);
  { �������� ������� or replace }
  _Or := Keyword('or');
  if Assigned(_Or) then _Replace := Keyword('replace');
  if Assigned(_Or) and not Assigned(_Replace) then exit(true);
  { �������� ������� force }
  _Force := Keyword('force');
  { �, �������, ����������, ��� �� �� ������ }
  Result := TParser.ParseCreation(Self, Source, _What) or
            TUnexpectedToken.Parse(Self, Source, _What);
end;

procedure TCreate.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Create, _Or, _Replace, _Force, _What]);
end;

function TCreate.StatementName: string;
begin
  Result := Concat([_Create, _Or, _Replace, _Force, _What]);
end;

{ TView }

function TView.InternalParse: boolean;
begin
  _View := Keyword('view');
  if not Assigned(_View) then exit(false);
  _ViewName := Identifier;
  _As := Keyword('as');
  TSelect.Parse(Self, Source, _Select);
  Result := true;
end;

procedure TView.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_View, _ViewName, _As]);
  APrinter.PrintIndented(_Select);
end;

function TView.StatementName: string;
begin
  Result := Concat([_View, _ViewName]);
end;

{ TTable }

function TTable.InternalParse: boolean;
begin
  _Global := Keyword('global');
  _Temporary := Keyword('temporary');
  _Table  := Keyword('table');
  if not Assigned(_Table) then exit(false);
  _TableName := Identifier;
  TMLBracketedStatement<TCommaList<TTableItem>>.Parse(Self, Source, _Items);
  if Assigned(_Temporary) then
    begin
      _On := Keyword('on');
      _Commit := Keyword('commit');
      _DeleteOrPreserve := Keyword(['delete', 'preserve']);
      _Rows := Keyword('rows');
    end
  else
    TTablespace.Parse(Self, Source, _Tablespace);
  inherited;
  Result := true;
end;

procedure TTable.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Global, _Temporary, _Table, _TableName, _IndentNextLine,
                       _Items, _Tablespace, _On, _Commit, _DeleteOrPreserve,
                       _Rows, _UndentNextLine]);
  inherited;
end;

function TTable.StatementName: string;
begin
  Result := Concat([_Global, _Temporary, _Table, _TableName]);
end;

{ TComment }

function TComment.InternalParse: boolean;
begin
  _Comment := Keyword('comment');
  if not Assigned(_Comment) then exit(false);
  _On := Keyword('on');
  _TableOrColumn := Keyword(['table', 'column']);
  TQualifiedIdent.Parse(Self, Source, _Name);
  _Is := Keyword('is');
  _Text := Literal;
  inherited;
  Result := true;
end;

procedure TComment.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Comment, _On, _TableOrColumn]);
  APrinter.Ruler('name', Settings.AlignTableColumnComments);
  APrinter.PrintItem(_Name);
  APrinter.Ruler('is', Settings.AlignTableColumnComments);
  APrinter.PrintItems([_Is, _Text]);
  inherited;
end;

{ TComments }

function TComments.ParseBreak: boolean;
begin
  Result := not Assigned(Keyword('comment'));
end;

{ TDrop }

function TDrop.InternalParse: boolean;
begin
  _Drop := Keyword('drop');
  if not Assigned(_Drop) then exit(false);
  _ObjectType := Keyword(['table', 'procedure', 'function', 'package', 'view']);
  if not Assigned(_ObjectType) then TUnexpectedToken.Parse(Self, Source, _Unexpected);
  if Assigned(_ObjectType) and (_ObjectType.Value = 'package') then _Body := Keyword('body');
  _ObjectName := Identifier;
  Result := inherited or Assigned(_ObjectType);
end;

procedure TDrop.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Drop, _ObjectType, _Unexpected, _Body, _ObjectName]);
  inherited;
end;

function TDrop.StatementName: string;
begin
  Result := Concat([_Drop, _ObjectType, _Body, _ObjectName]);
end;

{ TTablespace }

function TTablespace.InternalParse: boolean;
begin
  _Tablespace := Keyword('tablespace');
  Result := Assigned(_Tablespace);
  if Result then _TablespaceName := Identifier;
end;

procedure TTablespace.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Tablespace, _TablespaceName]);
end;

{ TTableItem }

class function TTableItem.Candidates: TArray<TStatementClass>;
begin
  Result := TArray<TStatementClass>.Create(TConstraint, TTableField);
end;

{ TTableField }

function TTableField.InternalParse: boolean;
begin
  _Name := Identifier;
  TTypeRef.Parse(Self, Source, _Type);
  if not Assigned(_Name) and not Assigned(_Type) then exit(false);
  _Not  := Keyword('not');
  _Null := Keyword('null');
  Result := true;
end;

procedure TTableField.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Name, _Type, _Not, _Null]);
end;

{ TConstraint }

function TConstraint.InternalParse: boolean;
begin
  _Constraint := Keyword('constraint');
  if not Assigned(_Constraint) then exit(false);
  _ConstraintName := Identifier;
  Result := true;
end;

procedure TConstraint.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Constraint, _ConstraintName]);
end;

class function TConstraint.Candidates: TArray<TStatementClass>;
begin
  if Self = TConstraint
    then Result := TArray<TStatementClass>.Create(TPrimaryKey, TForeignKey, TCheck)
    else Result := TArray<TStatementClass>.Create(Self)
end;

{ TPrimaryKey }

function TPrimaryKey.InternalParse: boolean;
begin
  if not inherited then exit(false);
  _Primary := Keyword('primary');
  if not Assigned(_Primary) then exit(false);
  _Key := Keyword('key');
  TSLBracketedStatement<TIdentFields>.Parse(Self, Source, _Fields);
  TUsingIndex.Parse(Self, Source, _UsingIndex);
  Result := true;
end;

procedure TPrimaryKey.PrintSelf(APrinter: TPrinter);
begin
  inherited;
  APrinter.PrintItems([_Primary, _Key, _Fields, _UsingIndex]);
end;

{ TUsingIndex }

function TUsingIndex.InternalParse: boolean;
begin
  _Using := Keyword('using');
  if not Assigned(_Using) then exit(false);
  _Index := Keyword('index');
  TTablespace.Parse(Self, Source, _Tablespace);
  Result := true;
end;

procedure TUsingIndex.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Using, _Index, _Tablespace]);
end;

{ TForeignKey }

function TForeignKey.InternalParse: boolean;
begin
  if not inherited then exit(false);
  _Foreign := Keyword('foreign');
  if not Assigned(_Foreign) then exit(false);
  _Key := Keyword('key');
  TSLBracketedStatement<TIdentFields>.Parse(Self, Source, _RefFields);
  _References := Keyword('references');
  _TableName  := Identifier;
  TSLBracketedStatement<TIdentFields>.Parse(Self, Source, _TargetFields);
  Result := true;
end;

procedure TForeignKey.PrintSelf(APrinter: TPrinter);
begin
  inherited;
  APrinter.PrintItems([_Foreign, _Key, _RefFields, _References, _TableName, _TargetFields]);
end;

{ TCheck }

function TCheck.InternalParse: boolean;
begin
  if not inherited then exit(false);
  _Check := Keyword('check');
  if not Assigned(_Check) then exit(false);
  TParser.ParseExpression(Self, Source, _Condition);
  Result := true;
end;

procedure TCheck.PrintSelf(APrinter: TPrinter);
begin
  inherited;
  APrinter.PrintItems([_Check, _Condition]);
end;

end.

