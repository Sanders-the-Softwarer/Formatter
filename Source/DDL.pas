////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                       �������������� ����������� DDL                       //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit DDL;

interface

uses SysUtils, Tokens, Statements, PrinterIntf, Streams, Commons, PLSQL;

type

  { ������� create [or replace] }
  TCreate = class(TSemicolonStatement)
  strict private
    _Create, _Or, _Replace, _Editionable, _Force: TEpithet;
    _What: TStatement;
    _Slash: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
    function Grouping: TStatementClass; override;
  end;

  { ������� drop }
  TDrop = class(TSemicolonStatement)
  strict private
    _Drop, _Type, _Force, _CascadeConstraints: TEpithet;
    _Name: TStatement;
    _Slash: TTerminal;
    function IsTable: boolean;
    function IsType: boolean;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
    function Grouping: TStatementClass; override;
  end;

  { ������� alter }
  TAlter = class(TSemicolonStatement)
  strict private
    _Alter: TEpithet;
    _What: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { ������ view }
  TView = class(TStatement)
  strict private
    _View, _As: TEpithet;
    _ViewName, _Columns, _Select: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
  end;

  { ������ index }
  TIndex = class(TSemicolonStatement)
  strict private
    _Unique, _Index, _On: TEpithet;
    _IndexName, _TableName, _Fields, _Tablespace: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { ������ type body }
  TTypeBody = class(TProgramBlock)
  strict protected
    function GetHeaderClass: TStatementClass; override;
  end;

  { ��������� type body }
  TTypeBodyHeader = class(TStatement)
  strict private
    _TypeBody, _AsIs: TEpithet;
    _TypeName: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { ������ table }
  TTable = class(TSemicolonStatement)
  strict private
    _Global, _Temporary, _Table: TEpithet;
    _TableName, _Items: TStatement;
    _Organization, _Index: TEpithet;
    _PartitionBy: TStatement;
    _Tablespace: TStatement;
    _LobStores: TStatement;
    _On, _Commit, _DeleteOrPreserve, _Rows: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
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
    _Default: TEpithet;
    _Value: TStatement;
    _Not, _Null: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { �������� constraint-� }
  TConstraint = class(TTableItem)
  strict private
    _Constraint, _ConstraintName: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    class function Candidates: TArray<TStatementClass>; override;
  end;

  { �������� primary key }
  TPrimaryKey = class(TConstraint)
  strict private
    _Primary, _Key: TEpithet;
    _Fields, _UsingIndex: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { �������� unique }
  TUnique = class(TConstraint)
  strict private
    _Unique: TEpithet;
    _Fields, _UsingIndex: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { �������� foreign key }
  TForeignKey = class(TConstraint)
  strict private
    _Foreign, _Key, _References: TEpithet;
    _RefFields, _TableName, _TargetFields: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { �������� check }
  TCheck = class(TConstraint)
  strict private
    _Check : TEpithet;
    _Condition: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { ��������� tablespace }
  TTablespace = class(TStatement)
  strict private
    _Tablespace: TEpithet;
    _Name: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { ��������� using index }
  TUsingIndex = class(TStatement)
  strict private
    _Using, _Index: TEpithet;
    _Tablespace: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { ��������� lob store as }
  TLobStore = class(TStatement)
  strict private
    _Lob, _Store, _As: TEpithet;
    _Column, _Stored, _TableName: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { ������ lob store-�� }
  TLobStores = class(TStatementList<TLobStore>)
  strict protected
    function ParseBreak: boolean; override;
  end;

  { ��������� partition by }
  TPartitions = class(TStatement)
  strict private
    _PartitionBy, _SubpartitionBy: TEpithet;
    _PRange, _PList, _SRange, _SList, _Partitions: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { ������� range partition }
  TPartitionRange = class(TStatement)
  strict private
    _Range: TEpithet;
    _Fields, _Expression: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { ������� list partition }
  TPartitionList = class(TStatement)
  strict private
    _List: TEpithet;
    _Fields: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { �������� �������� }
  TPartition = class(TStatement)
  strict private
    _Partition, _Name, _Values, _Less, _Than: TEpithet;
    _Expression, _Subpartitions: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
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
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function Grouping: TStatementClass; override;
  end;

  { ����������� sharing }
  TSharing = class(TStatement)
    _Sharing, _What: TEpithet;
    _Eq: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { ������ sequence }
  TSequence = class(TStatement)
  strict private
    _Sequence: TEpithet;
    _Name, _Parts: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { ����� ���������� sequence-� }
  TSequencePart = class(TStatement)
  strict private
    _First, _Second: TEpithet;
    _Value: TNumber;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
  end;

  { ������ ������ � sequence-� }
  TSequencePartList = class(TStatementList<TSequencePart>)
  strict protected
    function ParseBreak: boolean; override;
  end;

  { ������� }
  TSynonym = class(TStatement)
  strict private
    _Synonym, _For: TEpithet;
    _Name, _Sharing, _Object: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function Grouping: TStatementClass; override;
  end;

  { ������������ }
  TUser = class(TStatement)
  strict private
    _User: TEpithet;
    _UserName, _Params: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { ��������� ������������ }
  TUserParam = class(TStatement)
  strict private
    _Name: TEpithet;
    _Value: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { ������� grant }
  TGrant = class(TSemicolonStatement)
  strict private
    _Grant, _On, _To, _IdentifiedBy, _WithAdminOption, _WithGrantOption, _WithHierarchyOption: TEpithet;
    _Privileges, _Object, _Grantee, _Password: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function Grouping: TStatementClass; override;
  end;

  { ���������� ���������� }
  TPrivilege = class(TStatement)
  strict private
    Tokens: array of TToken;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { ������ ���������� ���������� }
  TPrivileges = class(TCommaList<TPrivilege>)
  strict protected
    function ParseBreak: boolean; override;
    function OnePerLine: boolean; override;
  end;

implementation

uses Parser, DML, Expressions, Triggers, Role;

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
  { �������� ������� editionable }
  _Editionable := Keyword(['editionable', 'noneditionable']);
  { �������� ������� force }
  _Force := Keyword('force');
  { �, �������, ����������, ��� �� �� ������ }
  Result := TTable.Parse(Self, Source, _What) or
            TView.Parse(Self, Source, _What) or
            TIndex.Parse(Self, Source, _What) or
            TPackage.Parse(Self, Source, _What) or
            TSubroutine.Parse(Self, Source, _What) or
            TTypeBody.Parse(Self, Source, _What) or
            TType.Parse(Self, Source, _What) or
            TSequence.Parse(Self, Source, _What) or
            TTrigger.Parse(Self, Source, _What) or
            TSynonym.Parse(Self, Source, _What) or
            TUser.Parse(Self, Source, _What) or
            TRole.Parse(Self, Source, _What) or
            TUnexpectedToken.Parse(Self, Source, _What);
  inherited;
  { ����������� ���� }
  _Slash := Terminal('/');
end;

procedure TCreate.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Create, _Or, _Replace, _Editionable, _Force, _What]);
  inherited;
  APrinter.NextLineIf([_Slash]);
end;

function TCreate.StatementName: string;
begin
  Result := Concat([_Create, _Or, _Replace, _What]);
end;

function TCreate.Grouping: TStatementClass;
begin
  if Assigned(_What)
    then Result := _What.Grouping
    else Result := nil;
end;

{ TView }

function TView.InternalParse: boolean;
begin
  _View := Keyword('view');
  if not Assigned(_View) then exit(false);
  TQualifiedIdent.Parse(Self, Source, _ViewName);
  TBracketedStatement<TIdentFields>.Parse(Self, Source, _Columns);
  _As := Keyword('as');
  TSelect.Parse(Self, Source, _Select);
  Result := true;
end;

procedure TView.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_View, _ViewName]);
  APrinter.NextLineIf([_IndentNextLine, _Columns, _UndentNextLine]);
  APrinter.PrintItem(_As);
  APrinter.PrintIndented(_Select);
end;

function TView.StatementName: string;
begin
  Result := Concat([_View, _ViewName]);
end;

{ TIndex }

function TIndex.InternalParse: boolean;
begin
  _Unique := Keyword('unique');
  _Index  := Keyword('index');
  if not Assigned(_Index) then exit(false);
  TQualifiedIdent.Parse(Self, Source, _IndexName);
  _On := Keyword('on');
  TQualifiedIdent.Parse(Self, Source, _TableName);
  TSingleLine<TBracketedStatement<TExpressionFields>>.Parse(Self, Source, _Fields);
  TTablespace.Parse(Self, Source, _Tablespace);
  inherited;
  Result := true;
end;

procedure TIndex.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Unique, _Index, _IndexName, _On, _TableName, _Fields, _Tablespace]);
  inherited;
end;

{ TTypeBody }

function TTypeBody.GetHeaderClass: TStatementClass;
begin
  Result := TTypeBodyHeader;
end;

{ TTypeBodyHeader }

function TTypeBodyHeader.InternalParse: boolean;
begin
  Result := true;
  _TypeBody := Keyword('type body');
  if not Assigned(_TypeBody) then exit(false);
  TQualifiedIdent.Parse(Self, Source, _TypeName);
  _AsIs := Keyword(['as', 'is']);
  if Assigned(_AsIs) then _AsIs.CanReplace := true;
end;

procedure TTypeBodyHeader.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_TypeBody, _TypeName, _AsIs]);
end;

{ TTable }

function TTable.InternalParse: boolean;
begin
  _Global := Keyword('global');
  _Temporary := Keyword('temporary');
  _Table  := Keyword('table');
  if not Assigned(_Table) then exit(false);
  TQualifiedIdent.Parse(Self, Source, _TableName);
  TBracketedStatement<TAligned<TCommaList<TTableItem>>>.Parse(Self, Source, _Items);
  TPartitions.Parse(Self, Source, _PartitionBy);
  if Assigned(_Temporary) then
    begin
      _On := Keyword('on');
      _Commit := Keyword('commit');
      _DeleteOrPreserve := Keyword(['delete', 'preserve']);
      _Rows := Keyword('rows');
    end
  else
    begin
      _Organization := Keyword('organization');
      _Index := Keyword('index');
      TTablespace.Parse(Self, Source, _Tablespace);
      TLobStores.Parse(Self, Source, _LobStores)
    end;
  inherited;
  Result := true;
end;

procedure TTable.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Global, _Temporary, _Table, _TableName, _IndentNextLine,
                       _Items]);
  APrinter.NextLineIf(_PartitionBy);
  APrinter.NextLineIf([_Organization, _Index]);
  APrinter.NextLineIf([_Tablespace]);
  APrinter.NextLineIf([_LobStores]);
  APrinter.NextLineIf([_On, _Commit, _DeleteOrPreserve, _Rows]);
  APrinter.Undent;
  inherited;
end;

function TTable.StatementName: string;
begin
  Result := Concat([_Global, _Temporary, _Table, _TableName]);
end;

{ TTablespace }

function TTablespace.InternalParse: boolean;
begin
  _Tablespace := Keyword('tablespace');
  Result := Assigned(_Tablespace);
  if Result then TQualifiedIdent.Parse(Self, Source, _Name);
end;

procedure TTablespace.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Tablespace, _Name]);
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
  _Default := Keyword('default');
  if Assigned(_Default) then TParser.ParseExpression(Self, Source, _Value);
  _Not  := Keyword('not');
  _Null := Keyword('null');
  Result := true;
end;

procedure TTableField.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.StartRuler(Settings.AlignColumns);
  APrinter.PrintRulerItem('name', _Name);
  APrinter.PrintRulerItem('type', _Type);
  APrinter.PrintRulerItems('default', [_Default, _Value]);
  APrinter.PrintRulerItems('not null', [_Not, _Null]);
end;

{ TConstraint }

function TConstraint.InternalParse: boolean;
begin
  _Constraint := Keyword('constraint');
  if not Assigned(_Constraint) then exit(false);
  _ConstraintName := Identifier;
  Result := true;
end;

procedure TConstraint.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Constraint, _ConstraintName]);
end;

class function TConstraint.Candidates: TArray<TStatementClass>;
begin
  if Self = TConstraint
    then Result := TArray<TStatementClass>.Create(TPrimaryKey, TUnique, TForeignKey, TCheck)
    else Result := TArray<TStatementClass>.Create(Self)
end;

{ TPrimaryKey }

function TPrimaryKey.InternalParse: boolean;
begin
  if not inherited then exit(false);
  _Primary := Keyword('primary');
  if not Assigned(_Primary) then exit(false);
  _Key := Keyword('key');
  TSingleLine<TBracketedStatement<TIdentFields>>.Parse(Self, Source, _Fields);
  TUsingIndex.Parse(Self, Source, _UsingIndex);
  Result := true;
end;

procedure TPrimaryKey.InternalPrintSelf(APrinter: TPrinter);
begin
  inherited;
  APrinter.PrintItems([_Primary, _Key, _Fields, _UsingIndex]);
end;

{ TUnique }

function TUnique.InternalParse: boolean;
begin
  if not inherited then exit(false);
  _Unique := Keyword('unique');
  if not Assigned(_Unique) then exit(false);
  TSingleLine<TBracketedStatement<TIdentFields>>.Parse(Self, Source, _Fields);
  TUsingIndex.Parse(Self, Source, _UsingIndex);
  Result := true;
end;

procedure TUnique.InternalPrintSelf(APrinter: TPrinter);
begin
  inherited;
  APrinter.PrintItems([_Unique, _Fields, _UsingIndex]);
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

procedure TUsingIndex.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Using, _Index, _Tablespace]);
end;

{ TLobStore }

function TLobStore.InternalParse: boolean;
begin
  Result := true;
  _Lob := Keyword('lob');
  if not Assigned(_Lob) then exit(false);
  TBracketedStatement<TQualifiedIdent>.Parse(Self, Source, _Column);
  _Store := Keyword('store');
  _As := Keyword('as');
  TQualifiedIdent.Parse(Self, Source, _TableName);
  TBracketedStatement<TTablespace>.Parse(Self, Source, _Stored);
end;

procedure TLobStore.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.NextLine;
  APrinter.SupressNextLine(true);
  APrinter.PrintItems([_Lob, _Column, _Store, _As, _TableName, _Stored]);
  APrinter.SupressNextLine(false);
end;

{ TLobStores }

function TLobStores.ParseBreak: boolean;
begin
  Result := not Assigned(Keyword('lob'));
end;

{ TForeignKey }

function TForeignKey.InternalParse: boolean;
begin
  if not inherited then exit(false);
  _Foreign := Keyword('foreign');
  if not Assigned(_Foreign) then exit(false);
  _Key := Keyword('key');
  TSingleLine<TBracketedStatement<TIdentFields>>.Parse(Self, Source, _RefFields);
  _References := Keyword('references');
  TQualifiedIdent.Parse(Self, Source, _TableName);
  TSingleLine<TBracketedStatement<TIdentFields>>.Parse(Self, Source, _TargetFields);
  Result := true;
end;

procedure TForeignKey.InternalPrintSelf(APrinter: TPrinter);
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

procedure TCheck.InternalPrintSelf(APrinter: TPrinter);
begin
  inherited;
  APrinter.PrintItems([_Check, _Condition]);
end;

{ TPartitions }

function TPartitions.InternalParse: boolean;
begin
  _PartitionBy := Keyword('partition by');
  if not Assigned(_PartitionBy) then exit(false);
  TPartitionRange.Parse(Self, Source, _PRange);
  if not Assigned(_PRange) then TPartitionList.Parse(Self, Source, _PList);
  _SubpartitionBy := Keyword('subpartition by');
  if Assigned(_SubpartitionBy) then
  begin
    TPartitionRange.Parse(Self, Source, _SRange);
    if not Assigned(_SRange) then TPartitionList.Parse(Self, Source, _SList);
  end;
  TBracketedStatement<TCommaList<TPartition>>.Parse(Self, Source, _Partitions);
  Result := true;
end;

procedure TPartitions.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_PartitionBy, _PRange, _PList, _NextLine,
                       _SubpartitionBy, _SRange, _SList, _IndentNextLine,
                       _Partitions, _UndentNextLine]);
end;

{ TPartitionRange }

function TPartitionRange.InternalParse: boolean;
begin
  _Range := Keyword('range');
  if not Assigned(_Range) then exit(false);
  TSingleLine<TBracketedStatement<TIdentFields>>.Parse(Self, Source, _Fields);
  TParser.ParseExpression(Self, Source, _Expression);
  Result := true;
end;

procedure TPartitionRange.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Range, _Fields, _Expression]);
end;

{ TPartitionList }

function TPartitionList.InternalParse: boolean;
begin
  _List := Keyword('list');
  Result := Assigned(_List);
  if Result then TSingleLine<TBracketedStatement<TIdentFields>>.Parse(Self, Source, _Fields);
end;

procedure TPartitionList.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_List, _Fields]);
end;

{ TPartition }

function TPartition.InternalParse: boolean;
begin
  _Partition := Keyword(['partition', 'subpartition']);
  if not Assigned(_Partition) then exit(false);
  _Name := Identifier;
  _Values := Keyword('values');
  _Less   := Keyword('less');
  _Than   := Keyword('than');
  TParser.ParseExpression(Self, Source, _Expression);
  TBracketedStatement<TCommaList<TPartition>>.Parse(Self, Source, _Subpartitions);
  Result  := true;
end;

procedure TPartition.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Partition, _Name, _Values, _Less, _Than, _Expression, _Indent]);
  APrinter.NextLineIf(_Subpartitions);
  APrinter.Undent;
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

procedure TComment.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.StartRuler(Settings.AlignTableColumnComments);
  APrinter.PrintRulerItems('comment', [_Comment, _On, _TableOrColumn]);
  APrinter.PrintRulerItem ('name', _Name);
  APrinter.PrintRulerItems('is', [_Is, _Text]);
  inherited;
end;

function TComment.Grouping: TStatementClass;
begin
  Result := TComment;
end;

{ TDrop }

function TDrop.InternalParse: boolean;
begin
  _Drop := Keyword('drop');
  if not Assigned(_Drop) then exit(false);
  _Type := Keyword(['table', 'procedure', 'function', 'package', 'package body',
                    'view', 'index', 'type', 'type body', 'sequence', 'trigger',
                    'synonym', 'public synonym', 'role']);
  TQualifiedIdent.Parse(Self, Source, _Name);
  if IsTable then _CascadeConstraints := Keyword('cascade constraints');
  if IsType then _Force := Keyword('force');
  Result := inherited or Assigned(_Type);
  if Result then _Slash := Terminal('/');
end;

procedure TDrop.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Drop, _Type, _Name, _Force, _CascadeConstraints]);
  inherited;
  APrinter.NextLineIf([_Slash]);
end;

function TDrop.StatementName: string;
begin
  Result := Concat([_Drop, _Type, _Name]);
end;

function TDrop.Grouping: TStatementClass;
begin
  Result := TDrop;
end;

function TDrop.IsTable: boolean;
begin
  Result := Assigned(_Type) and (_Type.Value = 'table');
end;

function TDrop.IsType: boolean;
begin
  Result := Assigned(_Type) and (_Type.Value = 'type');
end;

{ TSequence }

function TSequence.InternalParse: boolean;
begin
  Result := true;
  _Sequence := Keyword('sequence');
  if not Assigned(_Sequence) then exit(false);
  TQualifiedIdent.Parse(Self, Source, _Name);
  TSequencePartList.Parse(Self, Source, _Parts);
end;

procedure TSequence.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Sequence, _Name, _Indent]);
  APrinter.NextLineIf([_Parts]);
  APrinter.Undent;
end;

{ TSequencePart }

function TSequencePart.InternalParse: boolean;
begin
  _First  := Keyword(['increment', 'start', 'maxvalue', 'nomaxvalue', 'minvalue', 'nominvalue', 'cycle', 'nocycle', 'cache', 'nocache', 'order', 'noorder']);
  _Second := Keyword(['by', 'with']);
  _Value  := Number;
  Result  := Assigned(_First);
end;

procedure TSequencePart.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_First, _Second, _Value]);
end;

function TSequencePart.StatementName: string;
begin
  Result := Concat([_First, _Second, _Value]);
end;

{ TSequencePartList }

function TSequencePartList.ParseBreak: boolean;
var _Dummy: TStatement;
begin
  Result := not TSequencePart.Parse(Self, Source, _Dummy);
end;

{ TGrant }

function TGrant.InternalParse: boolean;
begin
  _Grant := Keyword('grant');
  if not Assigned(_Grant) then exit(false);
  TPrivileges.Parse(Self, Source, _Privileges);
  _On := Keyword('on');
  if Assigned(_On) then TQualifiedIdent.Parse(Self, Source, _Object);
  _To := Keyword('to');
  TQualifiedIdent.Parse(Self, Source, _Grantee);
  _IdentifiedBy := Keyword('identified by');
  if Assigned(_IdentifiedBy) then TQualifiedIdent.Parse(Self, Source, _Password);
  _WithAdminOption := Keyword('with admin option');
  _WithHierarchyOption := Keyword('with hierarchy option');
  _WithGrantOption := Keyword('with grant option');
  Result := true;
  inherited;
end;

procedure TGrant.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.StartRuler(Settings.AlignSQLPLUS);
  APrinter.PrintRulerItems('Grant', [_Grant, _Privileges]);
  APrinter.PrintRulerItems('On', [_On, _Object]);
  APrinter.PrintRulerItems('To', [_To, _Grantee]);
  APrinter.PrintRulerItems('Identified', [_IdentifiedBy, _Password]);
  APrinter.PrintRulerItems('With', [_WithAdminOption, _WithHierarchyOption, _WithGrantOption]);
  inherited;
end;

function TGrant.Grouping: TStatementClass;
begin
  Result := TGrant;
end;

{ TPrivileges }

function TPrivileges.ParseBreak: boolean;
begin
  Result := Any([Terminal(';'), Keyword(['on', 'to', 'with', 'with admin option', 'with grant option', 'with hierarchy option'])]);
end;

function TPrivileges.OnePerLine: boolean;
begin
  Result := false;
end;

{ TPrivilege }

function TPrivilege.InternalParse: boolean;
var
  Savepoint: TMark;
  T: TToken;
  L: integer;
begin
  Savepoint := Source.Mark;
  { ������� ���� ���� ��� ��������� ��������������� }
  T := NextToken;
  while (T is TEpithet) and not SameStr(T.Value, 'on') and not SameStr(T.Value, 'to') do
  begin
    TEpithet(T).IsIdent := true;
    Savepoint := Source.Mark;
    L := Length(Tokens);
    SetLength(Tokens, L + 1);
    Tokens[L] := T;
    T := NextToken;
  end;
  { ���� ������ �� ����������� ������, ������ ������� }
  if not SameStr(T.Value, '(') then
  begin
    Source.Restore(Savepoint);
    exit(Length(Tokens) > 0);
  end;
  { � ���� ����������� - ������ �������� ������� ������ �� ����������� }
  repeat
    if T is TEpithet then TEpithet(T).IsIdent := true;
    L := Length(Tokens);
    SetLength(Tokens, L + 1);
    Tokens[L] := T;
    if SameStr(T.Value, ')')
      then exit(true)
      else T := NextToken;
  until false;
end;

procedure TPrivilege.InternalPrintSelf(APrinter: TPrinter);
var i: integer;
begin
  for i := Low(Tokens) to High(Tokens) do APrinter.PrintItem(Tokens[i]);
end;

{ TSynonym }

function TSynonym.InternalParse: boolean;
begin
  _Synonym := Keyword(['synonym', 'public synonym']);
  if not Assigned(_Synonym) then exit(false);
  TQualifiedIdent.Parse(Self, Source, _Name);
  TSharing.Parse(Self, Source, _Sharing);
  _For := Keyword('for');
  if Assigned(_For) then TQualifiedIdent.Parse(Self, Source, _Object);
  Result := true;
end;

procedure TSynonym.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.StartRuler(Settings.AlignSQLPLUS);
  APrinter.PrintRulerItems('start', [_Synonym]);
  APrinter.PrintRulerItems('name',  [_Name, _Sharing]);
  APrinter.PrintRulerItems('for',   [ _For, _Object]);
end;

function TSynonym.Grouping: TStatementClass;
begin
  Result := TSynonym;
end;

{ TUser }

function TUser.InternalParse: boolean;
begin
  Result := true;
  _User := Keyword('user');
  if not Assigned(_User) then exit(false);
  TQualifiedIdent.Parse(Self, Source, _UserName);
  TStrictStatementList<TUserParam>.Parse(Self, Source, _Params);
end;

procedure TUser.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_User, _UserName, _IndentNextLine, _Params, _Undent]);
end;

{ TUserParam }

function TUserParam.InternalParse: boolean;
begin
  _Name := Keyword(['identified by', 'default tablespace', 'temporary tablespace', 'quota unlimited on', 'profile']);
  Result := Assigned(_Name);
  if Result then TQualifiedIdent.Parse(Self, Source, _Value);
end;

procedure TUserParam.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Name, _Value]);
end;

{ TSharing }

function TSharing.InternalParse: boolean;
begin
  Result := true;
  _Sharing := Keyword('sharing');
  if not Assigned(_Sharing) then exit(false);
  _Eq := Terminal('=');
  _What := Keyword(['metadata', 'none']);
end;

procedure TSharing.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Sharing, _Eq, _What]);
end;

{ TAlter }

function TAlter.InternalParse: boolean;
begin
  Result := true;
  _Alter := Keyword('alter');
  if not Assigned(_Alter) then exit(false);
  if not TSequence.Parse(Self, Source, _What) and
     not TRole.Parse(Self, Source, _What) then
    TUnexpectedToken.Parse(Self, Source, _What);
  inherited;
end;

procedure TAlter.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Alter, _What]);
  inherited;
end;

end.

