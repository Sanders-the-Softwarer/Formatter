////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                       Синтаксические конструкции DDL                       //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit DDL;

interface

uses SysUtils, Tokens, Statements, PrinterIntf, Streams, Commons, PLSQL;

type

  { Команда create [or replace] }
  TCreate = class(TSemicolonStatement)
  strict private
    _Create, _Or, _Replace, _Force: TEpithet;
    _What: TStatement;
    _Slash: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
  end;

  { Команда drop }
  TDrop = class(TSemicolonStatement)
  strict private
    _Drop, _ObjectType, _Body, _Force, _Cascade, _Constraints: TEpithet;
    _ObjectName, _Unexpected: TStatement;
    _Slash: TTerminal;
    function IsTable: boolean;
    function IsType: boolean;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
  end;

  { Объект view }
  TView = class(TStatement)
  strict private
    _View, _As: TEpithet;
    _ViewName, _Select: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
  end;

  { Объект index }
  TIndex = class(TSemicolonStatement)
  strict private
    _Unique, _Index, _On: TEpithet;
    _IndexName, _TableName, _Fields, _Tablespace: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Объект type body }
  TTypeBody = class(TProgramBlock)
  strict protected
    function GetHeaderClass: TStatementClass; override;
  end;

  { Заголовок type body }
  TTypeBodyHeader = class(TStatement)
  strict private
    _TypeBody, _AsIs: TEpithet;
    _TypeName: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Объект table }
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

  { Общий класс для составных частей таблицы }
  TTableItem = class(TStatement)
  public
    class function Candidates: TArray<TStatementClass>; override;
  end;

  { Описание поля таблицы }
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

  { Описание constraint-а }
  TConstraint = class(TTableItem)
  strict private
    _Constraint, _ConstraintName: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    class function Candidates: TArray<TStatementClass>; override;
  end;

  { Описание primary key }
  TPrimaryKey = class(TConstraint)
  strict private
    _Primary, _Key: TEpithet;
    _Fields, _UsingIndex: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Описание unique }
  TUnique = class(TConstraint)
  strict private
    _Unique: TEpithet;
    _Fields, _UsingIndex: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Описание foreign key }
  TForeignKey = class(TConstraint)
  strict private
    _Foreign, _Key, _References: TEpithet;
    _RefFields, _TableName, _TargetFields: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Описание check }
  TCheck = class(TConstraint)
  strict private
    _Check : TEpithet;
    _Condition: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Выражение tablespace }
  TTablespace = class(TStatement)
  strict private
    _Tablespace, _TablespaceName: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Выражение using index }
  TUsingIndex = class(TStatement)
  strict private
    _Using, _Index: TEpithet;
    _Tablespace: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Выражение lob store as }
  TLobStore = class(TStatement)
  strict private
    _Lob, _Store, _As: TEpithet;
    _Column, _Stored, _TableName: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Список lob store-ов }
  TLobStores = class(TStatementList<TLobStore>)
  strict protected
    function ParseBreak: boolean; override;
  end;

  { Выражение partition by }
  TPartitions = class(TStatement)
  strict private
    _PartitionBy, _SubpartitionBy: TEpithet;
    _PRange, _PList, _SRange, _SList, _Partitions: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Условие range partition }
  TPartitionRange = class(TStatement)
  strict private
    _Range: TEpithet;
    _Fields, _Expression: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Условие list partition }
  TPartitionList = class(TStatement)
  strict private
    _List: TEpithet;
    _Fields: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Описание партиции }
  TPartition = class(TStatement)
  strict private
    _Partition, _Name, _Values, _Less, _Than: TEpithet;
    _Expression, _Subpartitions: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Команда comment }
  TComment = class(TSemicolonStatement)
  strict private
    _Comment, _On, _TableOrColumn: TEpithet;
    _Name: TStatement;
    _Is: TEpithet;
    _Text: TLiteral;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Группа комментариев }
  TComments = class(TStatementList<TComment>)
  strict protected
    function ParseBreak: boolean; override;
  public
    function Aligned: boolean; override;
  end;

  { Объект sequence }
  TSequence = class(TStatement)
  strict private
    _Sequence: TEpithet;
    _Name, _Parts: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Части декларации sequence-а }
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

  { Список частей в sequence-е }
  TSequencePartList = class(TStatementList<TSequencePart>)
  strict protected
    function ParseBreak: boolean; override;
  end;

  { Команда grant }
  TGrant = class(TSemicolonStatement)
  strict private
    _Grant, _On, _To, _Grantee, _IdentifiedBy, _Password, _WithAdminOption, _WithGrantOption, _WithHierarchyOption: TEpithet;
    _Privileges, _Object: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Список грантов }
  TGrants = class(TStatementList<TGrant>)
  strict protected
    function AllowUnexpected: boolean; override;
  end;

  { Грантуемая привилегия }
  TPrivilege = class(TStatement)
  strict private
    Tokens: array of TToken;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Список грантуемых привилегий }
  TPrivileges = class(TCommaList<TPrivilege>)
  strict protected
    function ParseBreak: boolean; override;
    function OnePerLine: boolean; override;
  end;

implementation

uses Parser, DML, Expressions;

{ TCreateStatement }

function TCreate.InternalParse: boolean;
begin
  { Если распознали слово create, то распознали конструкцию }
  _Create := Keyword('create');
  if not Assigned(_Create) then exit(false);
  { Проверим наличие or replace }
  _Or := Keyword('or');
  if Assigned(_Or) then _Replace := Keyword('replace');
  if Assigned(_Or) and not Assigned(_Replace) then exit(true);
  { Проверим наличие force }
  _Force := Keyword('force');
  { И, наконец, распознаем, что же мы создаём }
  Result := TTable.Parse(Self, Source, _What) or
            TView.Parse(Self, Source, _What) or
            TIndex.Parse(Self, Source, _What) or
            TPackage.Parse(Self, Source, _What) or
            TSubroutine.Parse(Self, Source, _What) or
            TTypeBody.Parse(Self, Source, _What) or
            TType.Parse(Self, Source, _What) or
            TSequence.Parse(Self, Source, _What) or
            TTrigger.Parse(Self, Source, _What) or
            TUnexpectedToken.Parse(Self, Source, _What);
  inherited;
  { Завершающий слеш }
  _Slash := Terminal('/');
end;

procedure TCreate.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Create, _Or, _Replace, _Force, _What]);
  inherited;
  APrinter.NextLineIf([_Slash]);
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
  TQualifiedIdent.Parse(Self, Source, _ViewName);
  _As := Keyword('as');
  TSelect.Parse(Self, Source, _Select);
  Result := true;
end;

procedure TView.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_View, _ViewName, _As]);
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
  if Result then _TablespaceName := Identifier;
end;

procedure TTablespace.InternalPrintSelf(APrinter: TPrinter);
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
  APrinter.PrintRulerItems('notnull', [_Not, _Null]);
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

{ TComments }

function TComments.Aligned: boolean;
begin
  Result := true;
end;

function TComments.ParseBreak: boolean;
begin
  Result := not Assigned(Keyword('comment'));
end;

{ TDrop }

function TDrop.InternalParse: boolean;
begin
  _Drop := Keyword('drop');
  if not Assigned(_Drop) then exit(false);
  _ObjectType := Keyword(['table', 'procedure', 'function', 'package', 'view', 'index', 'type', 'sequence', 'trigger']);
  if not Assigned(_ObjectType) then TUnexpectedToken.Parse(Self, Source, _Unexpected);
  if Assigned(_ObjectType) and ((_ObjectType.Value = 'package') or (_ObjectType.Value = 'type')) then _Body := Keyword('body');
  TQualifiedIdent.Parse(Self, Source, _ObjectName);
  if IsTable then
  begin
    _Cascade := Keyword('cascade');
    _Constraints := Keyword('constraints');
  end;
  if IsType then _Cascade := Keyword('force');
  Result := inherited or Assigned(_ObjectType);
  if Result then _Slash := Terminal('/');
end;

procedure TDrop.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Drop, _ObjectType, _Unexpected, _Body, _ObjectName, _Force, _Cascade, _Constraints]);
  inherited;
  APrinter.NextLineIf([_Slash]);
end;

function TDrop.StatementName: string;
begin
  Result := Concat([_Drop, _ObjectType, _Body, _ObjectName]);
end;

function TDrop.IsTable: boolean;
begin
  Result := Assigned(_ObjectType) and (_ObjectType.Value = 'table');
end;

function TDrop.IsType: boolean;
begin
  Result := Assigned(_ObjectType) and (_ObjectType.Value = 'type');
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
  _Grantee := Identifier;
  _IdentifiedBy := Keyword('identified by');
  if Assigned(_IdentifiedBy) then _Password := Identifier;
  _WithAdminOption := Keyword('with admin option');
  _WithHierarchyOption := Keyword('with hierarchy option');
  _WithGrantOption := Keyword('with grant option');
  Result := true;
  inherited;
end;

procedure TGrant.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Grant, _Privileges, _On, _Object, _To, _Grantee,
    _IdentifiedBy, _Password, _WithAdminOption, _WithHierarchyOption, _WithGrantOption]);
  inherited;
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
  { Сначала идут один или несколько идентификаторов }
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
  { Если теперь не открывающая скобка, разбор окончен }
  if not SameStr(T.Value, '(') then
  begin
    Source.Restore(Savepoint);
    exit(Length(Tokens) > 0);
  end;
  { А если открывающая - читаем названия колонок вплоть до закрывающей }
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

{ TGrants }

function TGrants.AllowUnexpected: boolean;
begin
  Result := false;
end;

end.

