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

uses SysUtils, Tokens, Statements, Parser, Printer, Streams, Commons, PLSQL;

type

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
    class function Candidates(AParent: TStatement): TArray<TStatementClass>; override;
  end;

  { Список полей и ограничений таблицы }
  TTableItems = class(TCommaList<TTableItem>)
  strict protected
    function Aligned: TAlignMode; override;
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
    class function Candidates(AParent: TStatement): TArray<TStatementClass>; override;
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
    _Tablespace: TEpithet;
    _Name: TStatement;
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
  public
    function Grouping: TStatementClass; override;
    function SameTypeAligned: TAlignMode; override;
  end;

  { Конструкция sharing }
  TSharing = class(TStatement)
  strict private
    _Sharing, _What: TEpithet;
    _Eq: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Пользователь }
  TUser = class(TStatement)
  strict private
    _User: TEpithet;
    _UserName, _Params: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Параметры пользователя }
  TUserParam = class(TStatement)
  strict private
    _Name: TEpithet;
    _Value: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

{ Парсер для DDL }
function DDLParser: TParserInfo;

implementation

uses DML, Expressions, Trigger, Role, Sequence, Synonym, Alter,
  Set_, Select, Grant, Drop, DML_Commons, OracleCore, OracleCreate;

{ Парсер для DDL }
function DDLParser: TParserInfo;
begin
  Result := TParserInfo.InstanceFor('Oracle.DDL');
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
  TBracketedStatement<TTableItems>.Parse(Self, Source, _Items);
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
      TLobStores.Parse(Self, Source, _LobStores);
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

class function TTableItem.Candidates(AParent: TStatement): TArray<TStatementClass>;
begin
  Result := [TConstraint, TTableField];
end;

{ TTableItems }

function TTableItems.Aligned: TAlignMode;
begin
  Result := AlignMode(Settings.AlignColumns);
end;

{ TTableField }

function TTableField.InternalParse: boolean;
begin
  _Name := Identifier;
  TTypeRef.Parse(Self, Source, _Type);
  if not Assigned(_Name) and not Assigned(_Type) then exit(false);
  _Default := Keyword('default');
  if Assigned(_Default) then TExpression.Parse(Self, Source, _Value);
  _Not  := Keyword('not');
  _Null := Keyword('null');
  Result := true;
end;

procedure TTableField.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintRulerItems('name', [_Name]);
  APrinter.PrintRulerItems('type', [_Type]);
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

class function TConstraint.Candidates(AParent: TStatement): TArray<TStatementClass>;
begin
  if Self = TConstraint
    then Result := [TPrimaryKey, TUnique, TForeignKey, TCheck]
    else Result := [Self];
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
  TExpression.Parse(Self, Source, _Condition);
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
  TExpression.Parse(Self, Source, _Expression);
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
  TExpression.Parse(Self, Source, _Expression);
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
  APrinter.PrintRulerItems('comment', [_Comment, _On, _TableOrColumn]);
  APrinter.PrintRulerItems('name', [_Name]);
  APrinter.PrintRulerItems('is', [_Is, _Text]);
  inherited;
end;

function TComment.Grouping: TStatementClass;
begin
  Result := TComment;
end;

function TComment.SameTypeAligned: TAlignMode;
begin
  Result := AlignMode(Settings.AlignCommands);
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
  _What := Keyword(['metadata', 'data', 'none']);
end;

procedure TSharing.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Sharing, _Eq, _What]);
end;

initialization
  { Зарегистрируем конструкции DDL }
  with DDLParser do
  begin
    Add(TOracleCreate);
    Add(TDrop);
    Add(TAlter);
    Add(TSet);
    Add(TComment);
    Add(TGrant);
  end;
  { И добавим их в общеоракловый синтаксис }
  OracleParser.Add(DDLParser);

end.

