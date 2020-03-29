////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                             Синтаксис триггеров                            //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Trigger;

interface

uses Tokens, Statements, Printer, PLSQL;

type
  { Триггер }
  TTrigger = class(TStatement)
  strict private
    _Trigger, _Type, _ForEachRow, _State, _When: TEpithet;
    _Name, _Sharing, _DefaultCollation, _Events, _Referencing,
    _Edition, _Ordering, _Condition, _Body: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
  end;

  { Событие срабатывания триггера }
  TTriggerEvent = class(TStatement)
  strict private
    _Event, _Of: TEpithet;
    _Columns: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
  end;

  { События срабатывания триггера }
  TTriggerEvents = class(TStatementList<TTriggerEvent>)
  strict private
    _On, _Nested, _Table, _Column, _Of: TEpithet;
    _Target: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
    function ParseDelimiter(out AResult: TObject): boolean; override;
    function AllowUnexpected: boolean; override;
  end;

  { Конструкция referencing }
  TReferencing = class(TStatement)
  strict private
    _Referencing, _Old, _New, _Parent, _OldVar, _NewVar, _ParentVar,
      _OldAs, _NewAs, _ParentAs: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция default collation }
  TDefaultCollation = class(TStatement)
  strict private
    _Default, _Collation, _Option: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция edition }
  TTriggerEdition = class(TStatement)
  strict private
    _Direction, _Crossedition: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция ordering }
  TTriggerOrdering = class(TStatement)
  strict private
    _Relation: TEpithet;
    _Target: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Тело compound триггера }
  TTriggerCompoundBody = class(TStatement)
  strict private
    _CompoundTrigger, _End, _AfterEnd: TEpithet;
    _Declarations, _Blocks: TStatement;
    _Semicolon: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Блок составного триггера }
  TTimingBlock = class(TProgramBlock)
  strict protected
    function GetHeaderClass: TStatementClass; override;
    function ParseAfterEnd: TObject; override;
    function IndentBody: boolean; override;
  public
    function StatementName: string; override;
  end;

  { Заголовок блока }
  TTimingBlockHeader = class(TStatement)
  private
    _TimingPoint, _Is: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses Parser, Commons, DML, DDL, SQLPlus, Expressions;

{ TTrigger }

function TTrigger.InternalParse: boolean;
begin
  Result := true;
  _Trigger := Keyword('trigger');
  if not Assigned(_Trigger) then exit(false);
  TQualifiedIdent.Parse(Self, Source, _Name);
  TSharing.Parse(Self, Source, _Sharing);
  TDefaultCollation.Parse(Self, Source, _DefaultCollation);
  _Type := Keyword(['before', 'after', 'instead of', 'for']);
  if Assigned(_Type) then TSingleLine<TTriggerEvents>.Parse(Self, Source, _Events);
  TReferencing.Parse(Self, Source, _Referencing);
  _ForEachRow := Keyword('for each row');
  TTriggerEdition.Parse(Self, Source, _Edition);
  TTriggerOrdering.Parse(Self, Source, _Ordering);
  _State := Keyword(['enable', 'disable']);
  _When := Keyword('when');
  if Assigned(_When) then TParser.ParseExpression(Self, Source, _Condition);
  { Самое весёлое - тело триггера }
  if not TTriggerCompoundBody.Parse(Self, Source, _Body) and
     not TAnonymousBlock.Parse(Self, Source, _Body) and
     not TCall.Parse(Self, Source, _Body) then exit;
end;

procedure TTrigger.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Trigger, _Name,             _IndentNextLine,
                                 _Sharing,          _NextLine,
                                 _DefaultCollation, _NextLine,
                                 _Type,             _Events,         _NextLine,
                                 _Referencing,      _NextLine,
                                 _ForEachRow,       _NextLine,
                                 _Edition,          _NextLine,
                                 _Ordering,         _NextLine,
                                 _State,            _NextLine,
                                 _When,             _Condition,      _UndentNextLine,
                       _Body]);
end;

function TTrigger.StatementName: string;
begin
  Result := Concat([_Trigger, _Name]);
end;

{ TTriggerEvents }

function TTriggerEvents.InternalParse: boolean;
begin
  Result := true;
  if not inherited then exit(false);
  _On := Keyword('on');
  if not Assigned(_On) then exit(false);
  _Nested := Keyword('nested');
  if Assigned(_Nested) then _Table := Keyword('table');
  if Assigned(_Table) then _Column := Identifier;
  if Assigned(_Column) then _Of := Keyword('of');
  TQualifiedIdent.Parse(Self, Source, _Target);
end;

procedure TTriggerEvents.InternalPrintSelf(APrinter: TPrinter);
begin
  inherited;
  APrinter.PrintItems([_On, _Nested, _Table, _Column, _Of, _Target]);
end;

function TTriggerEvents.ParseDelimiter(out AResult: TObject): boolean;
begin
  AResult := Keyword('or');
  Result := Assigned(AResult);
end;

function TTriggerEvents.AllowUnexpected: boolean;
begin
  Result := false;
end;

{ TTriggerEvent }

function TTriggerEvent.InternalParse: boolean;
begin
  Result := true;
  _Event := Keyword(['insert', 'update', 'delete', 'alter', 'analyze',
    'associate statistics', 'audit', 'comment', 'create',
    'disassociate statistics', 'drop', 'grant', 'noaudit', 'rename', 'revoke',
    'truncate', 'ddl', 'after servererror', 'after logon', 'before logoff',
    'after suspend', 'after clone', 'before unplug', 'before set container',
    'after set container', 'after startup', 'before shutdown',
    'after db_role_change']);
  if not Assigned(_Event) then exit(false);
  if _Event.Value = 'update' then
  begin
    _Of := Keyword('of');
    if Assigned(_Of) then TSingleLine<TIdentFields>.Parse(Self, Source, _Columns);
  end;
end;

procedure TTriggerEvent.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Event, _Of, _Columns]);
end;

function TTriggerEvent.StatementName: string;
begin
  Result := Concat([_Event, _Of, _Columns]);
end;

{ TReferencing }

function TReferencing.InternalParse: boolean;
var
  i: integer;
  K: TEpithet;
begin
  Result := true;
  _Referencing := Keyword('referencing');
  if not Assigned(_Referencing) then exit(false);
  for i := 1 to 3 do
  begin
    K := Keyword(['old', 'new', 'parent']);
    if K.Value = 'old' then
      begin
        _Old := K;
        _OldAs := Keyword('as');
        if Assigned(_OldAs) then _OldVar := Identifier else exit;
      end
    else if K.Value = 'new' then
      begin
        _New := K;
        _NewAs := Keyword('as');
        if Assigned(_NewAs) then _NewVar := Identifier else exit;
      end
    else if K.Value = 'parent' then
      begin
        _Parent := K;
        _ParentAs := Keyword('as');
        if Assigned(_ParentAs) then _ParentVar := Identifier else exit;
      end
    else
      exit;
  end;
end;

procedure TReferencing.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Referencing, _Old, _OldAs, _OldVar,
                                     _New, _NewAs, _NewVar,
                                     _Parent, _ParentAs, _ParentVar]);
end;

{ TDefaultCollation }

function TDefaultCollation.InternalParse: boolean;
begin
  _Default := Keyword('default');
  _Collation := Keyword('collation');
  Result := Assigned(_Default) and Assigned(_Collation);
  if Result then _Option := Keyword('using_nls_comp');
end;

procedure TDefaultCollation.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Default, _Collation, _Option]);
end;

{ TTriggerEdition }

function TTriggerEdition.InternalParse: boolean;
begin
  _Direction := Keyword(['forward', 'reverse']);
  _Crossedition := Keyword('crossedition');
  Result := Assigned(_Crossedition);
end;

procedure TTriggerEdition.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Direction, _Crossedition]);
end;

{ TTriggerOrdering }

function TTriggerOrdering.InternalParse: boolean;
begin
  _Relation := Keyword(['follows', 'precedes']);
  Result := Assigned(_Relation);
  if Result then TQualifiedIdent.Parse(Self, Source, _Target);
end;

procedure TTriggerOrdering.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Relation, _Target]);
end;

{ TTriggerCompoundBody }

function TTriggerCompoundBody.InternalParse: boolean;
begin
  Result := true;
  _CompoundTrigger := Keyword('compound trigger');
  if not Assigned(_CompoundTrigger) then exit(false);
  TDeclarations.Parse(Self, Source, _Declarations);
  TStrictStatementList<TTimingBlock>.Parse(Self, Source, _Blocks);
  _End := Keyword('end');
  if Assigned(_End) then _AfterEnd := Identifier;
  _Semicolon := Terminal(';');
end;

procedure TTriggerCompoundBody.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_CompoundTrigger, _IndentNextLine,
                                         _Declarations,    _UndentNextLine,
                       _Blocks,          _NextLine,
                       _End,             _AfterEnd,       _Semicolon]);
end;

{ TTimingBlock }

function TTimingBlock.GetHeaderClass: TStatementClass;
begin
  Result := TTimingBlockHeader;
end;

function TTimingBlock.ParseAfterEnd: TObject;
begin
  Result := Keyword(['before each row', 'before statement', 'after each row',
                     'after statement', 'instead of each row']);
end;

function TTimingBlock.IndentBody: boolean;
begin
  Result := true;
end;

function TTimingBlock.StatementName: string;
begin
  Result := Concat([(Header as TTimingBlockHeader)._TimingPoint]);
end;

{ TTimingBlockHeader }

function TTimingBlockHeader.InternalParse: boolean;
begin
  Result := true;
  _TimingPoint := Keyword(['before each row', 'before statement', 'after each row',
                           'after statement', 'instead of each row']);
  if not Assigned(_TimingPoint) then exit(false);
  _Is := Keyword('is');
end;

procedure TTimingBlockHeader.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_TimingPoint, _Is]);
end;

end.
