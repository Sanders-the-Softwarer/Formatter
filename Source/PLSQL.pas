﻿////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                      Синтаксические конструкции PL/SQL                     //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit PLSQL;

{ ----- Примечания -------------------------------------------------------------

  Объекты, представляющие собой различные части выражения, в основном
  декларируются как TStatement, а не как вхождение класса-наследника,
  используемого в реальности. Это связано с тем, что никто не обещал
  нам корректности форматируемого кода, и на этом месте в результате
  парсинга может оказаться unexpected token или другой подобный объект.
  Кроме того, такой подход значительно уменьшает связность модулей.

  Мы рассматриваем метку как специфический вид оператора. Это неверно с
  семантической точки зрения, но удобно с практической.

------------------------------------------------------------------------------ }

interface

uses Classes, Windows, SysUtils, Math, Streams, Tokens, Statements, Commons,
  PrinterIntf, Utils;

type

  { Метка }
  TLabel = class(TStatement)
  strict private
    _Name: TToken;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Базовый класс для операторов PL/SQL }
  TPLSQLStatement = class(TSemicolonStatement);

  { Программный блок - та или иная конструкция на основе begin .. end }
  TProgramBlock = class(TPLSQLStatement)
  strict private
    _Header, _Declarations, _Operators, _Handlers: TStatement;
    _Begin, _Exception, _End: TEpithet;
    _AfterEnd: TObject;
  strict protected
    property Header: TStatement read _Header;
    function GetHeaderClass: TStatementClass; virtual; abstract;
    function ParseAfterEnd: TObject; virtual;
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
    function IndentBody: boolean; virtual;
  end;

  { Заголовок анонимного блока }
  TAnonymousHeader = class(TStatement)
  strict private
    _Declare: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Анонимный блок }
  TAnonymousBlock = class(TProgramBlock)
  strict protected
    function GetHeaderClass: TStatementClass; override;
  end;

  { Заголовок пакета }
  TPackageHeader = class(TStatement)
  strict private
    _Package, _AuthId, _AsIs: TEpithet;
    _Name: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
  end;

  { Пакет }
  TPackage = class(TProgramBlock)
  strict protected
    function GetHeaderClass: TStatementClass; override;
  public
    function StatementName: string; override;
  end;

  { Объявление подпрограммы }
  TSubroutineHeaderBase = class(TStatement)
  strict private
    _Map: TEpithet;
    _FunctionType: TEpithet;
    _ProcedureOrFunction: TEpithet;
    _Name: TStatement;
    _Params: TStatement;
    _Return: TEpithet;
    _SelfAsResult: TEpithet;
    _ReturnType: TStatement;
    _Deterministic: TEpithet;
    _Pipelined: TEpithet;
    FIndentedBeforeIs, FIndentedAfterIs: boolean;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
    property IndentedBeforeIs: boolean read FIndentedBeforeIs;
    property IndentedAfterIs: boolean read FIndentedAfterIs write FIndentedAfterIs;
  end;

  { Заголовок подпрограммы }
  TSubroutineHeader = class(TSubroutineHeaderBase)
  strict private
    _Is: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function Name: string; override;
  end;

  { Предварительное объявление подпрограммы }
  TSubroutineForwardDeclaration = class(TSubroutineHeaderBase)
  strict private
    _Semicolon: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Подпрограмма }
  TSubroutine = class(TProgramBlock)
  strict protected
    function GetHeaderClass: TStatementClass; override;
  public
    function StatementName: string; override;
  end;

  { Параметр подпрограммы }
  TParamDeclaration = class(TStatement)
  strict private
    _ParamName: TEpithet;
    _InOut: TEpithet;
    _Nocopy: TEpithet;
    _ParamType: TStatement;
    _Assignment: TToken;
    _DefaultValue: TStatement;
    FreeAccessor: boolean;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    procedure BeforeDestruction; override;
    function StatementName: string; override;
  end;

  { Блок параметров подпрограммы }
  TParamsDeclaration = class(TCommaList<TParamDeclaration>)
  strict private
    _OpenBracket: TTerminal;
    _CloseBracket: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    function ParseBreak: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function Aligned: boolean; override;
  end;

  { Блок деклараций }
  TDeclarations = class(TStatementList<TStatement>)
  strict protected
    function ParseStatement(out AResult: TStatement): boolean; override;
    function ParseBreak: boolean; override;
  public
    function EmptyLineBefore: boolean; override;
    function EmptyLineAfter: boolean; override;
    function EmptyLineInside: boolean; override;
  end;

  { Объявление переменной }
  TVariableDeclaration = class(TPLSQLStatement)
  strict private
    class var SkipSemicolonCheck: boolean;
  strict private
    _Name: TEpithet;
    _Constant: TEpithet;
    _Type: TStatement;
    _Assignment: TToken;
    _Value: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
    class function WithoutSemicolon(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
  end;

  { Блок переменных }
  TVariableDeclarations = class(TStatementList<TVariableDeclaration>)
  strict protected
    function ParseBreak: boolean; override;
    function AllowUnexpected: boolean; override;
    function AllowStatement(AStatement: TStatement): boolean; override;
  public
    function Aligned: boolean; override;
  end;

  { Курсор }
  TCursor = class(TPLSQLStatement)
  strict private
    _Cursor: TEpithet;
    _CursorName: TEpithet;
    _Params: TStatement;
    _Return: TEpithet;
    _ReturnType: TStatement;
    _Is: TEpithet;
    _Select: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Объявление исключения }
  TExceptionDeclaration = class(TPLSQLStatement)
  strict private
    _Name, _Exception: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Список операторов }
  TStatements = class(TStatementList<TStatement>)
  strict protected
    function ParseStatement(out AResult: TStatement): boolean; override;
    function ParseBreak: boolean; override;
    function AllowWhen: boolean; virtual;
  end;

  { Присваивание }
  TAssignment = class(TPLSQLStatement)
  strict private
    _Target: TStatement;
    _Assignment: TTerminal;
    _Expression: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Вызов процедуры }
  TProcedureCall = class(TPLSQLStatement)
  strict private
    _Call: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор return }
  TReturn = class(TPLSQLStatement)
  strict private
    _Return: TEpithet;
    _Value: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор null }
  TNull = class(TPLSQLStatement)
  strict private
    _Null: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор raise }
  TRaise = class(TPLSQLStatement)
  strict private
    _Raise: TEpithet;
    _ExceptionName: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Условный оператор }
  TIf = class(TPLSQLStatement)
  strict private
    _If: TEpithet;
    _Condition: TStatement;
    _Sections: TStatement;
    _EndIf: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Секция оператора if }
  TIfSection = class(TStatement)
  strict private
    _ThenOrElsifOrElse: TEpithet;
    _Condition: TStatement;
    _Then: TEpithet;
    _Statements: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
  end;

  { Секции оператора if }
  TIfSections = class(TStatementList<TIfSection>)
  strict protected
    function ParseBreak: boolean; override;
  end;

  { Секция оператора case }
  TCaseSection = class(TStatement)
  strict private
    _When, _Then, _Else: TEpithet;
    _Condition: TStatement;
    _Body: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор case }
  TCase = class(TStatementList<TCaseSection>)
  strict private
    _Case, _EndCase: TEpithet;
    _Condition: TStatement;
    _Semicolon: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
    function ParseBreak: boolean; override;
  end;

  { Оператор loop }
  TLoop = class(TStatements)
  strict private
    _Loop, _EndLoop: TEpithet;
    _Semicolon: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор for }
  TFor = class(TStatement)
  strict private
    _For: TEpithet;
    _Variable: TEpithet;
    _In: TEpithet;
    _Reverse: TEpithet;
    _Low: TStatement;
    _To: TTerminal;
    _High: TStatement;
    _Select: TStatement;
    _Cursor: TStatement;
    _Loop: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор while }
  TWhile = class(TStatement)
  strict private
    _While: TEpithet;
    _Condition: TStatement;
    _Loop: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор forall }
  TForAll = class(TPLSQLStatement)
  strict private
    _ForAll: TEpithet;
    _Variable: TEpithet;
    _In: TEpithet;
    _Low: TStatement;
    _To: TTerminal;
    _High: TStatement;
    _IndicesOrValues: TEpithet;
    _Of: TEpithet;
    _TableName: TStatement;
    _Save: TEpithet;
    _Exceptions: TEpithet;
    _DML: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор pipe row }
  TPipeRow = class(TPLSQLStatement)
  strict private
    _Pipe, _Row: TEpithet;
    _Arguments: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор open for }
  TOpenFor = class(TPLSQLStatement)
  strict private
    _Open, _For: TEpithet;
    _Cursor, _Select, _Using: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция using (для open for и execute immediate)}
  TUsing = class(TStatement)
  strict private
    _Using: TEpithet;
    _Params: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Параметр в операторе open for .. using }
  TUsingParam = class(TStatement)
  strict private
    _Accessor: TEpithet;
    _Value: TStatement;
    FreeAccessor: boolean;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    procedure BeforeDestruction; override;
  end;

  { Оператор fetch }
  TFetch = class(TPLSQLStatement)
  strict private
    _Fetch, _Into, _Limit: TEpithet;
    _Cursor, _Targets, _LimitValue: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор close }
  TClose = class(TPLSQLStatement)
  strict private
    _Close: TEpithet;
    _Cursor: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор exit }
  TExit = class(TPLSQLStatement)
  strict private
    _Exit, _When: TEpithet;
    _Condition: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор goto }
  TGoto = class(TPLSQLStatement)
  strict private
    _Goto, _Address: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор execute immediate }
  TExecuteImmediate = class(TPLSQLStatement)
  strict private
    _Execute, _Immediate, _Into: TEpithet;
    _Command, _IntoFields, _Using: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Блок обработки исключений }
  TExceptionHandler = class(TStatement)
  strict private
    _When: TEpithet;
    _Condition: TStatement;
    _Then: TEpithet;
    _Body: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Операторы секции exceptions }
  TExceptionStatements = class(TStatements)
  strict protected
    function ParseStatement(out AStatement: TStatement): boolean; override;
    function AllowWhen: boolean; override;
  end;

  { Декларация pragma }
  TPragma = class(TPLSQLStatement)
  strict private
    _Pragma: TEpithet;
    _Body: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Декларация типа }
  TType = class(TPLSQLStatement)
  strict private
    _Type, _Force, _AsIs: TEpithet;
    _TypeName, _Body: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
  end;

  { Декларация записи }
  TRecord = class(TStatement)
  strict private
    _Record: TEpithet;
    _Declarations: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function Aligned: boolean; override;
  end;

  { Декларация табличного типа }
  TPLSQLTable = class(TStatement)
  strict private
    _Table, _Of: TEpithet;
    _TypeRef: TStatement;
    _Index, _By: TEpithet;
    _IndexType: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Декларация ref cursor }
  TRefCursor = class(TStatement)
  strict private
    _Ref, _Cursor, _Returning: TEpithet;
    _TypeRef: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Объект object }
  TObject_ = class(TStatement)
  strict private
    _Object: TEpithet;
    _Body: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function Aligned: boolean; override;
  end;

  { Список членов класса }
  TObjectMemberList = class(TCommaList<TStatement>)
  strict protected
    function ParseStatement(out AResult: TStatement): boolean; override;
    function ParseBreak: boolean; override;
  end;

  { Декларация метода }
  TObjectMethodDeclaration = class(TSubroutineHeaderBase)
  end;

  { Обособленный комментарий, не привязанный к конструкции }
  TStandaloneComment = class(TStatement)
  strict private
    _Comment: TToken;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses Parser, Expressions, DML, DDL, Keywords;

{ TProgramBlock }

function TProgramBlock.ParseAfterEnd: TObject;
begin
  TQualifiedIdent.Parse(Self, Source, TStatement(Result));
end;

function TProgramBlock.InternalParse: boolean;
begin
  { Пытаемся распознать заголовок }
  Result := GetHeaderClass.Parse(Self, Source, _Header);
  if not Result then exit;
  { Распознаём декларации }
  TDeclarations.Parse(Self, Source, _Declarations);
  { Если нашли begin, распознаём операторы }
  _Begin := Keyword('begin');
  if Assigned(_Begin) then TStatements.Parse(Self, Source, _Operators);
  { Если нашли exception, распознаём обработчики исключений }
  _Exception := Keyword('exception');
  if Assigned(_Exception) then TExceptionStatements.Parse(Self, Source, _Handlers);
  { end }
  _End := Keyword('end');
  _AfterEnd := ParseAfterEnd;
  inherited;
end;

procedure TProgramBlock.InternalPrintSelf(APrinter: TPrinter);
var _AdditionalIndent, _AdditionalUndent: TObject;
begin
  if IndentBody
    then begin _AdditionalIndent := _Indent; _AdditionalUndent := _Undent; end
    else begin _AdditionalIndent := nil; _AdditionalUndent := nil; end;
  if _Header is TSubroutineHeaderBase then
    TSubroutineHeaderBase(_Header).IndentedAfterIs := Assigned(_Declarations);
  APrinter.PrintItems([_Header,    _IndentNextLine,
                                   _AdditionalIndent,
                                   _Declarations,   _UndentNextLine,
                       _Begin,     _IndentNextLine,
                                   _Operators,      _UndentNextLine,
                       _Exception, _IndentNextLine,
                                   _Handlers,       _UndentNextLine,
                       _End,       _AfterEnd,       _AdditionalUndent]);
  inherited;
end;

function TProgramBlock.IndentBody: boolean;
begin
  Result := false;
end;

{ TPackageHeader }

function TPackageHeader.InternalParse: boolean;
begin
  { Если распознали слово package, то распознали конструкцию }
  _Package := Keyword(['package', 'package body']);
  if not Assigned(_Package) then exit(false);
  Result := true;
  { Проверим название пакета }
  TQualifiedIdent.Parse(Self, Source, _Name);
  { Проверим наличие authid }
  _AuthId := Keyword(['authid definer', 'authid current_user']);
  { Проверим наличие is }
  _AsIs := Keyword(['is', 'as']);
  if Assigned(_AsIs) then _AsIs.CanReplace := true;
end;

procedure TPackageHeader.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Package, _Name, _AuthId, _AsIs]);
  APrinter.NextLine;
end;

function TPackageHeader.StatementName: string;
begin
  Result := Concat([_Package, _Name]);
end;

{ TPackage }

function TPackage.StatementName: string;
begin
  Result := Header.StatementName;
end;

function TPackage.GetHeaderClass: TStatementClass;
begin
  Result := TPackageHeader;
end;

{ TSubroutineHeaderBase }

function TSubroutineHeaderBase.InternalParse: boolean;
begin
  _Map := Keyword('map');
  _FunctionType := Keyword(['member', 'static', 'constructor']);
  { Проверим procedure/function }
  _ProcedureOrFunction := Keyword(['procedure', 'function']);
  if not Assigned(_ProcedureOrFunction) then exit(false);
  Result := true;
  { Название процедуры и параметры }
  TQualifiedIdent.Parse(Self, Source, _Name);
  if not Assigned(_Name) then exit;
  TParamsDeclaration.Parse(Self, Source, _Params);
  { Возвращаемое значение }
  _Return := Keyword('return');
  _SelfAsResult := Keyword('self as result');
  if not Assigned(_SelfAsResult) then TTypeRef.Parse(Self, Source, _ReturnType);
  { Признаки }
  _Deterministic := Keyword('deterministic');
  _Pipelined := Keyword('pipelined');
end;

procedure TSubroutineHeaderBase.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Map, _FunctionType, _ProcedureOrFunction, _Name]);
  APrinter.Indent;
  {$B+}
  FIndentedBeforeIs := APrinter.NextLineIf([_Params]) or
                       APrinter.NextLineIf([_Return, _SelfAsResult, _ReturnType]) or
                       APrinter.NextLineIf([_Deterministic]) or
                       APrinter.NextLineIf([_Pipelined]);
  {$B-}
  APrinter.Undent;
end;

function TSubroutineHeaderBase.StatementName: string;
begin
  Result := Concat([_FunctionType, _ProcedureOrFunction, _Name]);
end;

{ TParamDeclaration }

function TParamDeclaration.InternalParse: boolean;
begin
  _ParamName := Identifier;
  _InOut  := Keyword(['in', 'out', 'in out']);
  _Nocopy := Keyword('nocopy');
  TTypeRef.Parse(Self, Source, _ParamType);
  if not Assigned(_ParamName) and not Assigned(_InOut) and not Assigned(_Nocopy) and not Assigned(_ParamType) then exit(false);
  _Assignment := Terminal(':=');
  if not Assigned(_Assignment) then _Assignment := Keyword('default');
  if Assigned(_Assignment) then
  begin
    _Assignment.CanReplace := true;
    TParser.ParseExpression(Self, Source, _DefaultValue);
  end;
  Result := true;
end;

function TParamDeclaration.StatementName: string;
begin
  Result := Concat([_ParamName]);
end;

procedure TParamDeclaration.InternalPrintSelf(APrinter: TPrinter);
begin
  FreeAccessor := Settings.AddInAccessSpecificator and not Assigned(_InOut);
  if FreeAccessor then _InOut := TEpithet.Create('in', -1, -1);
  APrinter.StartRuler(Settings.AlignVariables);
  APrinter.PrintRulerItem('name', _ParamName);
  APrinter.PrintRulerItems('modifiers', [_InOut, _Nocopy]);
  APrinter.PrintRulerItem('type', _ParamType);
  APrinter.PrintRulerItem('assignment', _Assignment);
  APrinter.PrintRulerItem('value', _DefaultValue);
end;

procedure TParamDeclaration.BeforeDestruction;
begin
  if FreeAccessor then FreeAndNil(_InOut);
  inherited;
end;

{ TParamsDeclaration }

function TParamsDeclaration.Aligned: boolean;
begin
  Result := true;
end;

function TParamsDeclaration.InternalParse: boolean;
begin
  _OpenBracket := Terminal('(');
  if not Assigned(_OpenBracket) then exit(false);
  Result := inherited InternalParse;
  if Result then _CloseBracket := Terminal(')');
end;

function TParamsDeclaration.ParseBreak: boolean;
begin
  Result := Any([Terminal(')')]);
end;

procedure TParamsDeclaration.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_OpenBracket, _IndentNextLine]);
  inherited;
  APrinter.PrintItems([_UndentNextLine, _CloseBracket]);
end;

{ TDeclarations }

function TDeclarations.ParseStatement(out AResult: TStatement): boolean;
begin
  Result := TParser.ParseDeclaration(Self, Source, AResult);
end;

function TDeclarations.ParseBreak: boolean;
begin
  Result := Any([Keyword(['begin', 'end', 'create', 'after each row',
                          'after statement', 'before each row',
                          'before statement', 'instead of each row']),
                 Terminal('/')]);
end;

function TDeclarations.EmptyLineBefore: boolean;
begin
  Result := Parent is TPackage;
end;

function TDeclarations.EmptyLineAfter: boolean;
begin
  Result := Parent is TPackage;
end;

function TDeclarations.EmptyLineInside: boolean;
begin
  Result := Parent is TPackage;
end;

{ TVariableDeclaration }

function TVariableDeclaration.InternalParse: boolean;
begin
  { Если конструкция начинается с идентификатора, будем считать объявление распознанным }
  _Name := Identifier;
  if not Assigned(_Name) then exit(false);
  _Constant := Keyword('constant');
  { Теперь тип и значение }
  if not TTypeRef.Parse(Self, Source, _Type) then exit(false);
  { Осталось значение и т. п. }
  _Assignment := Terminal(':=');
  if not Assigned(_Assignment) then _Assignment := Keyword('default');
  if Assigned(_Assignment) then
  begin
    _Assignment.CanReplace := true;
    TParser.ParseExpression(Self, Source, _Value);
  end;
  { Слишком многие проблемные конструкции выглядят как пара идущих подряд
    идентификаторов и распознаются как variable declaration. Поэтому
    потребуем наличия либо точки с запятой, либо дополнительных конструкций }
  Result := inherited or Assigned(_Constant) or Assigned(_Assignment) or
            (Assigned(_Type) and not TTypeRef(_Type).IsSimpleIdent) or
            (Parent is TCommaList<TVariableDeclaration>) or SkipSemicolonCheck;
end;

function TVariableDeclaration.StatementName: string;
begin
  Result := Concat([_Name]);
end;

procedure TVariableDeclaration.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.StartRuler(Settings.AlignVariables);
  APrinter.PrintRulerItem('name', _Name);
  APrinter.PrintRulerItem('constant', _Constant);
  APrinter.PrintRulerItem('type', _Type);
  APrinter.PrintRulerItem('assignment', _Assignment);
  APrinter.PrintRulerItem('value', _Value);
  inherited;
end;

class function TVariableDeclaration.WithoutSemicolon(
  AParent: TStatement;
  ASource: TBufferedStream<TToken>;
  out AResult: TStatement): boolean;
begin
  try
    SkipSemicolonCheck := true;
    Result := Parse(AParent, ASource, AResult);
  finally
    SkipSemicolonCheck := false;
  end;
end;

{ TVariableDeclarations }

function TVariableDeclarations.ParseBreak: boolean;
begin
  Result := false;
end;

function TVariableDeclarations.AllowUnexpected: boolean;
begin
  Result := false;
end;

function TVariableDeclarations.Aligned: boolean;
begin
  Result := true;
end;

function TVariableDeclarations.AllowStatement(AStatement: TStatement): boolean;
begin
  Result := (Count = 0) or (AStatement.FirstToken.Line - Item(Count - 1).FirstToken.Line < 2);
end;

{ TProcedureCall }

function TProcedureCall.InternalParse: boolean;
begin
  Result := TQualifiedIndexedIdent.Parse(Self, Source, _Call);
  { Если результат - простой идентификатор, потребуем наличия точки с запятой,
    иначе вероятно ложное распознавание }
  if Result then
    Result := inherited or not (_Call as TQualifiedIndexedIdent).IsSimpleIdent;
end;

procedure TProcedureCall.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Call);
  inherited;
end;

{ TAssignment }

function TAssignment.InternalParse: boolean;
begin
  TQualifiedIndexedIdent.Parse(Self, Source, _Target);
  _Assignment := Terminal(':=');
  Result := Assigned(_Target) and Assigned(_Assignment);
  if not Result then exit;
  TParser.ParseExpression(Self, Source, _Expression);
  inherited;
end;

procedure TAssignment.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Target, _Assignment, _Expression]);
  inherited;
end;

{ TReturn }

function TReturn.InternalParse: boolean;
begin
  _Return := Keyword('return');
  if not Assigned(_Return) then exit(false);
  Result := true;
  TParser.ParseExpression(Self, Source, _Value);
  inherited;
end;

procedure TReturn.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Return, _Value]);
  inherited;
end;

{ TSubroutine }

function TSubroutine.GetHeaderClass: TStatementClass;
begin
  Result := TSubroutineHeader;
end;

function TSubroutine.StatementName: string;
begin
  Result := Header.StatementName;
end;

{ TSubroutineHeader }

function TSubroutineHeader.InternalParse: boolean;
begin
  Result := inherited InternalParse;
  if Result then _Is := Keyword(['is', 'as']);
  if Assigned(_Is) then _Is.CanReplace := true;
end;

procedure TSubroutineHeader.InternalPrintSelf(APrinter: TPrinter);
var Indented: boolean;
begin
  inherited;
  Indented := IndentedBeforeIs and not IndentedAfterIs;
  if Indented then APrinter.Indent;
  APrinter.PrintItems([_NextLine, _Is]);
  if Indented then APrinter.Undent;
end;

function TSubroutineHeader.Name: string;
begin
  Result := StatementType;
end;

{ TSubroutineForwardDeclaration }

function TSubroutineForwardDeclaration.InternalParse: boolean;
begin
  Result := inherited InternalParse;
  if Result then _Semicolon := Terminal(';');
  Result := Result and Assigned(_Semicolon);
end;

procedure TSubroutineForwardDeclaration.InternalPrintSelf(APrinter: TPrinter);
begin
  inherited;
  APrinter.PrintItem(_Semicolon);
end;

{ TOperators }

function TStatements.ParseStatement(out AResult: TStatement): boolean;
begin
  Result := TParser.ParseDML(Self, Source, AResult) or
            TParser.ParsePLSQL(Self, Source, AResult);
end;

function TStatements.ParseBreak: boolean;
begin
  Result := Any([Keyword(['end', 'end if', 'end loop', 'end case', 'exception', 'else', 'elsif'])]);
  if not Result and not AllowWhen then Result := Any([Keyword('when')]);
end;

function TStatements.AllowWhen: boolean;
begin
  Result := false;
end;

{ TIf }

function TIf.InternalParse: boolean;
begin
  _If := Keyword('if');
  if not Assigned(_If) then exit(false);
  TParser.ParseExpression(Self, Source, _Condition);
  TIfSections.Parse(Self, Source, _Sections);
  _EndIf := Keyword('end if');
  Result := true;
  inherited;
end;

procedure TIf.InternalPrintSelf(APrinter: TPrinter);
var _NL: TObject;
begin
  { При многострочном условии переносим then на следующую строку }
  if (_Condition is TExpression) and TExpression(_Condition).IsMultiLine
    then _NL := _NextLine
    else _NL := nil;
  { Собственно печать }
  APrinter.PrintItems([_If, _Condition, _NL, _Sections, _NextLine, _EndIf]);
  inherited;
end;

{ TAnonymousHeader }

function TAnonymousHeader.InternalParse: boolean;
var P: TMark;
begin
  _Declare := Keyword('declare');
  if Assigned(_Declare) then exit(true);
  P := Source.Mark;
  Result := Assigned(Keyword('begin'));
  Source.Restore(P);
end;

procedure TAnonymousHeader.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Declare);
  APrinter.NextLine;
end;

{ TAnonymousBlock }

function TAnonymousBlock.GetHeaderClass: TStatementClass;
begin
  Result := TAnonymousHeader;
end;

{ TExceptionHandler }

function TExceptionHandler.InternalParse: boolean;
begin
  _When := Keyword('when');
  Result := Assigned(_When) and TParser.ParseExpression(Self, Source, _Condition);
  if not Result then exit;
  _Then := Keyword('then');
  TStatements.Parse(Self, Source, _Body);
end;

procedure TExceptionHandler.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_When, _Condition, _Then]);
  APrinter.PrintIndented(_Body);
end;

{ TExceptionStatements }

function TExceptionStatements.ParseStatement(out AStatement: TStatement): boolean;
begin
  Result := TExceptionHandler.Parse(Self, Source, AStatement) or inherited ParseStatement(AStatement);
end;

function TExceptionStatements.AllowWhen: boolean;
begin
  Result := true;
end;

{ TNull }

function TNull.InternalParse: boolean;
begin
  _Null := Keyword('null');
  Result := Assigned(_Null);
  inherited;
end;

procedure TNull.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Null);
  inherited;
end;

{ TRaise }

function TRaise.InternalParse: boolean;
begin
  _Raise := Keyword('raise');
  TQualifiedIdent.Parse(Self, Source, _ExceptionName);
  Result := Assigned(_Raise);
  inherited;
end;

procedure TRaise.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Raise, _ExceptionName]);
  inherited;
end;

{ TExceptionDeclaration }

function TExceptionDeclaration.InternalParse: boolean;
begin
  _Name := Identifier;
  _Exception := Keyword('exception');
  Result := Assigned(_Name) and Assigned(_Exception);
  inherited;
end;

procedure TExceptionDeclaration.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Name, _Exception]);
  inherited;
end;

{ TIfSection }

function TIfSection.InternalParse: boolean;
begin
  _ThenOrElsifOrElse := Keyword(['then', 'elsif', 'else']);
  if not Assigned(_ThenOrElsifOrElse) then exit(false);
  if _ThenOrElsifOrElse.Value = 'elsif' then
  begin
    TParser.ParseExpression(Self, Source, _Condition);
    _Then := Keyword('then');
  end;
  TStatements.Parse(Self, Source, _Statements);
  Result := true;
end;

function TIfSection.StatementName: string;
begin
  Result := Concat([_ThenOrElsifOrElse]);
end;

procedure TIfSection.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_ThenOrElsifOrElse, _Condition, _Then, _IndentNextLine,
                                           _Statements, _UndentNextLine]);
end;

{ TIfSections }

function TIfSections.ParseBreak: boolean;
begin
  Result := Any([Keyword(['end if', 'end'])]);
end;

{ TOpenFor }

function TOpenFor.InternalParse: boolean;
begin
  _Open := Keyword('open');
  if not Assigned(_Open) then exit(false);
  TQualifiedIdent.Parse(Self, Source, _Cursor);
  _For := Keyword('for');
  if not TSelect.Parse(Self, Source, _Select) then TParser.ParseExpression(Self, Source, _Select);
  TUsing.Parse(Self, Source, _Using);
  inherited;
  Result := true;
end;

procedure TOpenFor.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Open,  _IndentNextLine,
                               _Cursor, _UndentNextLine,
                       _For,   _IndentNextLine,
                               _Select, _UndentNextLine,
                       _Using]);
  inherited;
end;

{ TUsing }

function TUsing.InternalParse: boolean;
begin
  _Using := Keyword('using');
  Result := Assigned(_Using);
  if Result then TAligned<TCommaList<TUsingParam>>.Parse(Self, Source, _Params);
end;

procedure TUsing.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Using, _IndentNextLine, _Params, _Undent]);
end;

{ TUsingParam }

function TUsingParam.InternalParse: boolean;
begin
  _Accessor := Keyword(['in', 'out', 'in out']);
  TParser.ParseExpression(Self, Source, _Value);
  Result := Assigned(_Accessor) or Assigned(_Value);
end;

procedure TUsingParam.InternalPrintSelf(APrinter: TPrinter);
begin
  FreeAccessor := Settings.AddInAccessSpecificator and not Assigned(_Accessor);
  if FreeAccessor then _Accessor := TEpithet.Create('in', -1, -1);
  APrinter.StartRuler(Settings.AlignVariables);
  APrinter.PrintRulerItem('accessor', _Accessor);
  APrinter.PrintRulerItem('value', _Value);
end;

procedure TUsingParam.BeforeDestruction;
begin
  if FreeAccessor then FreeAndNil(_Accessor);
  inherited;
end;

{ TPragma }

function TPragma.InternalParse: boolean;
begin
  _Pragma := Keyword('pragma');
  if not Assigned(_Pragma) then exit(false);
  TQualifiedIndexedIdent.Parse(Self, Source, _Body);
  inherited;
  Result := true;
end;

procedure TPragma.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Pragma);
  APrinter.SupressNextLine(true);
  APrinter.PrintItem(_Body);
  APrinter.SupressNextLine(false);
  inherited;
end;

{ TLoop }

function TLoop.InternalParse: boolean;
begin
  _Loop := Keyword('loop');
  if not Assigned(_Loop) then exit(false);
  inherited InternalParse;
  _EndLoop := Keyword('end loop');
  _Semicolon := Terminal(';');
  Result := true;
end;

procedure TLoop.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Loop, _IndentNextLine]);
  inherited;
  APrinter.PrintItems([_UndentNextLine, _EndLoop, _Semicolon]);
end;

{ TFor }

function TFor.InternalParse: boolean;
var P: TMark;
begin
  _For := Keyword('for');
  if not Assigned(_For) then exit(false);
  _Variable := Identifier;
  _In := Keyword('in');
  _Reverse := Keyword('reverse');
  if not TInnerSelect.Parse(Self, Source, _Select) then
  begin
    P := Source.Mark;
    TParser.ParseExpression(Self, Source, _Low);
    if Assigned(_Low) then _To := Terminal('..');
    if Assigned(_To)  then TParser.ParseExpression(Self, Source, _High);
    if not Assigned(_To) then
    begin
      _Low := nil;
      _High := nil;
      Source.Restore(P);
      TQualifiedIndexedIdent.Parse(Self, Source, _Cursor);
    end;
  end;
  TLoop.Parse(Self, Source, _Loop);
  Result := true;
end;

procedure TFor.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_For, _Variable, _In, _Reverse]);
  if Assigned(_Select) then
    begin
      APrinter.PrintIndented(_Select);
      APrinter.NextLine;
    end
  else
    APrinter.PrintItems([_Cursor, _Low, _To, _High]);
  APrinter.PrintItem(_Loop);
end;

{ TWhile }

function TWhile.InternalParse: boolean;
begin
  _While := Keyword('while');
  if not Assigned(_While) then exit(false);
  TParser.ParseExpression(Self, Source, _Condition);
  TLoop.Parse(Self, Source, _Loop);
  Result := true;
end;

procedure TWhile.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_While, _Condition, _Loop]);
end;

{ TType }

function TType.InternalParse: boolean;
begin
  _Type := Keyword('type');
  if not Assigned(_Type) then exit(false);
  TQualifiedIdent.Parse(Self, Source, _TypeName);
  _Force := Keyword('force');
  _AsIs := Keyword(['as', 'is']);
  if Assigned(_AsIs) then _AsIs.CanReplace := true;
  TParser.ParseType(Self, Source, _Body);
  inherited;
  Result := true;
end;

procedure TType.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Type, _TypeName, _Force, _AsIs, _Body]);
  inherited;
end;

function TType.StatementName: string;
begin
  Result := Concat([_Type, _TypeName]);
end;

{ TRecord }

function TRecord.Aligned: boolean;
begin
  Result := true;
end;

function TRecord.InternalParse: boolean;
begin
  _Record := Keyword('record');
  Result  := Assigned(_Record) and
             TBracketedStatement<TCommaList<TVariableDeclaration>>.Parse(Self, Source, _Declarations);
end;

procedure TRecord.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Record);
  APrinter.PrintIndented(_Declarations);
end;

{ TTable }

function TPLSQLTable.InternalParse: boolean;
begin
  _Table := Keyword('table');
  if not Assigned(_Table) then exit(false);
  _Of := Keyword('of');
  TTypeRef.Parse(Self, Source, _TypeRef);
  _Index := Keyword('index');
  _By := Keyword('by');
  TTypeRef.Parse(Self, Source, _IndexType);
  Result := true;
end;

procedure TPLSQLTable.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Table, _Of, _TypeRef, _Index, _By, _IndexType]);
end;

{ TRefCursor }

function TRefCursor.InternalParse: boolean;
begin
  _Ref := Keyword('ref');
  if not Assigned(_Ref) then exit(false);
  _Cursor := Keyword('cursor');
  _Returning := Keyword('returning');
  if Assigned(_Returning) then TTypeRef.Parse(Self, Source, _TypeRef);
  Result := true;
end;

procedure TRefCursor.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Ref, _Cursor, _Returning, _TypeRef]);
end;

{ TObject_ }

function TObject_.Aligned: boolean;
begin
  Result := true;
end;

function TObject_.InternalParse: boolean;
begin
  _Object := Keyword('object');
  if not Assigned(_Object) then exit(false);
  TOptionalBracketedStatement<TObjectMemberList>.Parse(Self, Source, _Body);
  Result := true;
end;

procedure TObject_.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Object, _IndentNextLine, _Body, _Undent]);
end;

{ TObjectMemberList }

function TObjectMemberList.ParseStatement(out AResult: TStatement): boolean;
begin
  Result := TObjectMethodDeclaration.Parse(Self, Source, AResult) or
            TVariableDeclaration.WithoutSemicolon(Self, Source, AResult);
end;

function TObjectMemberList.ParseBreak: boolean;
begin
  Result := Any([Terminal([';', ')'])]);
end;

{ TForall }

function TForAll.InternalParse: boolean;
begin
  _ForAll := Keyword('forall');
  if not Assigned(_ForAll) then exit(false);
  _Variable := Identifier;
  _In := Keyword('in');
  _IndicesOrValues := Keyword(['indices', 'values']);
  if Assigned(_IndicesOrValues) then
    begin
      _Of := Keyword('of');
      TQualifiedIndexedIdent.Parse(Self, Source, _TableName);
    end
  else
    begin
      TParser.ParseExpression(Self, Source, _Low);
      _To := Terminal('..');
      TParser.ParseExpression(Self, Source, _High);
    end;
  _Save := Keyword('save');
  _Exceptions := Keyword('exceptions');
  if not TParser.ParseDML(Self, Source, _DML) then TParser.ParsePLSQL(Self, Source, _DML);
  inherited;
  Result := true;
end;

procedure TForall.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_ForAll, _Variable, _In, _IndicesOrValues, _Low, _Of, _To, _TableName, _High, _Save, _Exceptions]);
  APrinter.PrintIndented(_DML);
  inherited;
end;

{ TCaseSection }

function TCaseSection.InternalParse: boolean;
begin
  _When := Keyword('when');
  if Assigned(_When) then
    begin
      TParser.ParseExpression(Self, Source, _Condition);
      _Then := Keyword('then');
    end
  else
    _Else := Keyword('else');
  if not Assigned(_When) and not Assigned(_Else) then exit(false);
  TStatements.Parse(Self, Source, _Body);
  Result := true;
end;

procedure TCaseSection.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_When, _Condition, _Then, _Else]);
  APrinter.PrintIndented(_Body);
end;

{ TCase }

function TCase.InternalParse: boolean;
begin
  _Case := Keyword('case');
  if not Assigned(_Case) then exit(false);
  TParser.ParseExpression(Self, Source, _Condition);
  inherited;
  _EndCase := Keyword('end case');
  _Semicolon := Terminal(';');
  Result := true;
end;

function TCase.ParseBreak: boolean;
begin
  Result := Any([Keyword(['end case', 'end'])]);
end;

procedure TCase.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Case, _Condition, _IndentNextLine]);
  inherited;
  APrinter.PrintItems([_UndentNextLine, _EndCase, _Semicolon]);
end;

{ TCursor }

function TCursor.InternalParse: boolean;
begin
  _Cursor := Keyword('cursor');
  if not Assigned(_Cursor) then exit(false);
  _CursorName := Identifier;
  TParamsDeclaration.Parse(Self, Source, _Params);
  _Return := Keyword('return');
  if Assigned(_Return) then TTypeRef.Parse(Self, Source, _ReturnType);
  _Is := Keyword('is');
  TSelect.Parse(Self, Source, _Select);
  Result := true;
end;

procedure TCursor.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Cursor, _CursorName, _IndentNextLine,
                                _Params, _NextLine,
                                _Return, _ReturnType, _UndentNextLine,
                       _Is,     _IndentNextLine,
                                _Select, _UndentNextLine]);
end;

{ TPipeRow }

function TPipeRow.InternalParse: boolean;
begin
  _Pipe := Keyword('pipe');
  _Row  := Keyword('row');
  Result := Assigned(_Pipe) or Assigned(_Row);
  if Result then TArguments.Parse(Self, Source, _Arguments);
  inherited;
end;

procedure TPipeRow.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Pipe, _Row, _Arguments]);
  inherited;
end;

{ TFetch }

function TFetch.InternalParse: boolean;
begin
  _Fetch := Keyword('fetch');
  if not Assigned(_Fetch) then exit(false);
  TQualifiedIdent.Parse(Self, Source, _Cursor);
  _Into := Keyword(['into', 'bulk collect into']);
  if Assigned(_Into) then TIdentFields.Parse(Self, Source, _Targets);
  _Limit := Keyword('limit');
  if Assigned(_Limit) then TParser.ParseExpression(Self, Source, _LimitValue);
  inherited;
  Result := true;
end;

procedure TFetch.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Fetch, _IndentNextLine,
                               _Cursor, _UndentNextLine,
                       _Into,  _IndentNextLine,
                               _Targets, _UndentNextLine,
                       _Limit, _IndentNextLine,
                               _LimitValue, _Undent]);
  inherited;
end;

{ TClose }

function TClose.InternalParse: boolean;
begin
  _Close := Keyword('close');
  if not Assigned(_Close) then exit(false);
  TQualifiedIndexedIdent.Parse(Self, Source, _Cursor);
  inherited;
  Result := true;
end;

procedure TClose.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Close, _Cursor]);
  inherited;
end;

{ TExit }

function TExit.InternalParse: boolean;
begin
  _Exit := Keyword('exit');
  if not Assigned(_Exit) then exit(false);
  _When := Keyword('when');
  if Assigned(_When) then TParser.ParseExpression(Self, Source, _Condition);
  inherited;
  Result := true;
end;

procedure TExit.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Exit, _When, _Condition]);
  inherited;
end;

{ TExecuteImmediate }

function TExecuteImmediate.InternalParse: boolean;
begin
  _Execute := Keyword('execute');
  if not Assigned(_Execute) then exit(false);
  _Immediate := Keyword('immediate');
  TParser.ParseExpression(Self, Source, _Command);
  _Into := Keyword('into');
  if Assigned(_Into) then TIdentFields.Parse(Self, Source, _IntoFields);
  TUsing.Parse(Self, Source, _Using);
  inherited;
  Result := true;
end;

procedure TExecuteImmediate.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Execute, _Immediate,  _IndentNextLine,
                                 _Command,    _UndentNextLine,
                       _Into,    _IndentNextLine,
                                 _IntoFields, _UndentNextLine,
                       _Using]);
  inherited;
end;

{ TStandaloneComment }

function TStandaloneComment.InternalParse: boolean;
var T: TToken;
begin
  T := NextToken;
  Result := T is Tokens.TComment;
  if Result then _Comment := T;
end;

procedure TStandaloneComment.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_NextLine, _NextLine, _Comment, _NextLine, _NextLine]);
end;

{ TLabel }

function TLabel.InternalParse: boolean;
begin
  Source.Mark;
  _Name := NextToken;
  Result := _Name is Tokens.TLabel;
  if not Result then Source.Restore;
end;

procedure TLabel.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Name);
end;

{ TGoto }

function TGoto.InternalParse: boolean;
begin
  _Goto := Keyword('goto');
  Result := Assigned(_Goto);
  if Result then _Address := Identifier;
  inherited;
end;

procedure TGoto.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Goto, _Address]);
  inherited;
end;

initialization
  Keywords.RegisterOrphan(TPLSQLStatement);
  Keywords.RegisterKeywords(TPLSQLStatement, ['as', 'begin', 'end', 'exception',
    'function', 'is', 'procedure', 'using']);
  Keywords.RegisterKeywords(TFetch, ['limit']);

end.
