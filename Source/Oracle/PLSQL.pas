////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                      Синтаксические конструкции PL/SQL                     //
//                                                                            //
//               Copyright(c) 2019-2024 by Sanders the Softwarer              //
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
  Printer, Parser, ProgramBlock, Subroutine;

type

  { Оператор PL/SQL с зарегистрированными в этом контексте дополнительными ключевыми словами }
  TPLSQLStatement = class(TTopStatement);

  { Анонимный блок }
  TAnonymousBlock = class(TProgramBlock);

  { Блок деклараций }
  TOracleDeclarations = class(TDeclarations)
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
    function Aligned: TAlignMode; override;
  public
    class function Priority: integer; override;
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

  { Вызов процедуры }
  TProcedureCall = class(TPLSQLStatement)
  strict private
    _Call: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    class function Priority: integer; override;
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

  { Оператор pipe row }
  TPipeRow = class(TPLSQLStatement)
  strict private
    _Pipe, _Row: TEpithet;
    _Arguments: TStatement;
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
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Параметры в операторе open for .. using }
  TUsingParams = class(TCommaList<TUsingParam>)
  strict protected
    function Aligned: TAlignMode; override;
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

  { TODO : Разобраться, нужен ли }
  { Секция when }
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

  { TODO : Разобраться, нужен ли }
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
  end;

  { Поля записи }
  TRecordFields = class(TCommaList<TVariableDeclaration>)
  strict protected
    function Aligned: TAlignMode; override;
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
    _Ref, _Cursor, _Return: TEpithet;
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
  end;

  { Список членов класса }
  TObjectMemberList = class(TCommaList<TStatement>)
  strict protected
    function ParseStatement(out AResult: TStatement): boolean; override;
    function ParseBreak: boolean; override;
  end;

  { Метод класса }
  TObjectMethodDeclaration = class(TSubroutineHeaderBase);

  { Обособленный комментарий, не привязанный к конструкции }
  TStandaloneComment = class(TStatement)
  strict private
    _Comment: TToken;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

{ Парсер для PL/SQL }
function PLSQLParser: TParserInfo;

{ Парсер для типов }
function TypeParser: TParserInfo;

{ Парсер для деклараций }
function DeclarationParser: TParserInfo;

implementation

uses Expressions, DML, DDL, Keywords, Select, Label_, Goto_, Exit_,
  OpenFor, ForAll, Assignment, DML_Commons, SubType, ExecuteImmediate,
  OracleCore, Package;

{ Парсер для PL/SQL }
function PLSQLParser: TParserInfo;
begin
  Result := TParserInfo.InstanceFor('Oracle.Statement');
end;

{ Парсер для типов }
function TypeParser: TParserInfo;
begin
  Result := TParserInfo.InstanceFor('Oracle.Types');
end;

{ Парсер для деклараций }
function DeclarationParser: TParserInfo;
begin
  Result := TParserInfo.InstanceFor('Oracle.Declaration');
end;

{ TOracleDeclarations }

function TOracleDeclarations.EmptyLineBefore: boolean;
begin
  Result := Assigned(Parent) and (Parent.Parent is TPackage);
end;

function TOracleDeclarations.EmptyLineAfter: boolean;
begin
  Result := Assigned(Parent) and (Parent.Parent is TPackage);
end;

function TOracleDeclarations.EmptyLineInside: boolean;
begin
  Result := Assigned(Parent) and (Parent.Parent is TPackage);
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
    TExpression.Parse(Self, Source, _Value);
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
  APrinter.PrintRulerItems('name', [_Name]);
  APrinter.PrintRulerItems('constant', [_Constant]);
  APrinter.PrintRulerItems('type', [_Type]);
  APrinter.PrintRulerItems('assignment', [_Assignment]);
  APrinter.PrintRulerItems('value', [_Value]);
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

class function TVariableDeclarations.Priority: integer;
begin
  Result := LOWEST_PRIORITY;
end;

function TVariableDeclarations.ParseBreak: boolean;
begin
  Result := false;
end;

function TVariableDeclarations.AllowUnexpected: boolean;
begin
  Result := false;
end;

function TVariableDeclarations.Aligned: TAlignMode;
begin
  Result := AlignMode(Settings.AlignVariables);
end;

function TVariableDeclarations.AllowStatement(AStatement: TStatement): boolean;
begin
  Result := (Count = 0) or (AStatement.FirstToken.Line - Item(Count - 1).FirstToken.Line < 2);
end;

{ TProcedureCall }

class function TProcedureCall.Priority: integer;
begin
  Result := LOWER_PRIORITY;
end;

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

{ TReturn }

function TReturn.InternalParse: boolean;
begin
  _Return := Keyword('return');
  if not Assigned(_Return) then exit(false);
  Result := true;
  TExpression.Parse(Self, Source, _Value);
  inherited;
end;

procedure TReturn.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Return, _Value]);
  inherited;
end;

{ TStatements }

function TStatements.ParseStatement(out AResult: TStatement): boolean;
begin
  Result := TParser.Parse(Source, Settings, PLSQLParser, Self, AResult);
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
  TExpression.Parse(Self, Source, _Condition);
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
  APrinter.PrintItems([_If, _Indent, _Condition, _Undent, _NL, _Sections, _NextLine, _EndIf]);
  inherited;
end;

{ TExceptionHandler }

function TExceptionHandler.InternalParse: boolean;
begin
  _When := Keyword('when');
  Result := Assigned(_When) and TSingleLine<TExpression>.Parse(Self, Source, _Condition);
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
    TExpression.Parse(Self, Source, _Condition);
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

{ TUsing }

function TUsing.InternalParse: boolean;
begin
  _Using := Keyword('using');
  Result := Assigned(_Using);
  if Result then TUsingParams.Parse(Self, Source, _Params);
end;

procedure TUsing.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Using, _IndentNextLine, _Params, _Undent]);
end;

{ TUsingParam }

function TUsingParam.InternalParse: boolean;
begin
  _Accessor := Keyword(['in', 'out', 'in out']);
  TExpression.Parse(Self, Source, _Value);
  Result := Assigned(_Accessor) or Assigned(_Value);
end;

procedure TUsingParam.InternalPrintSelf(APrinter: TPrinter);
begin
  if Settings.AddInAccessSpecificator and not Assigned(_Accessor) then
  begin
    _Accessor := TEpithet.Create('in', -1, -1);
    AddToFreeList(_Accessor);
  end;
  APrinter.PrintRulerItems('accessor', [_Accessor]);
  APrinter.PrintRulerItems('value', [_Value]);
end;

{ TUsingParams }

function TUsingParams.Aligned: TAlignMode;
begin
  Result := AlignMode(Settings.AlignVariables);
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
  if not TBracketedStatement<TSelect>.Parse(Self, Source, _Select) then
  begin
    P := Source.Mark;
    TExpression.Parse(Self, Source, _Low);
    if Assigned(_Low) then _To := Terminal('..');
    if Assigned(_To)  then TExpression.Parse(Self, Source, _High);
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
  TExpression.Parse(Self, Source, _Condition);
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
  Result := true;
  _Type := Keyword('type');
  if not Assigned(_Type) then exit(false);
  TQualifiedIdent.Parse(Self, Source, _TypeName);
  _Force := Keyword('force');
  _AsIs := Keyword(['as', 'is']);
  if Assigned(_AsIs) then _AsIs.CanReplace := true;
  TParser.Parse(Source, Settings, TypeParser, Self, _Body);
  inherited;
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

function TRecord.InternalParse: boolean;
begin
  _Record := Keyword('record');
  Result  := Assigned(_Record) and
             TBracketedStatement<TRecordFields>.Parse(Self, Source, _Declarations);
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
  _Return := Keyword('return');
  if Assigned(_Return) then TTypeRef.Parse(Self, Source, _TypeRef);
  Result := true;
end;

procedure TRefCursor.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Ref, _Cursor, _Return, _TypeRef]);
end;

{ TObject_ }

function TObject_.InternalParse: boolean;
begin
  _Object := Keyword('object');
  if not Assigned(_Object) then exit(false);
  TBracketedStatement<TObjectMemberList>.Parse(Self, Source, _Body);
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

{ TCaseSection }

function TCaseSection.InternalParse: boolean;
begin
  _When := Keyword('when');
  if Assigned(_When) then
    begin
      TExpression.Parse(Self, Source, _Condition);
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
  TExpression.Parse(Self, Source, _Condition);
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
  if Result then TBracketedArguments.Parse(Self, Source, _Arguments);
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
  if Assigned(_Limit) then TExpression.Parse(Self, Source, _LimitValue);
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

{ TRecordFields }

function TRecordFields.Aligned: TAlignMode;
begin
  Result := AlignMode(Settings.AlignVariables);
end;

initialization
  Keywords.RegisterOrphan(TPLSQLStatement);
  Keywords.RegisterKeywords(TPLSQLStatement, ['as', 'begin', 'end', 'exception',
    'function', 'is', 'procedure', 'using']);
  Keywords.RegisterKeywords(TFetch, ['limit']);
  { Зарегистрируем конструкции PL/SQL }
  with PLSQLParser do
  begin
    Add(TPragma);
    Add(TReturn);
    Add(TNull);
    Add(TRaise);
    Add(TIf);
    Add(TCase);
    Add(TLoop);
    Add(TFor);
    Add(TWhile);
    Add(TForAll);
    Add(TOpenFor);
    Add(TFetch);
    Add(TExit);
    Add(TPipeRow);
    Add(TClose);
    Add(TAnonymousBlock);
    Add(TAssignment);
    Add(TProcedureCall);
    Add(TLabel);
    Add(TGoto);
    Add(TStandaloneComment);
  end;
  { Зарегистрируем конструкции для деклараций }
  with DeclarationParser do
  begin
    Add(TType);
    Add(TCursor);
    Add(TPragma);
    Add(TSubroutineForwardDeclaration);
    Add(TSubroutine);
    Add(TExceptionDeclaration);
    Add(TVariableDeclarations);
  end;
  { Зарегистрируем конструкции для типов }
  with TypeParser do
  begin
    Add(TRecord);
    Add(TObject_);
    Add(TPLSQLTable);
    Add(TRefCursor);
  end;
  { Включим PLSQL в общеоракловый парсер }
  OracleParser.Add(PLSQLParser);
  OracleParser.Add(DeclarationParser);
  OracleParser.Add(TypeParser);

end.

