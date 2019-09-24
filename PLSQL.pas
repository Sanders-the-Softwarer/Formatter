﻿////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                      Синтаксические конструкции PL/SQL                     //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit PLSQL;

{ ----- Примечания -------------------------------------------------------------

  Объекты, представляющие собой различные части выражения, в основном
  декларируются как TStatement, а не как вхождение класса-наследника,
  используемого в реальности. Это связано с тем, что никто не обещал
  нам корректности форматируемого кода, и на этом месте в результате
  парсинга может оказаться unexpected token или другой подобный объект.

------------------------------------------------------------------------------ }

interface

uses Windows, SysUtils, Math, Streams, Tokens, Statements, Expressions,
  Printers_, Attributes;

type

  { Class reference для TProgramBlock.GetHeaderClass }
  TStatementClass = class of TStatement;

  { Программный блок - та или иная конструкция на основе begin .. end }
  TProgramBlock = class(TStatement)
  strict private
    _Header: TStatement;
    _Declarations: TStatement;
    _Begin: TKeyword;
    _Operators: TStatement;
    _Exception: TKeyword;
    _ExceptionHandlers: TStatement;
    _End: TKeyword;
    _EndName: TIdent;
    _Semicolon: TTerminal;
    _Slash: TTerminal;
  strict protected
    property Header: TStatement read _Header;
    function GetHeaderClass: TStatementClass; virtual; abstract;
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Заголовок анонимного блока }
  TAnonymousHeader = class(TStatement)
  strict private
    _Declare: TKeyword;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Анонимный блок }
  TAnonymousBlock = class(TProgramBlock)
  strict protected
    function GetHeaderClass: TStatementClass; override;
  end;

  { Заголовок пакета }
  TPackageHeader = class(TStatement)
  private {!strict}
    _Package, _Body: TKeyword;
    _PackageName: TIdent;
    _Is: TKeyword;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
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
    _Initial: TKeyword;
    _Name: TIdent;
    _Params: TStatement;
    _Return: TKeyword;
    _ReturnType: TStatement;
    _Deterministic: TKeyword;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function MultiLine: boolean;
    function StatementName: string; override;
  end;

  { Заголовок подпрограммы }
  TSubroutineHeader = class(TSubroutineHeaderBase)
  strict private
    _Is: TKeyword;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function Name: string; override;
  end;

  { Предварительное объявление подпрограммы }
  TSubroutineForwardDeclaration = class(TSubroutineHeaderBase)
  strict private
    _Semicolon: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Подпрограмма }
  TSubroutine = class(TProgramBlock)
  strict protected
    function GetHeaderClass: TStatementClass; override;
  public
    function StatementName: string; override;
  end;

  { Название типа данных }
  TTypeRef = class(TStatement)
  strict private
    _Ident: TIdent;
    _Dot: TTerminal;
    _SecondIdent: TIdent;
    _Type: TTerminal;
    _RowType: TTerminal;
    _OpenBracket: TTerminal;
    _Size: TNumber;
    _Unit: TKeyword;
    _CloseBracket: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Параметр подпрограммы }
  TParamDeclaration = class(TStatement)
  strict private
    _ParamName: TIdent;
    _In: TKeyword;
    _Out: TKeyword;
    _Nocopy: TKeyword;
    _ParamType: TStatement;
    _TAssignment: TTerminal;
    _KAssignment: TKeyword;
    _DefaultValue: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    function StatementName: string; override;
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Блок параметров подпрограммы }
  [Aligned]
  TParamsDeclaration = class(TCommaList<TParamDeclaration>)
  strict private
    _OpenBracket: TTerminal;
    _CloseBracket: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    function ParseBreak: boolean; override;
    function MultiLine: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Блок деклараций }
  TDeclarations = class(TStatementList<TStatement>)
  strict protected
    function ParseStatement(out AResult: TStatement): boolean; override;
    function ParseBreak: boolean; override;
  end;

  { Объявление переменной }
  TVariableDeclaration = class(TStatement)
  strict private
    _Name: TIdent;
    _Constant: TKeyword;
    _Type: TStatement;
    _TAssignment: TTerminal;
    _KAssignment: TKeyword;
    _Value: TStatement;
    _Semicolon: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function StatementName: string; override;
  end;

  { Блок переменных }
  [Aligned]
  TVariableDeclarations = class(TStatementList<TVariableDeclaration>)
  strict protected
    function ParseBreak: boolean; override;
  end;

  { Объявление исключения }
  TExceptionDeclaration = class(TStatement)
  strict private
    _Name: TIdent;
    _Exception: TKeyword;
    _Semicolon: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Список операторов }
  TOperators = class(TStatementList<TStatement>)
  strict protected
    function ParseStatement(out AResult: TStatement): boolean; override;
    function ParseDelimiter(out AResult: TToken): boolean; override;
    function ParseBreak: boolean; override;
  end;

  { Список операторов в секциях then/else }
  TIfOperators = class(TOperators)
  strict protected
    function ParseBreak: boolean; override;
  end;

  { Присваивание }
  TAssignment = class(TStatement)
  strict private
    _Ident: TIdent;
    _Assignment: TTerminal;
    _Expression: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Вызов процедуры }
  TProcedureCall = class(TStatement)
  strict private
    _FunctionCall: TStatement;
    _Ident: TIdent;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор return }
  TReturn = class(TStatement)
  strict private
    _Return: TKeyword;
    _Value: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор null }
  TNull = class(TStatement)
  strict private
    _Null: TKeyword;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор raise }
  TRaise = class(TStatement)
  strict private
    _Raise: TKeyword;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Условный оператор }
  TIf = class(TStatement)
  strict private
    _If: TKeyword;
    _Condition: TStatement;
    _Sections: TStatement;
    _EndIf: TKeyword;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Секция оператора if }
  TIfSection = class(TStatement)
  strict private
    _Keyword: TKeyword;
    _Condition: TStatement;
    _Then: TKeyword;
    _Statements: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    function StatementName: string; override;
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Секции оператора if }
  TIfSections = class(TStatementList<TIfSection>)
  strict protected
    function ParseBreak: boolean; override;
  end;

  { Оператор loop }
  TLoop = class(TOperators)
  strict private
    _Loop, _EndLoop: TKeyword;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор for }
  TFor = class(TStatement)
  strict private
    _For: TKeyword;
    _Variable: TIdent;
    _In: TKeyword;
    _Reverse: TKeyword;
    _Low: TStatement;
    _To: TTerminal;
    _High: TStatement;
    _Select: TStatement;
    _SCursor: TStatement;
    _ICursor: TIdent;
    _Loop: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Оператор while }
  TWhile = class(TStatement)
  strict private
    _While: TKeyword;
    _Condition: TStatement;
    _Loop: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Блок обработки исключений }
  TExceptionHandler = class(TStatement)
  strict private
    _When: TKeyword;
    _Condition: TStatement;
    _Then: TKeyword;
    _Statements: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Список обработчиков исключений }
  TExceptionHandlers = class(TStatementList<TExceptionHandler>)
  strict protected
    function ParseBreak: boolean; override;
  end;

  { Оператор open for }
  TOpenFor = class(TStatement)
  strict private
    _Open: TKeyword;
    _Cursor: TIdent;
    _For: TKeyword;
    _Select: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Декларация pragma }
  TPragma = class(TStatement)
  strict private
    _Pragma: TKeyword;
    _Body: TStatement;
    _Semicolon: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses Parser, DML;

{ TProgramBlock }

function TProgramBlock.InternalParse: boolean;
begin
  { Пытаемся распознать заголовок }
  Result := GetHeaderClass.Parse(Self, Source, _Header);
  if not Result then exit;
  { Распознаём декларации }
  TDeclarations.Parse(Self, Source, _Declarations);
  { Если нашли begin, распознаём операторы }
  _Begin := Keyword('begin');
  if Assigned(_Begin) then TOperators.Parse(Self, Source, _Operators);
  { Если нашли exception, распознаём обработчики исключений }
  _Exception := Keyword('exception');
  if Assigned(_Exception) then TExceptionHandlers.Parse(Self, Source, _ExceptionHandlers);
  { end }
  _End := Keyword('end');
  { Если это блок верхнего уровня, то загребём и завершающие символы после него }
  if Parent is TOperators then exit;
  if Assigned(_End) then
  begin
    _EndName := Identifier;
    _Semicolon := Terminal(';');
    _Slash := Terminal('/');
  end;
end;

procedure TProgramBlock.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Header);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_Declarations);
  APrinter.Undent;
  APrinter.PrintItem(_Begin);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_Operators);
  APrinter.Undent;
  APrinter.PrintItem(_Exception);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_ExceptionHandlers);
  APrinter.Undent;
  APrinter.PrintItem(_End);
  APrinter.Space;
  APrinter.PrintItem(_EndName);
  APrinter.SupressSpace;
  APrinter.PrintItem(_Semicolon);
  APrinter.NextLine;
  APrinter.PrintItem(_Slash);
  APrinter.NextLine;
end;

{ TPackageHeader }

function TPackageHeader.InternalParse: boolean;
begin
  { Если распознали слово package, то распознали конструкцию }
  _Package := Keyword('package');
  if not Assigned(_Package) then exit(false);
  Result := true;
  { Проверим наличие body }
  _Body := Keyword('body');
  { Проверим название пакета }
  _PackageName := Identifier;
  { Проверим наличие is }
  _Is := Keyword(['is', 'as']);
end;

procedure TPackageHeader.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Package);
  APrinter.Space;
  APrinter.PrintItem(_Body);
  APrinter.Space;
  APrinter.PrintItem(_PackageName);
  APrinter.Space;
  APrinter.PrintItem(_Is);
  APrinter.NextLine;
end;

{ TPackage }

function TPackage.StatementName: string;
begin
  with Header as TPackageHeader do
  begin
    if Assigned(_Package) then Result := _Package.Value + ' ';
    if Assigned(_Body) then Result := Result + _Body.Value + ' ';
    if Assigned(_PackageName) then Result := Result + _PackageName.Value + ' ';
  end;
end;

function TPackage.GetHeaderClass: TStatementClass;
begin
  Result := TPackageHeader;
end;

{ TSubroutineHeaderBase }

function TSubroutineHeaderBase.InternalParse: boolean;
begin
  { Проверим procedure/function }
  _Initial := Keyword(['procedure', 'function']);
  if not Assigned(_Initial) then exit(false);
  Result := true;
  { Название процедуры и параметры }
  _Name := Identifier;
  if not Assigned(_Name) then exit;
  TParamsDeclaration.Parse(Self, Source, _Params);
  { Возвращаемое значение }
  _Return := Keyword('return');
  TTypeRef.Parse(Self, Source, _ReturnType);
  { Признак deterministic }
  _Deterministic := Keyword('deterministic');
end;

procedure TSubroutineHeaderBase.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Initial);
  APrinter.Space;
  APrinter.PrintItem(_Name);
  APrinter.Space;
  if MultiLine then
  begin
    APrinter.NextLine;
    APrinter.Indent;
  end;
  APrinter.PrintItem(_Params);
  if Assigned(_Return) then APrinter.SpaceOrNextLine(MultiLine);
  APrinter.PrintItem(_Return);
  APrinter.Space;
  APrinter.PrintItem(_ReturnType);
  if Assigned(_Deterministic) then APrinter.SpaceOrNextLine(MultiLine);
  APrinter.PrintItem(_Deterministic);
end;

function TSubroutineHeaderBase.MultiLine: boolean;
begin
  Result := Assigned(_Params) and ((_Params as TCommaList<TParamDeclaration>).Count > Settings.DeclarationSingleLineParamLimit);
end;

function TSubroutineHeaderBase.StatementName: string;
begin
  Result := _Initial.Value + ' ' + _Name.Value;
end;

{ TTypeRef }

function TTypeRef.InternalParse: boolean;
begin
  { Если распознали идентификатор, тип данных распознан }
  _Ident := Identifier;
  if not Assigned(_Ident) then exit(false);
  Result := true;
  { Проверим точку, второй идентификатор и %[row]type }
  _Dot := Terminal('.');
  if Assigned(_Dot) then _SecondIdent := Identifier;
  _Type := Terminal('%type');
  _RowType := Terminal('%rowtype');
  { Проверим указание размера }
  _OpenBracket := Terminal('(');
  if Assigned(_OpenBracket) then
  begin
    _Size := Number;
    _Unit := Keyword(['char', 'byte']);
    _CloseBracket := Terminal(')');
  end;
end;

procedure TTypeRef.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Ident);
  APrinter.PrintItem(_Dot);
  APrinter.PrintItem(_SecondIdent);
  APrinter.PrintItem(_Type);
  APrinter.PrintItem(_RowType);
  APrinter.PrintItem(_OpenBracket);
  APrinter.PrintItem(_Size);
  APrinter.Space;
  APrinter.PrintItem(_Unit);
  APrinter.SupressSpace;
  APrinter.PrintItem(_CloseBracket);
end;

{ TParamDeclaration }

function TParamDeclaration.InternalParse: boolean;
begin
  _ParamName := Identifier;
  _In := Keyword('in');
  _Out := Keyword('out');
  _Nocopy := Keyword('nocopy');
  TTypeRef.Parse(Self, Source, _ParamType);
  if not Assigned(_ParamName) and not Assigned(_In) and not Assigned(_Out) and not Assigned(_Nocopy) and not Assigned(_ParamType) then exit(false);
  _TAssignment := Terminal(':=');
  if not Assigned(_TAssignment) then _KAssignment := Keyword('default');
  if Assigned(_TAssignment) or Assigned(_KAssignment) then TExpression.Parse(Self, Source, _DefaultValue);
  Result := true;
end;

function TParamDeclaration.StatementName: string;
begin
  Result := _ParamName.Value;
end;

procedure TParamDeclaration.PrintSelf(APrinter: TPrinter);
var NeedRuler: boolean;
begin
  NeedRuler := Settings.AlignSubroutineParams and (Parent.Parent as TSubroutineHeaderBase).MultiLine;
  APrinter.PrintItem(_ParamName);
  APrinter.Ruler('modifiers', NeedRuler);
  APrinter.Space;
  APrinter.PrintItem(_In);
  APrinter.Space;
  APrinter.PrintItem(_Out);
  APrinter.Space;
  APrinter.PrintItem(_Nocopy);
  APrinter.Ruler('type', NeedRuler);
  APrinter.Space;
  APrinter.PrintItem(_ParamType);
  APrinter.Space;
  APrinter.Ruler('default', NeedRuler and (Assigned(_TAssignment) or Assigned(_KAssignment)));
  APrinter.PrintItem(_TAssignment);
  APrinter.PrintItem(_KAssignment);
  APrinter.Space;
  APrinter.PrintItem(_DefaultValue);
end;

{ TParamsDeclaration }

function TParamsDeclaration.InternalParse: boolean;
begin
  _OpenBracket := Terminal('(');
  if not Assigned(_OpenBracket) then exit(false);
  Result := inherited InternalParse;
  if Result then _CloseBracket := Terminal(')');
end;

function TParamsDeclaration.ParseBreak: boolean;
begin
  Result := Any([Terminal(')')]) or (Source.Next is TKeyword);
end;

function TParamsDeclaration.MultiLine: boolean;
begin
  Result := (Parent as TSubroutineHeaderBase).MultiLine;
end;

procedure TParamsDeclaration.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_OpenBracket);
  if MultiLine then APrinter.NextLine;
  APrinter.Indent;
  inherited;
  APrinter.Undent;
  APrinter.SupressSpace;
  APrinter.PrintItem(_CloseBracket);
end;

{ TDeclarations }

function TDeclarations.ParseStatement(out AResult: TStatement): boolean;
begin
  Result := TSubroutineForwardDeclaration.Parse(Self, Source, AResult) or
            TSubroutine.Parse(Self, Source, AResult) or
            TPragma.Parse(Self, Source, AResult) or
            TExceptionDeclaration.Parse(Self, Source, AResult) or
            TVariableDeclarations.Parse(Self, Source, AResult);
end;

function TDeclarations.ParseBreak: boolean;
begin
  Result := Any([Keyword(['begin', 'end'])]);
end;

{ TVariableDeclaration }

function TVariableDeclaration.InternalParse: boolean;
begin
  { Если конструкция начинается с идентификатора, будем считать объявление распознанным }
  _Name := Identifier;
  if not Assigned(_Name) then exit(false);
  Result := true;
  _Constant := Keyword('constant');
  { Теперь тип и значение }
  if not TTypeRef.Parse(Self, Source, _Type) then exit;
  _TAssignment := Terminal(':=');
  if not Assigned(_TAssignment) then _KAssignment := Keyword('default');
  if Assigned(_TAssignment) or Assigned (_KAssignment) then TExpression.Parse(Self, Source, _Value);
  _Semicolon := Terminal(';');
end;

function TVariableDeclaration.StatementName: string;
begin
  Result := _Name.Value;
end;

procedure TVariableDeclaration.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Name);
  APrinter.Ruler('name', Settings.AlignVariables);
  APrinter.Space;
  APrinter.PrintItem(_Constant);
  APrinter.Ruler('constant', Settings.AlignVariables);
  APrinter.Space;
  APrinter.PrintItem(_Type);
  APrinter.Ruler('type', Settings.AlignVariables and (Assigned(_TAssignment) or Assigned(_KAssignment)));
  APrinter.Space;
  APrinter.PrintItem(_TAssignment);
  APrinter.PrintItem(_KAssignment);
  APrinter.Space;
  APrinter.PrintItem(_Value);
  APrinter.SupressSpace;
  APrinter.PrintItem(_Semicolon);
  APrinter.NextLine;
end;

{ TVariableDeclarations }

function TVariableDeclarations.ParseBreak: boolean;
begin
  Result := Any([Keyword(['begin', 'end', 'type', 'cursor', 'procedure', 'function'])]);
end;

{ TProcedureCall }

function TProcedureCall.InternalParse: boolean;
var P: TMark;
begin
  P := Source.Mark;
  if not TFunctionCall.Parse(Self, Source, _FunctionCall) then
  begin
    Source.Restore(P);
    _Ident := Identifier;
  end;
  Result := Assigned(_Ident) or Assigned(_FunctionCall);
  inherited;
end;

procedure TProcedureCall.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_FunctionCall);
  APrinter.PrintItem(_Ident);
end;

{ TAssignment }

function TAssignment.InternalParse: boolean;
begin
  _Ident := Identifier;
  _Assignment := Terminal(':=');
  Result := Assigned(_Ident) and Assigned(_Assignment);
  if not Result then exit;
  TExpression.Parse(Self, Source, _Expression);
  inherited;
end;

procedure TAssignment.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Ident);
  APrinter.Space;
  APrinter.PrintItem(_Assignment);
  APrinter.Space;
  APrinter.PrintItem(_Expression);
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

procedure TReturn.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Return);
  APrinter.Space;
  APrinter.PrintItem(_Value);
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
end;

procedure TSubroutineHeader.PrintSelf(APrinter: TPrinter);
begin
  inherited;
  APrinter.SpaceOrNextLine(MultiLine);
  if MultiLine then APrinter.Undent;
  APrinter.PrintItem(_Is);
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

procedure TSubroutineForwardDeclaration.PrintSelf(APrinter: TPrinter);
begin
  inherited;
  APrinter.SupressSpace;
  APrinter.PrintItem(_Semicolon);
  APrinter.NextLine;
  if MultiLine then APrinter.Undent;
end;

{ TOperators }

function TOperators.ParseStatement(out AResult: TStatement): boolean;
begin
  Result := TParser.ParseOperator(Self, Source, AResult);
end;

function TOperators.ParseDelimiter(out AResult: TToken): boolean;
begin
  AResult := Terminal(';');
  Result  := Assigned(AResult);
end;

function TOperators.ParseBreak: boolean;
begin
  Result := Any([Keyword(['end', 'end if', 'end loop', 'exception'])]);
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
end;

procedure TIf.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_If);
  APrinter.Space;
  APrinter.PrintItem(_Condition);
  APrinter.Space;
  APrinter.PrintItem(_Sections);
  APrinter.PrintItem(_EndIf);
end;

{ TIfOperators }

function TIfOperators.ParseBreak: boolean;
begin
  Result := Any([Keyword(['else', 'elsif', 'end if'])]) or inherited ParseBreak;
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

procedure TAnonymousHeader.PrintSelf(APrinter: TPrinter);
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
  Result := Assigned(_When) and TExpression.Parse(Self, Source, _Condition);
  if not Result then exit;
  _Then := Keyword('then');
  TOperators.Parse(Self, Source, _Statements);
end;

procedure TExceptionHandler.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_When);
  APrinter.Space;
  APrinter.PrintItem(_Condition);
  APrinter.Space;
  APrinter.PrintItem(_Then);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_Statements);
  APrinter.Undent;
end;

{ TExceptionHandlers }

function TExceptionHandlers.ParseBreak: boolean;
begin
  Result := Any([Keyword(['when', 'end'])]);
end;

{ TNull }

function TNull.InternalParse: boolean;
begin
  _Null := Keyword('null');
  Result := Assigned(_Null);
  inherited;
end;

procedure TNull.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Null);
end;

{ TRaise }

function TRaise.InternalParse: boolean;
begin
  _Raise := Keyword('raise');
  Result := Assigned(_Raise);
  inherited;
end;

procedure TRaise.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Raise);
end;

{ TExceptionDeclaration }

function TExceptionDeclaration.InternalParse: boolean;
begin
  _Name := Identifier;
  _Exception := Keyword('exception');
  _Semicolon := Terminal(';');
  Result := Assigned(_Name) and Assigned(_Exception);
end;

procedure TExceptionDeclaration.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Name);
  APrinter.Space;
  APrinter.PrintItem(_Exception);
  APrinter.PrintItem(_Semicolon);
end;

{ TIfSection }

function TIfSection.InternalParse: boolean;
begin
  _Keyword := Keyword(['then', 'elsif', 'else']);
  if not Assigned(_Keyword) then exit(false);
  if _Keyword.Value = 'elsif' then
  begin
    TExpression.Parse(Self, Source, _Condition);
    _Then := Keyword('then');
  end;
  TIfOperators.Parse(Self, Source, _Statements);
  Result := true;
end;

function TIfSection.StatementName: string;
begin
  Result := _Keyword.Value;
end;

procedure TIfSection.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Keyword);
  APrinter.Space;
  APrinter.PrintItem(_Condition);
  APrinter.Space;
  APrinter.PrintItem(_Then);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_Statements);
  APrinter.NextLine;
  APrinter.Undent;
end;

{ TIfSections }

function TIfSections.ParseBreak: boolean;
begin
  Result := Assigned(Keyword('end if'));
end;

{ TOpenFor }

function TOpenFor.InternalParse: boolean;
begin
  _Open := Keyword('open');
  if not Assigned(_Open) then exit(false);
  _Cursor := Identifier;
  _For := Keyword('for');
  TSelect.Parse(Self, Source, _Select);
  Result := true;
end;

procedure TOpenFor.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Open);
  APrinter.Space;
  APrinter.PrintItem(_Cursor);
  APrinter.NextLine;
  APrinter.PrintItem(_For);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_Select);
  APrinter.Undent;
end;

{ TPragma }

function TPragma.InternalParse: boolean;
begin
  _Pragma := Keyword('pragma');
  if not Assigned(_Pragma) then exit(false);
  TFunctionCall.Parse(Self, Source, _Body);
  _Semicolon := Terminal(';');
  Result := true;
end;

procedure TPragma.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Pragma);
  APrinter.Space;
  APrinter.PrintItem(_Body);
  APrinter.PrintItem(_Semicolon);
end;

{ TLoop }

function TLoop.InternalParse: boolean;
begin
  _Loop := Keyword('loop');
  if not Assigned(_Loop) then exit(false);
  Result := inherited InternalParse;
  if Result then _EndLoop := Keyword('end loop');
end;

procedure TLoop.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Loop);
  APrinter.NextLine;
  APrinter.Indent;
  inherited;
  APrinter.NextLine;
  APrinter.Undent;
  APrinter.PrintItem(_EndLoop);
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
    TExpression.Parse(Self, Source, _Low);
    _To := Terminal('..');
    TExpression.Parse(Self, Source, _High);
    if not Assigned(_To) then
    begin
      Source.Restore(P);
      if not TFunctionCall.Parse(Self, Source, _SCursor) then
        _ICursor := Identifier;
    end;
  end;
  TLoop.Parse(Self, Source, _Loop);
  Result := true;
end;

procedure TFor.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_For);
  APrinter.Space;
  APrinter.PrintItem(_Variable);
  APrinter.Space;
  APrinter.PrintItem(_In);
  APrinter.PrintItem(_ICursor);
  APrinter.PrintItem(_SCursor);
  APrinter.PrintItem(_Low);
  APrinter.PrintItem(_To);
  APrinter.PrintItem(_High);
  APrinter.Space;
  APrinter.PrintItem(_Loop);
end;

{ TWhile }

function TWhile.InternalParse: boolean;
begin
  _While := Keyword('while');
  if not Assigned(_While) then exit(false);
  TExpression.Parse(Self, Source, _Condition);
  TLoop.Parse(Self, Source, _Loop);
end;

procedure TWhile.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_While);
  APrinter.Space;
  APrinter.PrintItem(_Condition);
  APrinter.Space;
  APrinter.PrintItem(_Loop);
end;

end.
