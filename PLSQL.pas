////////////////////////////////////////////////////////////////////////////////
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

uses SysUtils, Math, System.Generics.Collections, Streams, Parser, Tokens, Expressions,
  Printers_, DML;

type

  { Программный блок - та или иная конструкция на основе begin .. end }
  TProgramBlock = class(TStatement)
  strict private
    _Header: TStatement;
    _Declarations: TStatement;
    _Begin: TKeyword;
    _Operators: TStatement;
    _Exceptions: TKeyword;
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
    function Name: string; override;
  end;

  { Заголовок пакета }
  TPackageHeader = class(TStatement)
  strict protected
    _Package, _Body: TKeyword;
    _PackageName: TIdent;
    _Is: TKeyword;
  strict protected
    function InternalParse: boolean; override;
  public
    function Name: string; override;
    procedure PrintSelf(APrinter: TPrinter); override;
    function PackageInfo: string;
  end;

  { Пакет }
  TPackage = class(TProgramBlock)
  strict protected
    function GetHeaderClass: TStatementClass; override;
  public
    function Name: string; override;
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
    function Name: string; override;
    function MultiLine: boolean;
  end;

  { Заголовок подпрограммы }
  TSubroutineHeader = class(TSubroutineHeaderBase)
  strict private
    _Is: TKeyword;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
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
    function Name: string; override;
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
    _Default: TKeyword;
    _Value: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    function Name: string; override;
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Блок параметров подпрограммы }
  TParamsDeclaration = class(TStatementList)
  strict private
    _OpenBracket: TTerminal;
    _CloseBracket: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    function ParseStatement(out AResult: TStatement): boolean; override;
    function ParseBreak: boolean; override;
  public
    function Name: string; override;
    procedure PrintSelf(APrinter: TPrinter); override;
    function ParamNameMax: integer;
    function ModifiersMax: integer;
  end;

  { Параметр подпрограммы }
  TParamDeclaration = class(TStatement)
  strict private
    _ParamName: TIdent;
    _In: TKeyword;
    _Out: TKeyword;
    _Nocopy: TKeyword;
    _ParamType: TStatement;
    _Comma: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    function Name: string; override;
    procedure PrintSelf(APrinter: TPrinter); override;
    function ParamNameLen: integer;
    function ModifiersLen: integer;
  end;

  { Блок деклараций }
  TDeclarations = class(TStatementList)
  strict protected
    function ParseStatement(out AResult: TStatement): boolean; override;
    function ParseBreak: boolean; override;
  public
    function Name: string; override;
  end;

  { Блок переменных }
  TVariableDeclarations = class(TStatementList)
  strict protected
    function ParseStatement(out AResult: TStatement): boolean; override;
    function ParseBreak: boolean; override;
  public
    function Name: string; override;
    function MaxNameLen: integer;
  end;

  { Объявление переменной }
  TVariableDeclaration = class(TStatement)
  strict private
    _Name: TIdent;
    _Constant: TKeyword;
    _Type: TStatement;
    _Assignment: TTerminal;
    _Value: TStatement;
    _Semicolon: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function Name: string; override;
    function NameLen: integer;
  end;

  { Список операторов }
  TOperators = class(TStatementList)
  strict protected
    function ParseStatement(out AResult: TStatement): boolean; override;
    function ParseBreak: boolean; override;
  public
    function Name: string; override;
  end;

  { Список операторов в секциях then/else }
  TIfOperators = class(TOperators)
  strict protected
    function ParseBreak: boolean; override;
  end;

  { Оператор }
  TOperator = class(TStatement)
  strict private
    _Semicolon: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  public
    class function ParseOperator(AParent: TStatement; Tokens: TBufferedStream<TToken>; out AResult: TStatement): boolean;
  end;

  { Присваивание }
  TAssignment = class(TOperator)
  strict private
    _Ident: TIdent;
    _Assignment: TTerminal;
    _Expression: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function Name: string; override;
  end;

  { Вызов процедуры }
  TProcedureCall = class(TOperator)
  strict private
    _FunctionCall: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function Name: string; override;
  end;

  { Оператор return }
  TReturn = class(TOperator)
  strict private
    _Return: TKeyword;
    _Value: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function Name: string; override;
  end;

  { Условный оператор }
  TIf = class(TOperator)
  strict private
    _If: TKeyword;
    _Condition: TStatement;
    _Then: TKeyword;
    _ThenStatements: TStatement;
    _Else: TKeyword;
    _ElseStatements: TStatement;
    _EndIf: TKeyword;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function Name: string; override;
  end;

implementation

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
{  _Exceptions := Keyword('exceptions');
  if Assigned(_Exceptions) then TExceptionHandlers.Parse(Self, Source, _ExceptionHandlers);}
  { end и завершающие символы после него }
  _End := Keyword('end');
  if Assigned(_End) then
  begin
    _EndName := Identifier;
    _Semicolon := Terminal(';');
    _Slash := Terminal('/');
  end;
end;

function TProgramBlock.Name: string;
begin
  Result := 'pl/sql block';
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
  APrinter.PrintItem(_Exceptions);
  APrinter.Indent;
  APrinter.PrintItem(_ExceptionHandlers);
  APrinter.Undent;
  APrinter.PrintItem(_End);
  APrinter.Space;
  APrinter.PrintItem(_EndName);
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

function TPackageHeader.Name: string;
begin
  Result := '< header >';
end;

function TPackageHeader.PackageInfo: string;
begin
  if Assigned(_Package) then Result := _Package.Value + ' ';
  if Assigned(_Body) then Result := Result + _Body.Value + ' ';
  if Assigned(_PackageName) then Result := Result + _PackageName.Value + ' ';
  Result := Trim(Result);
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

function TPackage.Name: string;
begin
  Result := '< ' + (Header as TPackageHeader).PackageInfo + ' >';
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

function TSubroutineHeaderBase.Name: string;
begin
  if Assigned(_Name)
    then Result := _Name.Value
    else Result := _Initial.Value;
end;

function TSubroutineHeaderBase.MultiLine: boolean;
begin
  Result := Assigned(_Params) and ((_Params as TStatementList).Count >= 3);
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
  { Проверим значение по умолчанию }
  _Default := Keyword('default');
  if Assigned(_Default) then TExpression.Parse(Self, Source, _Value);
end;

function TTypeRef.Name: string;
begin
  Result := '< type >';
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
  if Assigned(_Default) then
  begin
    APrinter.Space;
    APrinter.PrintItem(_Default);
    APrinter.Space;
    APrinter.PrintItem(_Value);
  end;
end;

{ TParamDeclaration }

function TParamDeclaration.InternalParse: boolean;
begin
  _ParamName := Identifier;
  _In := Keyword('in');
  _Out := Keyword('out');
  _Nocopy := Keyword('nocopy');
  TTypeRef.Parse(Self, Source, _ParamType);
  _Comma := Terminal(',');
  Result := Assigned(_ParamName) or Assigned(_In) or Assigned(_Out) or
    Assigned(_Nocopy) or Assigned(_ParamType) or Assigned(_Comma);
end;

function TParamDeclaration.Name: string;
begin
  if Assigned(_ParamName)
    then Result := _ParamName.Value
    else Result := '< param >';
end;

procedure TParamDeclaration.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PaddingFrom;
  APrinter.PrintItem(_ParamName);
  APrinter.PaddingTo((Parent as TParamsDeclaration).ParamNameMax);
  APrinter.PaddingFrom;
  APrinter.Space;
  APrinter.PrintItem(_In);
  APrinter.Space;
  APrinter.PrintItem(_Out);
  APrinter.Space;
  APrinter.PrintItem(_Nocopy);
  APrinter.PaddingTo((Parent as TParamsDeclaration).ModifiersMax);
  APrinter.Space;
  APrinter.PrintItem(_ParamType);
  APrinter.PrintItem(_Comma);
  APrinter.SpaceOrNextLine((Parent.Parent as TSubroutineHeaderBase).MultiLine);
end;

function TParamDeclaration.ParamNameLen: integer;
begin
  Result := Length(_ParamName.Value);
end;

function TParamDeclaration.ModifiersLen: integer;
begin
  Result := 0;
  if Assigned(_In)     then Inc(Result, 3);
  if Assigned(_Out)    then Inc(Result, 4);
  if Assigned(_Nocopy) then Inc(Result, 7);
end;

{ TParamsDeclaration }

function TParamsDeclaration.InternalParse: boolean;
begin
  _OpenBracket := Terminal('(');
  if not Assigned(_OpenBracket) then exit(false);
  Result := inherited InternalParse;
  if Result then _CloseBracket := Terminal(')');
end;

function TParamsDeclaration.ParseStatement(out AResult: TStatement): boolean;
begin
  Result := TParamDeclaration.Parse(Self, Source, AResult);
end;

function TParamsDeclaration.ParseBreak: boolean;
begin
  Result := Any([Terminal(')')]);
end;

function TParamsDeclaration.Name: string;
begin
  Result := '< parameters >';
end;

procedure TParamsDeclaration.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_OpenBracket);
  if (Parent as TSubroutineHeaderBase).MultiLine then APrinter.NextLine;
  APrinter.Indent;
  inherited;
  APrinter.Undent;
  APrinter.SupressSpace;
  APrinter.PrintItem(_CloseBracket);
end;

function TParamsDeclaration.ParamNameMax: integer;
var i: integer;
begin
  Result := 0;
  if not (Parent as TSubroutineHeaderBase).MultiLine then exit;
  for i := 0 to Count - 1 do
    if Item(i) is TParamDeclaration then
      Result := Math.Max(Result, TParamDeclaration(Item(i)).ParamNameLen);
end;

function TParamsDeclaration.ModifiersMax: integer;
var i: integer;
begin
  Result := 0;
  if not (Parent as TSubroutineHeaderBase).MultiLine then exit;
  for i := 0 to Count - 1 do
    if Item(i) is TParamDeclaration then
      Result := Math.Max(Result, TParamDeclaration(Item(i)).ModifiersLen);
end;

{ TDeclarations }

function TDeclarations.ParseStatement(out AResult: TStatement): boolean;
begin
  Result := TSubroutineForwardDeclaration.Parse(Self, Source, AResult) or
            TSubroutine.Parse(Self, Source, AResult) or
            TVariableDeclarations.Parse(Self, Source, AResult);
end;

function TDeclarations.ParseBreak: boolean;
begin
  Result := Any([Keyword(['begin', 'end'])]);
end;

function TDeclarations.Name: string;
begin
  Result := '< declarations >';
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
  _Assignment := Terminal(':=');
  if Assigned(_Assignment) then TExpression.Parse(Self, Source, _Value);
  _Semicolon := Terminal(';');
end;

function TVariableDeclaration.Name: string;
begin
  Result := 'variable';
  if Assigned(_Name) then Result := Result + ' ' + _Name.Value;
end;

function TVariableDeclaration.NameLen: integer;
begin
  if Assigned(_Name)
    then Result := Length(_Name.Value)
    else Result := 0;
end;

procedure TVariableDeclaration.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PaddingFrom;
  APrinter.PrintItem(_Name);
  APrinter.PaddingTo((Parent as TVariableDeclarations).MaxNameLen);
  APrinter.Space;
  APrinter.PrintItem(_Constant);
  APrinter.Space;
  APrinter.PrintItem(_Type);
  APrinter.Space;
  APrinter.PrintItem(_Assignment);
  APrinter.Space;
  APrinter.PrintItem(_Value);
  APrinter.SupressSpace;
  APrinter.PrintItem(_Semicolon);
  APrinter.NextLine;
end;

{ TVariableDeclarations }

function TVariableDeclarations.ParseStatement(out AResult: TStatement): boolean;
begin
  Result := TVariableDeclaration.Parse(Self, Source, AResult);
end;

function TVariableDeclarations.ParseBreak: boolean;
begin
  Result := Any([Keyword(['begin', 'end', 'type', 'cursor', 'procedure', 'function'])]);
end;

function TVariableDeclarations.Name: string;
begin
  Result := '< variables >';
end;

function TVariableDeclarations.MaxNameLen: integer;
var i: integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if Item(i) is TVariableDeclaration then
      Result := Math.Max(Result, TVariableDeclaration(Item(i)).NameLen);
end;

{ TOperator }

class function TOperator.ParseOperator(AParent: TStatement;
  Tokens: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result :=
    TAssignment.Parse(AParent, Tokens, AResult) or
    TReturn.Parse(AParent, Tokens, AResult) or
    TIf.Parse(AParent, Tokens, AResult) or
    TDML.Parse(AParent, Tokens, AResult) or
    TProcedureCall.Parse(AParent, Tokens, AResult);
end;

function TOperator.InternalParse: boolean;
begin
  _Semicolon := Terminal(';');
  Result := Assigned(_Semicolon);
end;

procedure TOperator.PrintSelf(APrinter: TPrinter);
begin
  APrinter.SupressSpace;
  APrinter.PrintItem(_Semicolon);
  APrinter.NextLine;
end;

{ TProcedureCall }

function TProcedureCall.InternalParse: boolean;
begin
  Result := TFunctionCall.Parse(Self, Source, _FunctionCall);
  inherited;
end;

function TProcedureCall.Name: string;
begin
  Result := 'procedure call';
end;

procedure TProcedureCall.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_FunctionCall);
  inherited;
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

function TAssignment.Name: string;
begin
  Result := '< assignment >';
end;

procedure TAssignment.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Ident);
  APrinter.Space;
  APrinter.PrintItem(_Assignment);
  APrinter.Space;
  APrinter.PrintItem(_Expression);
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

function TReturn.Name: string;
begin
  Name := 'return';
end;

procedure TReturn.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Return);
  APrinter.Space;
  APrinter.PrintItem(_Value);
  inherited;
end;

{ TSubroutine }

function TSubroutine.GetHeaderClass: TStatementClass;
begin
  Result := TSubroutineHeader;
end;

function TSubroutine.Name: string;
begin
  Result := '< subroutine >';
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
  APrinter.PrintItem(_Is);
  APrinter.NextLine;
  if MultiLine then APrinter.Undent;
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
  Result := TOperator.ParseOperator(Self, Source, AResult);
end;

function TOperators.ParseBreak: boolean;
begin
  Result := Any([Keyword(['end', 'exceptions'])]);
end;

function TOperators.Name: string;
begin
  Result := '< statements >';
end;

{ TIf }

function TIf.InternalParse: boolean;
begin
  _If := Keyword('if');
  if not Assigned(_If) then exit(false);
  TExpression.Parse(Self, Source, _Condition);
  _Then := Keyword('then');
  TIfOperators.Parse(Self, Source, _ThenStatements);
  _Else := Keyword('else');
  if Assigned(_Else) then TIfOperators.Parse(Self, Source, _ElseStatements);
  _EndIf := Keyword('end if');
  Result := true;
  inherited;
end;

function TIf.Name: string;
begin
  Result := 'if';
end;

procedure TIf.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_If);
  APrinter.Space;
  APrinter.PrintItem(_Condition);
  APrinter.Space;
  APrinter.PrintItem(_Then);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_ThenStatements);
  APrinter.Undent;
  APrinter.PrintItem(_Else);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_ElseStatements);
  APrinter.Undent;
  APrinter.PrintItem(_EndIf);
  APrinter.Space;
  inherited;
end;

{ TIfOperators }

function TIfOperators.ParseBreak: boolean;
begin
  Result := Any([Keyword(['else', 'elsif', 'end if'])]) or inherited ParseBreak;
end;

end.
