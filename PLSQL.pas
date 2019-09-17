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
  Printers_;

type

  { Конструкция package [body] }
  TPackage = class(TStatement)
  strict private
    _Package, _Body: TKeyword;
    _PackageName: TIdent;
    _Is: TKeyword;
    _Statements: TList<TStatement>;
    _End: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Name: string; override;
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Завершение конструкции package }
  TEndOfUnit = class(TStatement)
  strict private
    _End: TKeyword;
    _PackageName: TIdent;
    _Semicolon, _Slash: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    function Name: string; override;
    procedure PrintSelf(APrinter: TPrinter); override;
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
  TParamsDeclaration = class(TStatement)
  strict private
    _OpenBracket: TTerminal;
    _Params: TList<TStatement>;
    _CloseBracket: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
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

  { Подпрограмма }
  TSubroutine = class(TStatement)
  strict private
    _Initial: TKeyword;
    _Name: TIdent;
    _Params: TStatement;
    _Return: TKeyword;
    _ReturnType: TStatement;
    _Deterministic: TKeyword;
    _Semicolon: TTerminal;
    _Is: TKeyword;
    _Body: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function Name: string; override;
  end;

  { Тело подпрограммы }
  TSubroutineBody = class(TStatement)
  strict private
    _Declarations: TStatement;
    _Begin: TKeyword;
    _Statements: TList<TStatement>;
    _Exceptions: TKeyword;
    _ExceptionHandlers: TList<TStatement>;
    _End: TKeyword;
    _Semicolon: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure PrintSelf(APrinter: TPrinter); override;
    function Name: string; override;
  end;

  { Блок деклараций }
  TDeclarations = class(TStatement)
  strict private
    _Declarations: TList<TStatement>;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure PrintSelf(APrinter: TPrinter); override;
    function Name: string; override;
  end;

  { Блок переменных }
  TVariableDeclarations = class(TStatement)
  strict private
    _Variables: TList<TStatement>;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure PrintSelf(APrinter: TPrinter); override;
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
    _Ident: TStatement;
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

implementation

{ TPackage }

procedure TPackage.AfterConstruction;
begin
  inherited;
  _Statements := TList<TStatement>.Create;
end;

procedure TPackage.BeforeDestruction;
begin
  FreeAndNil(_Statements);
  inherited;
end;

function TPackage.InternalParse: boolean;
var Statement: TStatement;
begin
  { Если распознали слово package, то распознали конструкцию }
  _Package := Keyword('package');
  if not Assigned(_Package) then exit(false);
  Result := true;
  { Проверим наличие body }
  _Body := Keyword('body');
  { Проверим название пакета }
  _PackageName := Identifier;
  if not Assigned(_PackageName) then exit;
  { Проверим наличие is }
  _Is := Keyword(['is', 'as']);
  { И теперь будем парсить конструкции, пока не доберёмся до end-а }
  while not TEndOfUnit.Parse(Self, Source, Statement) do
    if TDeclarations.Parse(Self, Source, Statement) or TUnexpectedToken.Parse(Self, Source, Statement)
      then _Statements.Add(Statement)
      else exit;
  { Добрались }
  _End := Statement as TEndOfUnit;
end;

function TPackage.Name: string;
begin
  Result := 'package';
  if Assigned(_Body) then Result := Result + ' body';
  if Assigned(_PackageName) then Result := Result + ' ' + _PackageName.Value;
end;

procedure TPackage.PrintSelf(APrinter: TPrinter);
var i: integer;
begin
  APrinter.PrintItem(_Package);
  APrinter.Space;
  APrinter.PrintItem(_Body);
  APrinter.Space;
  APrinter.PrintItem(_PackageName);
  APrinter.Space;
  APrinter.PrintItem(_Is);
  APrinter.Space;
  APrinter.NextLine;
  APrinter.Indent;
  for i := 0 to _Statements.Count - 1 do APrinter.PrintItem(_Statements[i]);
  APrinter.Undent;
  APrinter.PrintItem(_End);
end;

{ TEndOfUnit }

function TEndOfUnit.InternalParse: boolean;
begin
  { Проверим end }
  _End := Keyword('end');
  if not Assigned(_End) then exit(false);
  { Необязательное название пакета }
  _PackageName := Identifier;
  { Проверим точку с запятой }
  _Semicolon := Terminal(';');
  if not Assigned(_Semicolon) then exit(false);
  { Проверим слеш }
  _Slash := Terminal('/');
  Result := Assigned(_Slash);
end;

function TEndOfUnit.Name: string;
begin
  Result := 'end';
end;

procedure TEndOfUnit.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_End);
  APrinter.Space;
  APrinter.PrintItem(_PackageName);
  APrinter.PrintItem(_Semicolon);
  APrinter.NextLine;
  APrinter.PrintItem(_Slash);
  APrinter.NextLine;
end;

{ TSubroutine }

function TSubroutine.InternalParse: boolean;
var Param: TStatement;
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
  { Если завершается точкой с запятой - значит, описание }
  _Semicolon := Terminal(';');
  if Assigned(_Semicolon) then exit;
  { Если завершается is или as, значит определение }
  _Is := Keyword(['is', 'as']);
  if Assigned(_Is) then TSubroutineBody.Parse(Self, Source, _Body);
end;

function TSubroutine.Name: string;
begin
  Result := _Initial.Value + ' ' + _Name.Value;
end;

procedure TSubroutine.PrintSelf(APrinter: TPrinter);
var i: integer;
begin
  APrinter.PrintItem(_Initial);
  APrinter.Space;
  APrinter.PrintItem(_Name);
  APrinter.Space;
  if Assigned(_Params) then
  begin
    APrinter.NextLine;
    APrinter.Indent;
    APrinter.PrintItem(_Params);
  end;
  if Assigned(_Return) then APrinter.NextLine;
  APrinter.PrintItem(_Return);
  APrinter.Space;
  APrinter.PrintItem(_ReturnType);
  APrinter.Space;
  APrinter.PrintItem(_Deterministic);
  APrinter.Space;
  if Assigned(_Semicolon) then
  begin
    APrinter.SupressSpace;
    APrinter.PrintItem(_Semicolon);
  end;
  APrinter.NextLine;
  if Assigned(_Params) then APrinter.Undent;
  if Assigned(_Is) then
  begin
    APrinter.PrintItem(_Is);
    APrinter.NextLine;
    if _Body is TSubroutineBody then APrinter.Indent;
    APrinter.PrintItem(_Body);
  end;
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
  Result := 'type reference';
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
  Result := 'param';
  if Assigned(_ParamName) then Result := Result + ' ' + _ParamName.Value;
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

procedure TParamsDeclaration.AfterConstruction;
begin
  inherited;
  _Params := TList<TStatement>.Create;
end;

procedure TParamsDeclaration.BeforeDestruction;
begin
  FreeAndNil(_Params);
  inherited;
end;

function TParamsDeclaration.InternalParse: boolean;
var _Param: TStatement;
begin
  { Если нашли открывающую скобку - всё вплоть до закрывающей считаем параметрами }
  _OpenBracket := Terminal('(');
  if not Assigned(_OpenBracket) then exit(false);
  Result := true;
  while not Assigned(_CloseBracket) do
  begin
    _CloseBracket := Terminal(')');
    if Assigned(_CloseBracket) then
      break
    else if TParamDeclaration.Parse(Self, Source, _Param) then
      _Params.Add(_Param)
    else if TUnexpectedToken.Parse(Self, Source, _Param) then
      _Params.Add(_Param)
    else
      exit(true);
  end;
end;

function TParamsDeclaration.Name: string;
begin
  Result := 'parameters';
end;

procedure TParamsDeclaration.PrintSelf(APrinter: TPrinter);
var
  i: integer;
  MultiLine: boolean;
begin
  MultiLine := (_Params.Count > 0);
  APrinter.PrintItem(_OpenBracket);
  if MultiLine then
  begin
    APrinter.NextLine;
    APrinter.Indent;
  end;
  for i := 0 to _Params.Count - 1 do
  begin
    APrinter.PrintItem(_Params[i]);
    if MultiLine then APrinter.NextLine;
  end;
  if MultiLine then APrinter.Undent;
  APrinter.PrintItem(_CloseBracket);
end;

function TParamsDeclaration.ParamNameMax: integer;
var i: integer;
begin
  Result := 0;
  for i := 0 to _Params.Count - 1 do
    if _Params[i] is TParamDeclaration then
      Result := Math.Max(Result, TParamDeclaration(_Params[i]).ParamNameLen);
end;

function TParamsDeclaration.ModifiersMax: integer;
var i: integer;
begin
  Result := 0;
  for i := 0 to _Params.Count - 1 do
    if _Params[i] is TParamDeclaration then
      Result := Math.Max(Result, TParamDeclaration(_Params[i]).ModifiersLen);
end;

{ TSubroutineBody }

procedure TSubroutineBody.AfterConstruction;
begin
  inherited;
  _Statements   := TList<TStatement>.Create;
  _ExceptionHandlers := TList<TStatement>.Create;
end;

procedure TSubroutineBody.BeforeDestruction;
begin
  inherited;
  FreeAndNil(_Statements);
  FreeAndNil(_ExceptionHandlers);
end;

function TSubroutineBody.InternalParse: boolean;
var _Stmt: TStatement;
begin
  { Тело считаем распознанным, если успешно добрались до begin }
  TDeclarations.Parse(Self, Source, _Declarations);
  _Begin := Keyword('begin');
  if not Assigned(_Begin) then exit(false);
  Result := true;
  { И сканируем всё до end-а }
  repeat
    _End := Keyword('end');
    if Assigned(_End) then break;
    if TOperator.ParseOperator(Self, Source, _Stmt) then
      _Statements.Add(_Stmt)
    else if TUnexpectedToken.Parse(Self, Source, _Stmt) then
      _Statements.Add(_Stmt)
    else
      exit;
  until false;
  _Semicolon := Terminal(';');
end;

function TSubroutineBody.Name: string;
begin
  Result := 'body';
end;

procedure TSubroutineBody.PrintSelf(APrinter: TPrinter);
var i: integer;
begin
  APrinter.PrintItem(_Declarations);
  APrinter.Undent;
  APrinter.PrintItem(_Begin);
  APrinter.NextLine;
  APrinter.Indent;
  for i := 0 to _Statements.Count - 1 do
    APrinter.PrintItem(_Statements[i]);
  APrinter.Undent;
  APrinter.PrintItem(_Exceptions);
  APrinter.Indent;
  for i := 0 to _ExceptionHandlers.Count - 1 do
    APrinter.PrintItem(_ExceptionHandlers[i]);
  APrinter.Undent;
  APrinter.PrintItem(_End);
  APrinter.PrintItem(_Semicolon);
  APrinter.NextLine;
end;

{ TDeclarations }

procedure TDeclarations.AfterConstruction;
begin
  inherited;
  _Declarations := TList<TStatement>.Create;
end;

procedure TDeclarations.BeforeDestruction;
begin
  inherited;
  FreeAndNil(_Declarations);
end;

function TDeclarations.InternalParse: boolean;
var _Statement: TStatement;
begin
  while TVariableDeclarations.Parse(Self, Source, _Statement) or
        TSubroutine.Parse(Self, Source, _Statement)
  do
    _Declarations.Add(_Statement);
  Result := (_Declarations.Count > 0);
end;

function TDeclarations.Name: string;
begin
  Result := 'declarations';
end;

procedure TDeclarations.PrintSelf(APrinter: TPrinter);
var i: integer;
begin
  for i := 0 to _Declarations.Count - 1 do
    APrinter.PrintItem(_Declarations[i]);
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

procedure TVariableDeclarations.AfterConstruction;
begin
  inherited;
  _Variables := TList<TStatement>.Create;
end;

procedure TVariableDeclarations.BeforeDestruction;
begin
  inherited;
  FreeAndNil(_Variables);
end;

function TVariableDeclarations.InternalParse: boolean;
var _Var: TStatement;
begin
  while TVariableDeclaration.Parse(Self, Source, _Var) do
    _Variables.Add(_Var);
  Result := (_Variables.Count > 0);
end;

function TVariableDeclarations.MaxNameLen: integer;
var i: integer;
begin
  Result := 0;
  for i := 0 to _Variables.Count - 1 do
    if _Variables[i] is TVariableDeclaration then
      Result := Math.Max(Result, TVariableDeclaration(_Variables[i]).NameLen);
end;

function TVariableDeclarations.Name: string;
begin
  Name := 'variables';
end;

procedure TVariableDeclarations.PrintSelf(APrinter: TPrinter);
var i: integer;
begin
  for i := 0 to _Variables.Count - 1 do
    APrinter.PrintItem(_Variables[i]);
end;

{ TOperator }

class function TOperator.ParseOperator(AParent: TStatement;
  Tokens: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result :=
    TAssignment.Parse(AParent, Tokens, AResult) or
    TReturn.Parse(AParent, Tokens, AResult) or
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
  TQualifiedIdentifier.Parse(Self, Source, _Ident);
  _Assignment := Terminal(':=');
  Result := Assigned(_Ident) and Assigned(_Assignment);
  TExpression.Parse(Self, Source, _Expression);
  inherited;
end;

function TAssignment.Name: string;
begin
  Result := 'assignment';
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

end.
