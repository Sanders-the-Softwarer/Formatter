////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                          Синтаксический анализатор                         //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Parser;

interface

uses System.SysUtils, System.Generics.Collections, Streams, Tokens, Printers_;

type

  { Базовый класс синтаксических конструкций }
  TStatement = class
  strict protected
    Source: TBufferedStream<TToken>;
    function InternalParse: boolean; virtual; abstract;
    function NextToken: TToken;
    function Keyword(const AKeyword: string): TKeyword;
    function Identifier: TIdent;
    function Number: TNumber;
    function Literal: TLiteral;
    function Terminal(const ATerminal: string): TTerminal;
  public
    class function Parse(Tokens: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class procedure ParseAnyTarget(Tokens: TBufferedStream<TToken>; out AResult: TStatement);
  public
    constructor Create(ASource: TBufferedStream<TToken>);
    function Name: string; virtual;
    procedure PrintSelf(APrinter: TPrinter); virtual; abstract;
  end;

  { Синтаксический анализатор }
  TParser = class(TNextStream<TToken, TStatement>)
  strict protected
    function InternalNext: TStatement; override;
  end;

  { Неожиданная лексема - класс для конструкций, которые не удалось разобрать }
  TUnexpectedToken = class(TStatement)
  strict private
    Token: TToken;
  strict protected
    function InternalParse: boolean; override;
  public
    function Name: string; override;
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Команда create [or replace] }
  TCreateStatement = class(TStatement)
  strict private
    _Create, _Or, _Replace, _Force: TKeyword;
    _What: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    function Name: string; override;
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Завершение конструкции package }
  TPackageStatementEnd = class(TStatement)
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

  { Конструкция package [body] }
  TPackageStatement = class(TStatement)
  strict private
    _Package, _Body: TKeyword;
    _PackageName: TIdent;
    _Is: TKeyword;
    _Statements: TList<TStatement>;
    _End: TPackageStatementEnd;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Name: string; override;
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Название типа данных }
  TTypeRefStatement = class(TStatement)
  strict private
    _Ident: TIdent;
    _Dot: TTerminal;
    _SecondIdent: TIdent;
    _Type: TTerminal;
    _RowType: TTerminal;
    _OpenBracket: TTerminal;
    _Size: TNumber;
    _CloseBracket: TTerminal;
    _Default: TKeyword;
    _Value: TToken;
  strict protected
    function InternalParse: boolean; override;
  public
    function Name: string; override;
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Подпрограмма }
  TProcedureStatement = class(TStatement)
  strict private
    _Procedure, _Function: TKeyword;
    _ProcedureName: TIdent;
    _OpenBracket: TTerminal;
    _Params: TList<TStatement>;
    _CloseBracket: TTerminal;
    _Return: TKeyword;
    _ReturnType: TStatement;
    _Semicolon: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Name: string; override;
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
    _Comma: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    function Name: string; override;
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

implementation

{ TParser }

function TParser.InternalNext: TStatement;
begin
  TStatement.ParseAnyTarget(Source, Result);
end;

{ TUnexpectedToken }

function TUnexpectedToken.Name: string;
begin
  Result := Format('*** НЕОЖИДАННАЯ КОНСТРУКЦИЯ *** [%s ''%s'']', [Token.TokenType, Token.Value]);
end;

function TUnexpectedToken.InternalParse: boolean;
begin
  Token := NextToken;
  Result := true;
end;

procedure TUnexpectedToken.PrintSelf(APrinter: TPrinter);
begin
  APrinter.Space;
  APrinter.PrintItem(Token);
  APrinter.NextLine;
end;

{ TStatement }

constructor TStatement.Create(ASource: TBufferedStream<TToken>);
begin
  Source := ASource;
end;

class function TStatement.Parse(Tokens: TBufferedStream<TToken>; out AResult: TStatement): boolean;
var SavedPosition: TMark;
begin
  AResult := Self.Create(Tokens);
  SavedPosition := Tokens.Mark;
  Result := AResult.InternalParse;
  if not Result then
  begin
    Tokens.Restore(SavedPosition);
    FreeAndNil(AResult);
  end;
end;

class procedure TStatement.ParseAnyTarget(Tokens: TBufferedStream<TToken>; out AResult: TStatement);
begin
  if not TCreateStatement.Parse(Tokens, AResult) and
     not TPackageStatement.Parse(Tokens, AResult) and
     not TProcedureStatement.Parse(Tokens, AResult)
    then TUnexpectedToken.Parse(Tokens, AResult);
end;

function TStatement.Name: string;
begin
  Result := Self.ClassName;
end;

function TStatement.NextToken: TToken;
begin
  if Source.Eof
    then Result := UnexpectedEOF
    else Result := Source.Next;
end;

function TStatement.Keyword(const AKeyword: string): TKeyword;
var
  Token: TToken;
  P: TMark;
begin
  Result := nil;
  P := Source.Mark;
  Token := NextToken;
  if (Token is TKeyword) and SameText(Token.Value, AKeyword)
    then Result := Token as TKeyword
    else Source.Restore(P);
end;

function TStatement.Identifier: TIdent;
var
  Token: TToken;
  P: TMark;
begin
  Result := nil;
  P := Source.Mark;
  Token := NextToken;
  if Token is TIdent
    then Result := Token as TIdent
    else Source.Restore(P);
end;

function TStatement.Number: TNumber;
var
  Token: TToken;
  P: TMark;
begin
  Result := nil;
  P := Source.Mark;
  Token := NextToken;
  if Token is TNumber
    then Result := Token as TNumber
    else Source.Restore(P);
end;

function TStatement.Literal: TLiteral;
var
  Token: TToken;
  P: TMark;
begin
  Result := nil;
  P := Source.Mark;
  Token := NextToken;
  if Token is TLiteral
    then Result := Token as TLiteral
    else Source.Restore(P);
end;

function TStatement.Terminal(const ATerminal: string): TTerminal;
var
  Token: TToken;
  P: TMark;
begin
  Result := nil;
  P := Source.Mark;
  Token := NextToken;
  if (Token is TTerminal) and SameText(ATerminal, Token.Value)
    then Result := Token as TTerminal
    else Source.Restore(P);
end;

{ TCreateStatement }

function TCreateStatement.InternalParse: boolean;
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
  TPackageStatement.Parse(Source, _What);
  Result := true;
end;

function TCreateStatement.Name: string;
begin
  Result := 'create';
  if Assigned(_What) then Result := Result + ' ' + _What.Name;
end;

procedure TCreateStatement.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Create);
  APrinter.Space;
  APrinter.PrintItem(_Or);
  APrinter.Space;
  APrinter.PrintItem(_Replace);
  APrinter.Space;
  APrinter.PrintItem(_What);
end;

{ TPackageStatement }

procedure TPackageStatement.AfterConstruction;
begin
  inherited;
  _Statements := TList<TStatement>.Create;
end;

procedure TPackageStatement.BeforeDestruction;
begin
  FreeAndNil(_Statements);
  inherited;
end;

function TPackageStatement.InternalParse: boolean;
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
  _Is := Keyword('is');
  { И теперь будем парсить конструкции, пока не доберёмся до end-а }
  while not TPackageStatementEnd.Parse(Source, Statement) do
  begin
    TStatement.ParseAnyTarget(Source, Statement);
    _Statements.Add(Statement);
  end;
  { Добрались }
  _End := Statement as TPackageStatementEnd;
end;

function TPackageStatement.Name: string;
begin
  Result := 'package';
  if Assigned(_Body) then Result := Result + ' body';
  if Assigned(_PackageName) then Result := Result + ' ' + _PackageName.Value;
end;

procedure TPackageStatement.PrintSelf(APrinter: TPrinter);
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

{ TPackageStatementEnd }

function TPackageStatementEnd.InternalParse: boolean;
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

function TPackageStatementEnd.Name: string;
begin
  Result := 'end';
end;

procedure TPackageStatementEnd.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_End);
  APrinter.Space;
  APrinter.PrintItem(_PackageName);
  APrinter.PrintItem(_Semicolon);
  APrinter.NextLine;
  APrinter.PrintItem(_Slash);
  APrinter.NextLine;
end;

{ TProcedureStatement }

procedure TProcedureStatement.AfterConstruction;
begin
  inherited;
  _Params := TList<TStatement>.Create;
end;

procedure TProcedureStatement.BeforeDestruction;
begin
  inherited;
  FreeAndNil(_Params);
end;

function TProcedureStatement.InternalParse: boolean;
var Param: TStatement;
begin
  { Проверим procedure/function }
  _Procedure := Keyword('procedure');
  _Function  := Keyword('function');
  if not Assigned(_Procedure) and not Assigned(_Function) then exit(false);
  Result := true;
  { Название процедуры и параметры }
  _ProcedureName := Identifier;
  if not Assigned(_ProcedureName) then exit;
  _OpenBracket := Terminal('(');
  if Assigned(_OpenBracket) then
    repeat
      _CloseBracket := Terminal(')');
      if Assigned(_CloseBracket) then break;
      if TParamDeclaration.Parse(Source, Param) or
         TUnexpectedToken.Parse(Source, Param) then
        _Params.Add(Param);
    until false;
  { Возвращаемое значение }
  _Return := Keyword('return');
  TTypeRefStatement.Parse(Source, _ReturnType);
  { Если завершается точкой с запятой - значит, декларация }
  _Semicolon := Terminal(';');
  if Assigned(_Semicolon) then exit;
end;

function TProcedureStatement.Name: string;
begin
  if Assigned(_Procedure) then
    Result := 'procedure'
  else if Assigned(_Function) then
    Result := 'function'
  else
    Result := '<<<subroutine>>>';
  if Assigned(_ProcedureName) then
    Result := Result + ' ' + _ProcedureName.Value;
end;

procedure TProcedureStatement.PrintSelf(APrinter: TPrinter);
var i: integer;
begin
  APrinter.PrintItem(_Procedure);
  APrinter.PrintItem(_Function);
  APrinter.Space;
  APrinter.PrintItem(_ProcedureName);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_OpenBracket);
  APrinter.NextLine;
  APrinter.Indent;
  for i := 0 to _Params.Count - 1 do
  begin
    APrinter.PrintItem(_Params[i]);
    APrinter.NextLine;
  end;
  APrinter.Undent;
  APrinter.PrintItem(_CloseBracket);
  APrinter.Space;
  APrinter.PrintItem(_Return);
  APrinter.Space;
  APrinter.PrintItem(_ReturnType);
  APrinter.PrintItem(_Semicolon);
  APrinter.NextLine;
  APrinter.Undent;
end;

{ TTypeRefStatement }

function TTypeRefStatement.InternalParse: boolean;
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
    _CloseBracket := Terminal(')');
  end;
  { Проверим значение по умолчанию }
  _Default := Keyword('default');
  if Assigned(_Default) then
  begin
    _Value := Identifier;
    if not Assigned(_Value) then _Value := Number;
    if not Assigned(_Value) then _Value := Literal;
  end;
end;

function TTypeRefStatement.Name: string;
begin
  Result := 'type reference';
end;

procedure TTypeRefStatement.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Ident);
  APrinter.PrintItem(_Dot);
  APrinter.PrintItem(_SecondIdent);
  APrinter.PrintItem(_Type);
  APrinter.PrintItem(_RowType);
  APrinter.PrintItem(_OpenBracket);
  APrinter.PrintItem(_Size);
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
  TTypeRefStatement.Parse(Source, _ParamType);
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
  APrinter.PrintItem(_ParamName);
  APrinter.Space;
  APrinter.PrintItem(_In);
  APrinter.Space;
  APrinter.PrintItem(_Out);
  APrinter.Space;
  APrinter.PrintItem(_Nocopy);
  APrinter.Space;
  APrinter.PrintItem(_ParamType);
  APrinter.PrintItem(_Comma);
end;

end.

