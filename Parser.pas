////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                          �������������� ����������                         //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Parser;

interface

uses System.SysUtils, System.Generics.Collections, Streams, Tokens, TreePrinter;

type

  { ������� ����� �������������� ����������� }
  TStatement = class
  strict protected
    Source: TBufferedStream<TToken>;
    function InternalParse: boolean; virtual; abstract;
    procedure InternalDescribe(APrinter: TTreePrinter); virtual;
    function NextToken: TToken;
    function Keyword(const AKeyword: string): TKeyword;
    function Identifier: TIdent;
    function Terminal(const ATerminal: string): TTerminal;
  public
    class function Parse(Tokens: TBufferedStream<TToken>; out AResult: TStatement): boolean;
  public
    constructor Create(ASource: TBufferedStream<TToken>);
    function Name: string; virtual;
    procedure Describe(APrinter: TTreePrinter);
  end;

  { �������������� ���������� }
  TParser = class(TNextStream<TToken, TStatement>)
  strict protected
    function InternalNext: TStatement; override;
  end;

  { ����������� ������� - ����� ��� �����������, ������� �� ������� ��������� }
  TUnexpectedToken = class(TStatement)
  strict private
    Token: TToken;
  strict protected
    function InternalParse: boolean; override;
  public
    function Name: string; override;
  end;

  { ������� create [or replace] }
  TCreateStatement = class(TStatement)
  strict private
    _Create, _Or, _Replace, _Force: TKeyword;
    What: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalDescribe(APrinter: TTreePrinter); override;
  public
    function Name: string; override;
  end;

  { ���������� ����������� package }
  TPackageStatementEnd = class(TStatement)
  strict private
    _End: TKeyword;
    _PackageName: TIdent;
    _Semicolon, _Slash: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalDescribe(APrinter: TTreePrinter); override;
  public
    function Name: string; override;
  end;

  { ����������� package [body] }
  TPackageStatement = class(TStatement)
  strict private
    _Package, _Body: TKeyword;
    _PackageName: TIdent;
    _Is: TKeyword;
    _Statements: TList<TStatement>;
    _End: TPackageStatementEnd;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalDescribe(APrinter: TTreePrinter); override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Name: string; override;
  end;

  { �������� ���� ������ }
  TTypeRefStatement = class(TStatement)
  strict private
    _Ident: TIdent;
    _Dot: TTerminal;
    _SecondIdent: TIdent;
    _Type: TTerminal;
    _RowType: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalDescribe(APrinter: TTreePrinter); override;
  public
    function Name: string; override;
  end;

  { ����������� procedure }
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
    procedure InternalDescribe(APrinter: TTreePrinter); override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Name: string; override;
  end;

implementation

{ TParser }

function TParser.InternalNext: TStatement;
begin
  if not TCreateStatement.Parse(Source, Result) and
     not TProcedureStatement.Parse(Source, Result) then
    TUnexpectedToken.Parse(Source, Result);
end;

{ TUnexpectedToken }

function TUnexpectedToken.Name: string;
begin
  Result := Format('*** ����������� ����������� *** [%s ''%s'']', [Token.TokenType, Token.Value]);
end;

function TUnexpectedToken.InternalParse: boolean;
begin
  Token := NextToken;
  Result := true;
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

function TStatement.Name: string;
begin
  Result := Self.ClassName;
end;

procedure TStatement.Describe(APrinter: TTreePrinter);
begin
  APrinter.PrintNode(Name);
  APrinter.BeginChildren;
  InternalDescribe(APrinter);
  APrinter.EndChildren;
end;

procedure TStatement.InternalDescribe(APrinter: TTreePrinter);
begin
  { ������ �� ������ }
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
  { ���� ���������� ����� create, �� ���������� ����������� }
  _Create := Keyword('create');
  if not Assigned(_Create) then exit(false);
  { �������� ������� or replace }
  _Or := Keyword('or');
  if Assigned(_Or) then _Replace := Keyword('replace');
  if Assigned(_Or) and not Assigned(_Replace) then exit(true);
  { �������� ������� force }
  _Force := Keyword('force');
  { �, �������, ����������, ��� �� �� ������ }
  TPackageStatement.Parse(Source, What);
  Result := true;
end;

function TCreateStatement.Name: string;
begin
  Result := 'create';
  if Assigned(What) then Result := Result + ' ' + What.Name;
end;

procedure TCreateStatement.InternalDescribe(APrinter: TTreePrinter);
begin
  APrinter.PrintNode(_Create);
  APrinter.PrintNode(_Or);
  APrinter.PrintNode(_Replace);
  if Assigned(What) then What.Describe(APrinter);
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
  { ���� ���������� ����� package, �� ���������� ����������� }
  _Package := Keyword('package');
  if not Assigned(_Package) then exit(false);
  Result := true;
  { �������� ������� body }
  _Body := Keyword('body');
  { �������� �������� ������ }
  _PackageName := Identifier;
  if not Assigned(_PackageName) then exit;
  { �������� ������� is }
  _Is := Keyword('is');
  { � ������ ����� ������� �����������, ���� �� �������� �� end-� }
  while not TPackageStatementEnd.Parse(Source, Statement) do
    if TProcedureStatement.Parse(Source, Statement) or
       TUnexpectedToken.Parse(Source, Statement) then
      _Statements.Add(Statement);
  { ��������� }
  _End := Statement as TPackageStatementEnd;
end;

function TPackageStatement.Name: string;
begin
  Result := 'package';
  if Assigned(_Body) then Result := Result + ' body';
  if Assigned(_PackageName) then Result := Result + ' ' + _PackageName.Value;
end;

procedure TPackageStatement.InternalDescribe(APrinter: TTreePrinter);
var i: integer;
begin
  APrinter.PrintNode(_Package);
  APrinter.PrintNode(_Body);
  APrinter.PrintNode(_PackageName);
  APrinter.PrintNode(_Is);
  for i := 0 to _Statements.Count - 1 do _Statements[i].Describe(APrinter);
  if Assigned(_End) then _End.Describe(APrinter);
end;

{ TPackageStatementEnd }

function TPackageStatementEnd.InternalParse: boolean;
begin
  { �������� end }
  _End := Keyword('end');
  if not Assigned(_End) then exit(false);
  { �������������� �������� ������ }
  _PackageName := Identifier;
  { �������� ����� � ������� }
  _Semicolon := Terminal(';');
  if not Assigned(_Semicolon) then exit(false);
  { �������� ���� }
  _Slash := Terminal('/');
  Result := Assigned(_Slash);
end;

function TPackageStatementEnd.Name: string;
begin
  Result := 'end';
end;

procedure TPackageStatementEnd.InternalDescribe(APrinter: TTreePrinter);
begin
  APrinter.PrintNode(_End);
  APrinter.PrintNode(_PackageName);
  APrinter.PrintNode(_Semicolon);
  APrinter.PrintNode(_Slash);
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
  { �������� procedure/function }
  _Procedure := Keyword('procedure');
  _Function  := Keyword('function');
  if not Assigned(_Procedure) and not Assigned(_Function) then exit(false);
  Result := true;
  { �������� ��������� � ��������� }
  _ProcedureName := Identifier;
  if not Assigned(_ProcedureName) then exit;
  _OpenBracket := Terminal('(');
  if Assigned(_OpenBracket) then
    repeat
      _CloseBracket := Terminal(')');
      if not Assigned(_CloseBracket) and TUnexpectedToken.Parse(Source, Param) then
        _Params.Add(Param);
    until Assigned(_CloseBracket);
  { ������������ �������� }
  _Return := Keyword('return');
  TTypeRefStatement.Parse(Source, _ReturnType);
  { ���� ����������� ������ � ������� - ������, ���������� }
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

procedure TProcedureStatement.InternalDescribe(APrinter: TTreePrinter);
var i: integer;
begin
  APrinter.PrintNode(_Procedure);
  APrinter.PrintNode(_Function);
  APrinter.PrintNode(_ProcedureName);
  APrinter.PrintNode(_OpenBracket);
  for i := 0 to _Params.Count - 1 do
    _Params[i].Describe(APrinter);
  APrinter.PrintNode(_CloseBracket);
  APrinter.PrintNode(_Return);
  if Assigned(_ReturnType) then _ReturnType.Describe(APrinter);
  APrinter.PrintNode(_Semicolon);
end;

{ TTypeRefStatement }

function TTypeRefStatement.InternalParse: boolean;
begin
  { ���� ���������� �������������, ��� ������ ��������� }
  _Ident := Identifier;
  if not Assigned(_Ident) then exit(false);
  Result := true;
  { �������� �����, ������ ������������� � %[row]type }
  _Dot := Terminal('.');
  if Assigned(_Dot) then _SecondIdent := Identifier;
  _Type := Terminal('%type');
  _RowType := Terminal('%rowtype');
end;

function TTypeRefStatement.Name: string;
begin
  Result := 'type reference';
end;

procedure TTypeRefStatement.InternalDescribe(APrinter: TTreePrinter);
begin
  APrinter.PrintNode(_Ident);
  APrinter.PrintNode(_Dot);
  APrinter.PrintNode(_SecondIdent);
  APrinter.PrintNode(_Type);
  APrinter.PrintNode(_RowType);
end;

end.

