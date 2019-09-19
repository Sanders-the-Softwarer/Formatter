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

uses Windows, System.SysUtils, System.Generics.Collections, Math, Streams, Tokens, Printers_;

type
  TParserSettings = class
  public
    DeclarationSingleLineParamLimit: integer;
    ArgumentSingleLineParamLimit: integer;
  end;

type

  { Базовый класс синтаксических конструкций }
  TStatement = class
  strict private
    FParent: TStatement;
    FSettings: TParserSettings;
    function GetSettings: TParserSettings;
  strict protected
    Source: TBufferedStream<TToken>;
    function InternalParse: boolean; virtual; abstract;
    function NextToken: TToken;
    function Keyword(const AKeyword: string): TKeyword; overload;
    function Keyword(const AKeywords: array of string): TKeyword; overload;
    function Identifier: TIdent;
    function Number: TNumber;
    function Literal: TLiteral;
    function Terminal(const ATerminal: string): TTerminal; overload;
    function Terminal(const ATerminals: array of string): TTerminal; overload;
  public
    class function Parse(AParent: TStatement; Tokens: TBufferedStream<TToken>; out AResult: TStatement): boolean;
  public
    constructor Create(AParent: TStatement; ASource: TBufferedStream<TToken>);
    function Name: string; virtual;
    procedure PrintSelf(APrinter: TPrinter); virtual; abstract;
    property Parent: TStatement read FParent;
    property Settings: TParserSettings read GetSettings write FSettings;
  end;

  TStatementClass = class of TStatement;

  { "Пустое" выражение }
  TEOFStatement = class(TStatement)
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Базовый класс для списков однотипных конструкций (переменные, операторы и т. п. }
  TStatementList = class(TStatement)
  strict private
    FList: TList<TStatement>;
  strict protected
    function InternalParse: boolean; override;
    function ParseStatement(out AResult: TStatement): boolean; virtual; abstract;
    function ParseBreak: boolean; virtual; abstract;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure PrintSelf(APrinter: TPrinter); override;
    function Count: integer;
    function Item(Index: integer): TStatement;
    function Any(Found: array of TObject): boolean;
  end;

  { Синтаксический анализатор }
  TParser = class(TNextStream<TToken, TStatement>)
  strict private
    Settings: TParserSettings;
  strict protected
    function InternalNext: TStatement; override;
  public
    constructor Create(AStream: TBufferedStream<TToken>; ASettings: TParserSettings);
  end;

  { Неожиданная лексема - класс для конструкций, которые не удалось разобрать }
  TUnexpectedToken = class(TStatement)
  strict private
    _Token: TToken;
  strict protected
    function InternalParse: boolean; override;
  public
    function Name: string; override;
    procedure PrintSelf(APrinter: TPrinter); override;
    property Token: TToken read _Token;
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

implementation

uses DML, PLSQL;

{ TParser }

constructor TParser.Create(AStream: TBufferedStream<TToken>; ASettings: TParserSettings);
begin
  inherited Create(AStream);
  Settings := ASettings;
end;

function TParser.InternalNext: TStatement;
begin
  if TCreateStatement.Parse(nil, Source, Result) or
     TPackage.Parse(nil, Source, Result) or
     TSubroutine.Parse(nil, Source, Result) or
     TAnonymousBlock.Parse(nil, Source, Result) or
     TDML.Parse(nil, Source, Result) or
     TUnexpectedToken.Parse(nil, Source, Result)
    then Result.Settings := Self.Settings
    else Result := TEOFStatement.Create(nil, Source);
end;

{ TUnexpectedToken }

function TUnexpectedToken.Name: string;
begin
  try
    Result := Format('*** НЕОЖИДАННАЯ КОНСТРУКЦИЯ *** [%s ''%s'']', [Token.TokenType, Token.Value]);
  except
    Result := '*** НЕОЖИДАННАЯ КОНСТРУКЦИЯ ***';
  end;
end;

function TUnexpectedToken.InternalParse: boolean;
begin
  _Token := NextToken;
  Result := not (_Token is TUnexpectedEOF);
end;

procedure TUnexpectedToken.PrintSelf(APrinter: TPrinter);
begin
  APrinter.Space;
  APrinter.PrintItem(_Token);
  APrinter.NextLine;
end;

{ TStatement }

constructor TStatement.Create(AParent: TStatement; ASource: TBufferedStream<TToken>);
begin
  FParent := AParent;
  Source := ASource;
end;

class function TStatement.Parse(AParent: TStatement; Tokens: TBufferedStream<TToken>; out AResult: TStatement): boolean;
var SavedPosition: TMark; S: string;
begin
  AResult := Self.Create(AParent, Tokens);
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

function TStatement.NextToken: TToken;
begin
  if Source.Eof
    then Result := UnexpectedEOF
    else Result := Source.Next;
end;

function TStatement.Keyword(const AKeyword: string): TKeyword;
begin
  Result := Keyword([AKeyword]);
end;

function TStatement.Keyword(const AKeywords: array of string): TKeyword;
var
  Token: TToken;
  P: TMark;
  i: integer;
begin
  Result := nil;
  P := Source.Mark;
  Token := NextToken;
  if Token is TKeyword then
    for i := Low(AKeywords) to High(AKeywords) do
      if SameText(Token.Value, AKeywords[i]) then
        Result := Token as TKeyword;
  if not Assigned(Result) then
    Source.Restore(P);
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
begin
  Result := Terminal([ATerminal]);
end;

function TStatement.Terminal(const ATerminals: array of string): TTerminal;
var
  Token: TToken;
  P: TMark;
  i: integer;
begin
  Result := nil;
  P := Source.Mark;
  Token := NextToken;
  if Token is TTerminal then
    for i := Low(ATerminals) to High(ATerminals) do
      if SameText(Token.Value, ATerminals[i]) then
        Result := Token as TTerminal;
  if not Assigned(Result) then
    Source.Restore(P);
end;

function TStatement.GetSettings: TParserSettings;
begin
  if Assigned(Parent)
    then Result := Parent.Settings
    else Result := FSettings;
end;

{ TStatementList }

procedure TStatementList.AfterConstruction;
begin
  inherited;
  FList := TList<TStatement>.Create;
end;

procedure TStatementList.BeforeDestruction;
begin
  inherited;
  FreeAndNil(FList);
end;

function TStatementList.InternalParse: boolean;
var
  P: TMark;
  S: TStatement;
  B: boolean;
begin
  repeat
    P := Source.Mark;
    { Если успешно разобрали конструкцию - работаем дальше }
    if ParseStatement(S) then
    begin
      FList.Add(S);
      continue;
    end;
    { Если встретили завершающую конструкцию - выходим }
    Source.Restore(P);
    B := ParseBreak;
    Source.Restore(P);
    if B then break;
    { Если ни то, ни другое - фиксируем неожиданную конструкцию и дальше ждём завершающей конструкции }
    if TUnexpectedToken.Parse(Self, Source, S)
      then FList.Add(S)
      else break;
  until false;
  Result := (Count > 0);
end;

procedure TStatementList.PrintSelf(APrinter: TPrinter);
var i: integer;
begin
  for i := 0 to FList.Count - 1 do APrinter.PrintItem(FList[i]);
end;

function TStatementList.Count: integer;
begin
  Result := FList.Count;
end;

function TStatementList.Item(Index: integer): TStatement;
begin
  Result := FList[Index];
end;

function TStatementList.Any(Found: array of TObject): boolean;
var i: integer;
begin
  Result := false;
  for i := Low(Found) to High(Found) do Result := Result or Assigned(Found[i]);
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
  TPackage.Parse(Self, Source, _What);
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

{ TEOFStatement }

procedure TEOFStatement.PrintSelf(APrinter: TPrinter);
begin
  { ничего не делаем }
end;

end.
