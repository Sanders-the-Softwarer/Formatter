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

uses System.SysUtils, System.Generics.Collections, Math, Streams, Tokens, Printers_;

type

  { Базовый класс синтаксических конструкций }
  TStatement = class
  strict private
    FParent: TStatement;
  strict protected
    Source: TBufferedStream<TToken>;
    function InternalParse: boolean; virtual; abstract;
    function NextToken: TToken;
    function Keyword(const AKeyword: string): TKeyword; overload;
    function Keyword(const AKeywords: array of string): TKeyword; overload;
    function Identifier: TIdent;
    function Number: TNumber;
    function Literal: TLiteral;
    function Terminal(const ATerminal: string): TTerminal;
  public
    class function Parse(AParent: TStatement; Tokens: TBufferedStream<TToken>; out AResult: TStatement): boolean;
  public
    constructor Create(AParent: TStatement; ASource: TBufferedStream<TToken>);
    function Name: string; virtual;
    procedure PrintSelf(APrinter: TPrinter); virtual; abstract;
    property Parent: TStatement read FParent;
  end;

  { Синтаксический анализатор }
  TParser = class(TNextStream<TToken, TStatement>)
  strict protected
    function InternalNext: TStatement; override;
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

  { Квалифицированный идентификатор }
  TQualifiedIdentifier = class(TStatement)
  strict private
    _Ident: TIdent;
    _Dot: TTerminal;
    _Rest: TStatement;
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

implementation

uses PLSQL;

{ TParser }

function TParser.InternalNext: TStatement;
begin
  if not TCreateStatement.Parse(nil, Source, Result) then
    TUnexpectedToken.Parse(nil, Source, Result);
end;

{ TUnexpectedToken }

function TUnexpectedToken.Name: string;
begin
  Result := Format('*** НЕОЖИДАННАЯ КОНСТРУКЦИЯ *** [%s ''%s'']', [Token.TokenType, Token.Value]);
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
var SavedPosition: TMark;
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

{ TQualifiedIdentifier }

function TQualifiedIdentifier.InternalParse: boolean;
begin
  _Ident := Identifier;
  if not Assigned(_Ident) then exit(false);
  Result := true;
  _Dot := Terminal('.');
  if Assigned(_Dot) then TQualifiedIdentifier.Parse(Self, Source, _Rest);
end;

function TQualifiedIdentifier.Name: string;
begin
  Result := 'qualified identifier';
end;

procedure TQualifiedIdentifier.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Ident);
  APrinter.PrintItem(_Dot);
  APrinter.PrintItem(_Rest);
end;

end.
