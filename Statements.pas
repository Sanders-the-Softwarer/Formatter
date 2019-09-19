////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//           Базовые классы выражений для синтаксического анализатора         //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Statements;

{ ----- Примечания -------------------------------------------------------------

  Класс TStatement является базовым для результатов синтаксического анализатора.
  В действительности основной разбор проходит именно в нём и его наследниках,
  в методах InternalParse, в то время как сам синтаксический анализатор только
  запускает процесс анализа очередной конструкции верхнего уровня. Класс
  поддерживает выстраивание дерева синтаксического разбора, передачу настроек
  печати, примитивы анализа.

  Наследники TStatement практически всегда используют и должны использовать
  для описания своих составляющих именно TStatement вместо какого-либо более
  специализированного типа. Это не только уменьшает связность классов, позволяя
  описывать их независимо и в произвольном порядке, но и даёт возможность
  правильно обрабатывать ситуации синтаксически неверных исходников.

  Класс TStatementList предназначен для описания последовательности однотипных
  конструкций - например, параметров подпрограмм или операторов в блоке
  begin .. end. Его использование важно для надёжности и корректности решения,
  поскольку он умеет правильно обрабатывать ситуации "А когда пора заканчивать
  список?", "А что, если кончился входной текст?", "А что, если встретилась
  неожиданная конструкция?"

  Класс TUnexpectedToken предназначен для выхода из ситуаций, когда во входном
  потоке встретилось что-то, чего по логике языка в этом месте быть решительно
  не должно. Он выцепляет из входного потока одну лексему так, словно она
  именно здесь и должна находиться и позволяет парсеру продолжить разбор,
  найти следующую корректную конструкцию и заняться делом. Благодаря этому
  классу, влияние таких ситуций на результат носит ограниченный и локальный
  характер.

------------------------------------------------------------------------------ }

interface

uses SysUtils, System.Generics.Collections, Streams, Tokens, Printers_;

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

implementation

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

end.

