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
  {$TypeInfo On}
  TStatement = class
  strict private
    FParent: TStatement;
    FSettings: TFormatSettings;
    function GetSettings: TFormatSettings;
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
    procedure PrintSelf(APrinter: TPrinter); virtual;
    function Aligned: boolean; virtual;
    property Parent: TStatement read FParent;
    property Settings: TFormatSettings read GetSettings write FSettings;
  public
    function Name: string; virtual;
    function StatementType: string; virtual;
    function StatementName: string; virtual;
  end;
  {$TypeInfo Off}

  { Базовый класс для списков однотипных конструкций (переменные, операторы и т. п.) }
  TStatementList<S: TStatement> = class(TStatement)
  strict private
    Statements: TList<TStatement>;
    Delimiters: TList<TToken>;
  strict protected
    function InternalParse: boolean; override;
    function ParseStatement(out AResult: TStatement): boolean; virtual;
    function ParseDelimiter(out AResult: TToken): boolean; virtual;
    function ParseBreak: boolean; virtual;
    function MultiLine: boolean; virtual;
    procedure PrintDelimiter(APrinter: TPrinter; ADelimiter: TToken); virtual;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure PrintSelf(APrinter: TPrinter); override;
    function Count: integer;
    function Item(Index: integer): TStatement;
    function Any(Found: array of TObject): boolean;
  end;

  { Базовый класс для списка однотипных конструкций, разделённых запятыми }
  TCommaList<S: TStatement> = class(TStatementList<S>)
  strict protected
    function ParseDelimiter(out AResult: TToken): boolean; override;
  end;

  { Неожиданная лексема - класс для конструкций, которые не удалось разобрать }
  TUnexpectedToken = class(TStatement)
  strict private
    _Token: TToken;
  strict protected
    function InternalParse: boolean; override;
  public
    function StatementName: string; override;
    procedure PrintSelf(APrinter: TPrinter); override;
    property Token: TToken read _Token;
  end;

implementation

uses Attributes;

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

{ Конструирование названия выражения, выводимого в синтаксическое дерево }
function TStatement.Name: string;
begin
  try
    Result := StatementName;
    if Result = '' then Result := StatementType;
  except
    Result := StatementType;
  end;
end;

{ Вычисление "типа выражения" из имени класса }
function TStatement.StatementType: string; 
var
  S, U, L: string;
  i: integer;
begin
  S := Self.ClassName;
  if S[1] = 'T' then S := S.Substring(1);
  U := S.ToUpper;
  L := S.ToLower;
  Result := '';
  for i := 1 to Length(S) do
    if S[i] = L[i] then
      Result := Result + L[i]
    else if S[i] = U[i] then
      Result := Result + ' ' + L[i]
    else
      raise Exception.CreateFmt('Cannot make statement name for class %s', [Self.ClassName]);
end;

{ Выражение по умолчанию не имеет собственного имени }
function TStatement.StatementName: string; 
begin
  Result := '';
end;

procedure TStatement.PrintSelf(APrinter: TPrinter);
begin
  raise Exception.CreateFmt('Cannot print %s - PrintSelf is absent', [ClassName]);
end;

function TStatement.Aligned: boolean;
begin
  Result := Attributes.HasAttribute(ClassType, AlignedAttribute);
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

function TStatement.GetSettings: TFormatSettings;
begin
  if Assigned(Parent)
    then Result := Parent.Settings
    else Result := FSettings;
end;

{ TStatementList }

procedure TStatementList<S>.AfterConstruction;
begin
  inherited;
  Statements := TList<TStatement>.Create;
  Delimiters := TList<TToken>.Create;
end;

procedure TStatementList<S>.BeforeDestruction;
begin
  inherited;
  FreeAndNil(Statements);
  FreeAndNil(Delimiters);
end;

function TStatementList<S>.InternalParse: boolean;
var
  P: TMark;
  Statement: TStatement;
  Delimiter: TToken;
  B: boolean;
  UnexpectedToken: TStatement;
begin
  repeat
    P := Source.Mark;
    { Если успешно разобрали конструкцию - работаем дальше }
    if ParseStatement(Statement) then
    begin
      P := Source.Mark;
      Statements.Add(Statement);
      if ParseDelimiter(Delimiter) then
        begin
          Delimiters.Add(Delimiter);
          continue;
        end
      else
        Delimiters.Add(nil);
    end;
    { Если встретили завершающую конструкцию - выходим }
    Source.Restore(P);
    B := ParseBreak;
    Source.Restore(P);
    if B then break;
    { Если ни то, ни другое - фиксируем неожиданную конструкцию и дальше ждём завершающей конструкции }
    if TUnexpectedToken.Parse(Self, Source, UnexpectedToken)
      then begin Statements.Add(UnexpectedToken); Delimiters.Add(nil); end
      else break;
  until false;
  Result := (Count > 0);
end;

procedure TStatementList<S>.PrintSelf(APrinter: TPrinter);
var i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    APrinter.PrintItem(Statements[i]);
    PrintDelimiter(APrinter, Delimiters[i]);
  end;
  APrinter.SupressSpace;
end;

procedure TStatementList<S>.PrintDelimiter(APrinter: TPrinter; ADelimiter: TToken);
begin
  if (ADelimiter is TTerminal) and ((ADelimiter.Value = ',') or (ADelimiter.Value = ';')) then
    begin
      APrinter.SupressSpace;
      APrinter.SupressNextLine;
    end
  else
    APrinter.Space;
  APrinter.PrintItem(ADelimiter);
  APrinter.SpaceOrNextLine(MultiLine);
end;

function TStatementList<S>.ParseStatement(out AResult: TStatement): boolean;
begin
  Result := S.Parse(Self, Source, AResult);
end;

function TStatementList<S>.ParseDelimiter(out AResult: TToken): boolean;
begin
  Result := true;
  AResult := nil;
end;

function TStatementList<S>.ParseBreak: boolean;
begin
  raise Exception.CreateFmt('%s has no break condition', [ClassName]);
end;

function TStatementList<S>.MultiLine: boolean;
begin
  Result := true;
end;

function TStatementList<S>.Count: integer;
begin
  Result := Statements.Count;
end;

function TStatementList<S>.Item(Index: integer): TStatement;
begin
  Result := Statements[Index];
end;

function TStatementList<S>.Any(Found: array of TObject): boolean;
var i: integer;
begin
  Result := false;
  for i := Low(Found) to High(Found) do Result := Result or Assigned(Found[i]);
end;

{ TCommaList }

function TCommaList<S>.ParseDelimiter(out AResult: TToken): boolean;
begin
  AResult := Terminal(',');
  Result  := Assigned(AResult);
end;

{ TUnexpectedToken }

function TUnexpectedToken.StatementName: string;
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
  APrinter.PrintSpecialComment('!!! SHIT HAPPENS !!!');
end;

end.
