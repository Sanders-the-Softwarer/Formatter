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

  Операторы и конструкции могут и часто должны завершаться точкой с запятой,
  поэтому класс TSemicolonStatement избавляет от необходимости описывать в
  тысяче мест парсинг одного и того же символа. Аналогично, класс
  TBracedStatement позволяет удобно описывать конструкции в скобках.

------------------------------------------------------------------------------ }

interface

uses Classes, SysUtils, System.Generics.Collections, Streams, Tokens, Printers_;

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
    function GetKeywords: TStrings; virtual;
    function NextToken: TToken;
    function Keyword(const AKeyword: string): TEpithet; overload;
    function Keyword(const AKeywords: array of string): TEpithet; overload;
    function Identifier: TEpithet;
    function Number: TNumber;
    function Literal: TLiteral;
    function Terminal(const ATerminal: string): TTerminal; overload;
    function Terminal(const ATerminals: array of string): TTerminal; overload;
    function Concat(Params: array of TObject): string;
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
    Delimiters: TList<TObject>;
  strict protected
    function InternalParse: boolean; override;
    function ParseStatement(out AResult: TStatement): boolean; virtual;
    function ParseDelimiter(out AResult: TObject): boolean; virtual;
    function ParseBreak: boolean; virtual;
    procedure PrintDelimiter(APrinter: TPrinter; ADelimiter: TObject); virtual;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function MultiLine: boolean; virtual;
    procedure PrintSelf(APrinter: TPrinter); override;
    function Count: integer;
    function Item(Index: integer): TStatement;
    function Any(Found: array of TObject): boolean;
  end;

  { Базовый класс для списка однотипных конструкций, разделённых запятыми }
  TCommaList<S: TStatement> = class(TStatementList<S>)
  strict protected
    function ParseDelimiter(out AResult: TObject): boolean; override;
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

  { Конструкция, завершающаяся точкой с запятой }
  TSemicolonStatement = class(TStatement)
  strict private
    _Semicolon: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция заданного типа в скобках }

  TSLBracketedStatement<T: TStatement> = class(TStatement)
  strict protected
    _OpenBracket, _CloseBracket: TTerminal;
    _Stmt: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  TMLBracketedStatement<T: TStatement> = class(TSLBracketedStatement<T>)
  public
    procedure PrintSelf(APrinter: TPrinter); override;
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
  i, P: integer;
begin
  S := Self.ClassName;
  if S[1] = 'T' then S := S.Substring(1);
  if S.StartsWith('ML') or S.StartsWith('SL') then S := S.Substring(2);
  P := Pos('<', S);
  if P > 0 then S := S.Substring(0, P - 1);
  U := S.ToUpper;
  L := S.ToLower;
  Result := '';
  for i := 1 to Length(S) do
    if (S[i] = U[i]) and (i > 1) and (S[i - 1] = L[i - 1])
      then Result := Result + ' ' + L[i]
      else Result := Result + L[i];
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

function TStatement.GetKeywords: TStrings;
begin
  if Assigned(Parent) then Result := Parent.GetKeywords else Result := nil;
end;

function TStatement.NextToken: TToken;
begin
  if Source.Eof
    then Result := UnexpectedEOF
    else Result := Source.Next;
end;

function TStatement.Keyword(const AKeyword: string): TEpithet;
begin
  Result := Keyword([AKeyword]);
end;

function TStatement.Keyword(const AKeywords: array of string): TEpithet;
var
  Token: TToken;
  P: TMark;
  i: integer;
begin
  Result := nil;
  P := Source.Mark;
  Token := NextToken;
  if Token is TEpithet then
    for i := Low(AKeywords) to High(AKeywords) do
      if SameText(Token.Value, AKeywords[i]) then
      begin
        Result := Token as TEpithet;
        Result.IsKeyword := true;
        Result.IsIdent   := false;
      end;
  if not Assigned(Result) then
    Source.Restore(P);
end;

function TStatement.Identifier: TEpithet;
var
  Token: TToken;
  P: TMark;
  K: TStrings;
begin
  Result := nil;
  K := GetKeywords;
  P := Source.Mark;
  Token := NextToken;
  if (Token is TEpithet) and (not Assigned(K) or (K.IndexOf(Token.Value) < 0)) then
    begin
      Result := Token as TEpithet;
      Result.IsKeyword := false;
      Result.IsIdent   := true;
    end
  else
    Source.Restore(P);
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

function TStatement.Concat(Params: array of TObject): string;
var
  i: integer;
  S: string;
begin
  Result := '';
  for i := Low(Params) to High(Params) do
  begin
    S := '';
    if Params[i] is TToken then S := TToken(Params[i]).Value;
    if Params[i] is TStatement then S := TStatement(Params[i]).StatementName;
    if S <> '' then
      if Result = ''
        then Result := S
        else Result := Trim(Result + ' ' + S);
  end;
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
  Delimiters := TList<TObject>.Create;
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
  Delimiter: TObject;
  B: boolean;
  UnexpectedToken: TStatement;
begin
  repeat
    P := Source.Mark;
    { Если успешно разобрали конструкцию - работаем дальше }
    Statement := nil;
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
end;

procedure TStatementList<S>.PrintDelimiter(APrinter: TPrinter; ADelimiter: TObject);
begin
  if (ADelimiter is TTerminal) and ((TTerminal(ADelimiter).Value = ',') or (TTerminal(ADelimiter).Value = ';')) then
    APrinter.SupressNextLine;
  APrinter.PrintItem(ADelimiter);
  APrinter.SpaceOrNextLine(MultiLine);
end;

function TStatementList<S>.ParseStatement(out AResult: TStatement): boolean;
begin
  Result := S.Parse(Self, Source, AResult);
end;

function TStatementList<S>.ParseDelimiter(out AResult: TObject): boolean;
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

function TCommaList<S>.ParseDelimiter(out AResult: TObject): boolean;
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
  APrinter.PrintItem(_Token);
  APrinter.NextLine;
  APrinter.PrintSpecialComment('!!! SHIT HAPPENS !!!');
end;

{ TSemicolonStatement }

function TSemicolonStatement.InternalParse: boolean;
begin
  _Semicolon := Terminal(';');
  Result := true;
end;

procedure TSemicolonStatement.PrintSelf(APrinter: TPrinter);
begin
  APrinter.SupressNextLine;
  APrinter.PrintItem(_Semicolon);
end;

{ TBracketedStatement<T> }

function TSLBracketedStatement<T>.InternalParse: boolean;
begin
  _OpenBracket := Terminal('(');
  if not Assigned(_OpenBracket) then exit(false);
  if not T.Parse(Self, Source, _Stmt) then exit(false);
  _CloseBracket := Terminal(')');
  Result := true;
end;

procedure TSLBracketedStatement<T>.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_OpenBracket, _Stmt, _CloseBracket]);
end;

procedure TMLBracketedStatement<T>.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_OpenBracket);
  APrinter.PrintIndented(_Stmt);
  APrinter.NextLine;
  APrinter.PrintItem(_CloseBracket);
end;

end.
