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
  TBracketedStatement позволяет удобно описывать конструкции в скобках.

------------------------------------------------------------------------------ }

interface

uses Classes, SysUtils, System.Generics.Collections, Utils, Streams, Tokens,
  Printers_;

type

  { Ссылка на класс синтаксических конструкций }
  TStatementClass = class of TStatement;

  { Базовый класс синтаксических конструкций }
  {$TypeInfo On}
  TStatement = class
  strict private
    FParent: TStatement;
    FSettings: TFormatSettings;
    FFirstToken: TToken;
    function GetSettings: TFormatSettings;
  strict protected
    function GetKeywords: TKeywords; virtual;
    function InternalParse: boolean; virtual;
    procedure InternalMatch(AStatement: TStatement); virtual;
    procedure InternalMatchChildren; virtual;
    procedure InternalPrintSelf(APrinter: TPrinter); virtual;
  strict protected
    Source: TBufferedStream<TToken>;
    function IsStrongKeyword(const AEpithet: string): boolean;
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
    class function Candidates: TArray<TStatementClass>; virtual;
    function EmptyLineBefore: boolean; virtual;
    function EmptyLineAfter: boolean; virtual;
    function EmptyLineInside: boolean; virtual;
  public
    constructor Create(AParent: TStatement; ASource: TBufferedStream<TToken>); virtual;
    procedure PrintSelf(APrinter: TPrinter);
    function Aligned: boolean; virtual;
    procedure Match(AStatement: TStatement);
    procedure MatchChildren;
    property Parent: TStatement read FParent;
    property Settings: TFormatSettings read GetSettings write FSettings;
    property FirstToken: TToken read FFirstToken;
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
    SpecialCommentAfterDelimiter: string;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
    function ParseStatement(out AResult: TStatement): boolean; virtual;
    function ParseDelimiter(out AResult: TObject): boolean; virtual;
    function ParseBreak: boolean; virtual;
    procedure PrintDelimiter(APrinter: TPrinter; ADelimiter: TObject; ALast: boolean); virtual;
    function OnePerLine: boolean; virtual;
    function AllowStatement(AStatement: TStatement): boolean; virtual;
    function AllowUnexpected: boolean; virtual;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Count: integer;
    function Item(Index: integer): TStatement;
    function Delimiter(Index: integer): TObject;
    function Any(Found: array of TObject): boolean;
    procedure PrintSpecialCommentAfterDelimiter(const AComment: string);
  end;

  { Базовый класс для списка однотипных конструкций, разделённых запятыми }
  TCommaList<S: TStatement> = class(TStatementList<S>)
  strict protected
    function ParseDelimiter(out AResult: TObject): boolean; override;
    function ParseBreak: boolean; override;
  end;

  { Неожиданная лексема - класс для конструкций, которые не удалось разобрать }
  TUnexpectedToken = class(TStatement)
  strict private
    _Token: TToken;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
    property Token: TToken read _Token;
  end;

  { Конструкция, завершающаяся точкой с запятой }
  TSemicolonStatement = class(TStatement)
  strict private
    _Semicolon: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function HasSemicolon: boolean;
  end;

  { Конструкция заданного типа в скобках }
  TBracketedStatement<T: TStatement> = class(TStatement)
  strict private
    _OpenBracket, _CloseBracket: TTerminal;
    _Stmt: TStatement;
  strict protected
    function InternalParse: boolean; override;
    function AllowEmpty: boolean; virtual;
    function MultiLine: boolean; virtual;
    procedure InternalMatch(AStatement: TStatement); override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    property InnerStatement: TStatement read _Stmt;
  end;

  { Конструкция заданного типа в скобках, допускающая пустые скобки }
  TOptionalBracketedStatement<T: TStatement> = class(TBracketedStatement<T>)
  strict protected
    function AllowEmpty: boolean; override;
  end;

  { Конструкция для форматирования заданной в одну строку }
  TSingleLine<T: TStatement> = class(TStatement)
  strict private
    _Stmt: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

{ TStatement }

constructor TStatement.Create(AParent: TStatement; ASource: TBufferedStream<TToken>);
begin
  FParent := AParent;
  Source := ASource;
end;

{ Ключевое место продукта - попытка разбора указанного выражения и восстановление при неудаче }
class function TStatement.Parse(AParent: TStatement; Tokens: TBufferedStream<TToken>; out AResult: TStatement): boolean;
var
  SavedPosition: TMark;
  Candidate: TStatementClass;
  Candidates: TArray<TStatementClass>;
begin
  AResult := nil;
  SavedPosition := Tokens.Mark;
  Candidates := Self.Candidates;
  for Candidate in Candidates do
    if Candidate = Self then
      begin
        AResult := Candidate.Create(AParent, Tokens);
        if AResult.InternalParse then exit(true);
        Tokens.Restore(SavedPosition);
        FreeAndNil(AResult);
      end
    else
      begin
        Result := Candidate.Parse(AParent, Tokens, AResult);
        if Result then exit(true);
        Tokens.Restore(SavedPosition);
      end;
  Result := false;
end;

procedure TStatement.PrintSelf(APrinter: TPrinter);
begin
  MatchChildren;
  InternalPrintSelf(APrinter);
end;

class function TStatement.Candidates: TArray<TStatementClass>;
begin
  Result := TArray<TStatementClass>.Create(Self);
end;

{ Определение необходимости пустой строки перед конструкцией }
function TStatement.EmptyLineBefore: boolean;
begin
  Result := false;
end;

{ Определение необходимости пустой строки после конструкции }
function TStatement.EmptyLineAfter: boolean;
begin
  Result := false;
end;

{ Определение необходимости пустой строки между элементами конструкции }
function TStatement.EmptyLineInside: boolean;
begin
  Result := false;
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
  S := S.TrimEnd(['_']);
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

{ Обработчик печати по умолчанию }
procedure TStatement.InternalPrintSelf(APrinter: TPrinter);
begin
  raise Exception.CreateFmt('Cannot print %s - PrintSelf is absent', [ClassName]);
end;

function TStatement.Aligned: boolean;
begin
  Result := false;
end;

procedure TStatement.MatchChildren;
begin
  if Assigned(Self) then InternalMatchChildren;
end;

procedure TStatement.Match(AStatement: TStatement);
begin
  if not Assigned(Self) or not Assigned(AStatement) then exit;
  InternalMatch(AStatement);
end;

function TStatement.InternalParse: boolean;
begin
  raise Exception.CreateFmt('Cannot parse %s - InternalParse is absent', [ClassName]);
end;

function TStatement.GetKeywords: TKeywords;
begin
  if Assigned(Parent) then Result := Parent.GetKeywords else Result := nil;
end;

procedure TStatement.InternalMatch(AStatement: TStatement);
begin
  { ничего не делаем }
end;

procedure TStatement.InternalMatchChildren;
begin
  { ничего не делаем }
end;

function TStatement.IsStrongKeyword(const AEpithet: string): boolean;
var Keywords: TStrings;
begin
  if AEpithet.Contains(' ') then exit(true);
  Keywords := GetKeywords;
  Result := Assigned(Keywords) and (Keywords.IndexOf(AEpithet) >= 0);
end;

function TStatement.NextToken: TToken;
begin
  if Source.Eof
    then Result := UnexpectedEOF
    else Result := Source.Next;
  if not Assigned(FFirstToken)
    then FFirstToken := Result;
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
      if SameText(Token.Value, AKeywords[i]) or
         SameText(AKeywords[i], '*') and IsStrongKeyword(Token.Value) then
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
begin
  Result := nil;
  P := Source.Mark;
  Token := NextToken;
  if (Token is TEpithet) and not IsStrongKeyword(Token.Value) then
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
    if Params[i] is TStatement then S := TStatement(Params[i]).Name;
    if S <> '' then
      if Result = ''
        then Result := S
        else Result := Trim(Result + ' ' + S);
  end;
end;

function TStatement.GetSettings: TFormatSettings;
begin
  if not Assigned(FSettings) and Assigned(Parent) then FSettings := Parent.Settings;
  Result := FSettings;
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
  StatementOk, DelimiterOk, BreakOk: boolean;
  UnexpectedToken: TStatement;
  UnexpectedCnt, UnexpectedTotal: integer;
begin
  UnexpectedCnt := 0;
  UnexpectedTotal := 0;
  repeat
    P := Source.Mark;
    { Разберём конструкцию }
    Statement := nil;
    StatementOk := ParseStatement(Statement);
    if StatementOk then
      begin
        Statements.Add(Statement);
        Assert(Source <> nil);
        P := Source.Mark;
        UnexpectedCnt := 0;
      end
    else
      begin
        Source.Restore(P);
        if not AllowUnexpected then break;
      end;
    { Если разобрали конструкцию, разберём разделитель }
    if StatementOk then
    begin
      Delimiter := nil;
      DelimiterOk := ParseDelimiter(Delimiter) and (P <> Source.Mark); { пустой разделитель не считается }
      if DelimiterOk then
        begin
          Delimiters.Add(Delimiter);
          P := Source.Mark;
        end
      else
        begin
          Delimiters.Add(nil);
          Source.Restore(P);
        end;
    end;
    { Если нет либо конструкции, либо разделителя, проверим условия выхода }
    if not StatementOk or not DelimiterOk then
    begin
      BreakOk := ParseBreak;
      Source.Restore(P);
      if BreakOk then break;
    end;
    { Если не удалось разобрать конструкцию и не удалось выйти - фиксируем неожиданную лексему и идём дальше }
    if StatementOk then
      continue
    else if TUnexpectedToken.Parse(Self, Source, UnexpectedToken) then
      begin
        Statements.Add(UnexpectedToken);
        Delimiters.Add(nil);
        Inc(UnexpectedCnt);
        Inc(UnexpectedTotal);
      end
    else
      break;
    { Если копятся неожиданные лексемы и не удаётся восстановиться - прервём разбор. Пусть вышестоящему повезёт больше }
    if (UnexpectedCnt > 10) or (UnexpectedTotal > 100) then
      break;
  until Source.Eof;
  Result := (Count > 0);
end;

procedure TStatementList<S>.InternalPrintSelf(APrinter: TPrinter);
var i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    APrinter.PrintItem(Statements[i]);
    PrintDelimiter(APrinter, Delimiters[i], i >= Count - 1);
  end;
end;

procedure TStatementList<S>.PrintDelimiter(APrinter: TPrinter; ADelimiter: TObject; ALast: boolean);
begin
  APrinter.PrintItem(ADelimiter);
  if SpecialCommentAfterDelimiter <> '' then APrinter.PrintSpecialComment(SpecialCommentAfterDelimiter);
  SpecialCommentAfterDelimiter := '';
  if OnePerLine and not ALast then APrinter.NextLine;
end;

function TStatementList<S>.OnePerLine: boolean;
begin
  Result := true;
end;

function TStatementList<S>.AllowStatement(AStatement: TStatement): boolean;
begin
  Result := true;
end;

function TStatementList<S>.AllowUnexpected: boolean;
begin
  Result := true;
end;

function TStatementList<S>.ParseStatement(out AResult: TStatement): boolean;
begin
  Result := S.Parse(Self, Source, AResult) and AllowStatement(AResult);
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

function TStatementList<S>.Count: integer;
begin
  Result := Statements.Count;
end;

function TStatementList<S>.Item(Index: integer): TStatement;
begin
  Result := Statements[Index];
end;

function TStatementList<S>.Delimiter(Index: integer): TObject;
begin
  Result := Delimiters[Index];
end;

function TStatementList<S>.Any(Found: array of TObject): boolean;
var i: integer;
begin
  Result := false;
  for i := Low(Found) to High(Found) do Result := Result or Assigned(Found[i]);
end;

procedure TStatementList<S>.PrintSpecialCommentAfterDelimiter(const AComment: string);
begin
  SpecialCommentAfterDelimiter := AComment;
end;

{ TCommaList }

function TCommaList<S>.ParseDelimiter(out AResult: TObject): boolean;
begin
  AResult := Terminal(',');
  Result  := Assigned(AResult);
end;

function TCommaList<S>.ParseBreak: boolean;
begin
  Result := true;
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

procedure TUnexpectedToken.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Token);
  APrinter.NextLine;
  APrinter.PrintSpecialComment('!!! SHIT HAPPENS !!!');
end;

{ TSemicolonStatement }

function TSemicolonStatement.InternalParse: boolean;
begin
  _Semicolon := Terminal(';');
  Result := Assigned(_Semicolon);
end;

procedure TSemicolonStatement.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.CancelNextLine;
  APrinter.PrintItem(_Semicolon);
end;

function TSemicolonStatement.HasSemicolon: boolean;
begin
  Result := Assigned(_Semicolon);
end;

{ TBracketedStatement<T> }

function TBracketedStatement<T>.InternalParse: boolean;
begin
  _OpenBracket := Terminal('(');
  if not Assigned(_OpenBracket) then exit(false);
  if not T.Parse(Self, Source, _Stmt) and not AllowEmpty then exit(false);
  _CloseBracket := Terminal(')');
  Result := true;
end;

function TBracketedStatement<T>.AllowEmpty: boolean;
begin
  Result := false;
end;

function TBracketedStatement<T>.MultiLine: boolean;
begin
  Result := true;
end;

procedure TBracketedStatement<T>.InternalMatch(AStatement: TStatement);
begin
  _Stmt.Match(AStatement);
end;

procedure TBracketedStatement<T>.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_OpenBracket);
  if MultiLine then APrinter.PrintItem(_IndentNextLine);
  APrinter.PrintItem(_Stmt);
  if MultiLine then APrinter.PrintItem(_UndentNextLine);
  APrinter.PrintItem(_CloseBracket);
end;

{ TOptionalBracketedStatement<T> }

function TOptionalBracketedStatement<T>.AllowEmpty: boolean;
begin
  Result := true;
end;

{ TSingleLine<T> }

function TSingleLine<T>.InternalParse: boolean;
begin
  Result := T.Parse(Self, Source, _Stmt);
end;

procedure TSingleLine<T>.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.SupressNextLine(true);
  APrinter.PrintItem(_Stmt);
  APrinter.SupressNextLine(false);
end;

end.
