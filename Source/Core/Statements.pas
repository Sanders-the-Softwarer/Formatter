////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//           Базовые классы выражений для синтаксического анализатора         //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
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

uses Classes, SysUtils, Math, System.Generics.Collections, Contnrs, TypInfo,
  Streams, Tokens, Printer, Rulers;

type

  { Ссылка на класс синтаксических конструкций }
  TStatementClass = class of TStatement;

  { Предобъявление списка для использования в GetMatchSource/Target }
  TBaseStatementList = class;

  { Режим выравнивания }
  TAlignMode = (amNever, amNo, amYes);

  { Наличие спецкомментариев }
  THasSpecialComments = (hscUnknown, hscNo, hscYes);

  { Базовый класс синтаксических конструкций }
  {$TypeInfo On}
  TStatement = class
  strict private
    FParent: TStatement;
    FSettings: TFormatSettings;
    FFirstToken, FLastToken: TToken;
    FMatchedTo: TStatement;
    FreeList: TObjectList;
    FRulers: TRulers;
    FHasSpecialComments: THasSpecialComments;
    function GetSettings: TFormatSettings;
    procedure SetHasSpecialComments(AHasSpecialComments: THasSpecialComments);
    procedure FixLastToken;
  strict protected
    function InternalParse: boolean; virtual;
    procedure InternalPrintSelf(APrinter: TPrinter); virtual;
    procedure MatchChildren; virtual;
    function InternalGetMatchSource: TBaseStatementList; virtual;
    function InternalGetMatchTarget: TBaseStatementList; virtual;
    function Aligned: TAlignMode; virtual;
  strict protected
    Source: TBufferedStream<TToken>;
    function IsStrongKeyword(const AEpithet: string): boolean;
    function NextToken: TToken;
    function Keyword(const AKeyword: string): TEpithet; overload;
    function Keyword(const AKeywords: array of string): TEpithet; overload;
    function Identifier: TEpithet;
    function Epithet: TEpithet;
    function Number: TNumber;
    function Literal: TLiteral;
    function Terminal(const ATerminal: string): TTerminal; overload;
    function Terminal(const ATerminals: array of string): TTerminal; overload;
    function Concat(Params: array of TObject): string;
    procedure AddToFreeList(AObj: TObject);
    procedure RemoveFromFreeList(AObj: TObject);
    procedure ReplaceToken(var AOld: TToken; ANew: TToken);
    function GetRulers: TRulers;
  public
    class function Parse(AParent: TStatement; Tokens: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class function Candidates(AParent: TStatement): TArray<TStatementClass>; virtual;
    function EmptyLineBefore: boolean; virtual;
    function EmptyLineAfter: boolean; virtual;
    function EmptyLineInside: boolean; virtual;
    function NoEmptyLineBefore: boolean; virtual;
  public
    constructor Create(AParent: TStatement; ASource: TBufferedStream<TToken>); virtual;
    destructor Destroy; override;
    procedure PrintSelf(APrinter: TPrinter);
    function HasCommentsAbove: boolean;
    property Parent: TStatement read FParent write FParent;
    property Settings: TFormatSettings read GetSettings write FSettings;
    property FirstToken: TToken read FFirstToken;
    property LastToken: TToken read FLastToken;
    property Rulers: TRulers read GetRulers;
    property MatchedTo: TStatement read FMatchedTo write FMatchedTo;
    property HasSpecialComments: THasSpecialComments read FHasSpecialComments write SetHasSpecialComments;
  public
    class function StatementType: string;
    class procedure Match(ASource, ATarget: TStatement);
    { Приоритет конструкции в синтаксическом анализаторе }
    class function Priority: integer; virtual;
    function Name: string; virtual;
    function StatementName: string; virtual;
    function SameTypeAligned: TAlignMode; virtual;
    function Transparent: boolean; virtual;
    function Grouping: TStatementClass; virtual;
    function GetMatchSource: TBaseStatementList;
    function GetMatchTarget: TBaseStatementList;
    function IsAligned: boolean;
    {$IFDEF DEBUG}
    function DebugInfo: string;
    {$ENDIF}
  end;

  {$TypeInfo Off}

  { Базовый класс для списков однотипных конструкций (переменные, операторы и т. п.) }

  TBaseStatementList = class(TStatement)
  strict protected
    SpecialCommentAfterDelimiter: string;
  public
    procedure PrintSpecialCommentAfterDelimiter(const AComment: string);
    function Count: integer; virtual; abstract;
    function Item(Index: integer): TStatement; virtual; abstract;
  end;

  TStatementList<S: TStatement> = class(TBaseStatementList)
  strict private
    Statements: TList<TStatement>;
    Delimiters: TList<TObject>;
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
    function StatementName: string; override;
    function Count: integer; override;
    function Item(Index: integer): TStatement; override;
    function Delimiter(Index: integer): TObject;
    function Any(Found: array of TObject): boolean;
  end;

  { Базовый класс для списка однотипных конструкций без разделителя }
  TStrictStatementList<S: TStatement> = class(TStatementList<S>)
  strict protected
    function AllowUnexpected: boolean; override;
  end;

  { Неожиданная лексема - класс для конструкций, которые не удалось разобрать }
  TUnexpectedToken = class(TStatement)
  strict private
    _Token: TToken;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    class function Priority: integer; override;
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
    procedure InternalPrintSelf(APrinter: TPrinter); override;
    function InternalGetMatchSource: TBaseStatementList; override;
    function InternalGetMatchTarget: TBaseStatementList; override;
  public
    property InnerStatement: TStatement read _Stmt;
    function Transparent: boolean; override;
  end;

  { Конструкция заданного типа, возможно, заключённая в скобки }
  TOptionalBracketedStatement<T: TStatement> = class(TBracketedStatement<T>)
  public
    class function Candidates(AParent: TStatement): TArray<TStatementClass>; override;
  end;

  { Конструкция для форматирования заданной в одну строку }
  TSingleLine<T: TStatement> = class(TStatement)
  strict private
    _Stmt: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function Transparent: boolean; override;
  end;

{ Конвертация настройки в TAlignMode }
function AlignMode(ASetting: boolean): TAlignMode;

implementation

uses Parser, Keywords;

{ Конвертация настройки в TAlignMode }
function AlignMode(ASetting: boolean): TAlignMode;
begin
  if ASetting
    then Result := amYes
    else Result := amNo;
end;

{ TStatement }

constructor TStatement.Create(AParent: TStatement; ASource: TBufferedStream<TToken>);
begin
  FParent := AParent;
  if Assigned(AParent) then AParent.AddToFreeList(Self);
  Source := ASource;
end;

destructor TStatement.Destroy;
begin
  if Assigned(Parent) then Parent.RemoveFromFreeList(Self);
  inherited;
  FreeAndNil(FreeList);
  FreeAndNil(FRulers);
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
  Candidates := Self.Candidates(AParent);
  for Candidate in Candidates do
    if Candidate = Self then
      begin
        AResult := Candidate.Create(AParent, Tokens);
        if AResult.InternalParse then
          begin
            AResult.FixLastToken;
            exit(true);
          end
        else
          begin
            Tokens.Restore(SavedPosition);
            FreeAndNil(AResult);
          end;
      end
    else
      begin
        Result := Candidate.Parse(AParent, Tokens, AResult);
        if Result then exit(true);
        Tokens.Restore(SavedPosition);
      end;
  Result := false;
end;

{ Вывод разобранного выражения на печать }
procedure TStatement.PrintSelf(APrinter: TPrinter);
begin
  MatchChildren;
  InternalPrintSelf(APrinter);
  if Assigned(MatchedTo) and (Parent is TBaseStatementList) then
    (Parent as TBaseStatementList).PrintSpecialCommentAfterDelimiter('=> ' + MatchedTo.StatementName);
end;

{ По умолчанию единственным кандидатом на реализацию класса является сам класс }
class function TStatement.Candidates(AParent: TStatement): TArray<TStatementClass>;
begin
  Result := [Self];
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

{ Запрет пустой строки перед конструкцией }
function TStatement.NoEmptyLineBefore: boolean;
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
class function TStatement.StatementType: string;
var
  S, U, L: string;
  i, P: integer;
begin
  S := Self.ClassName;
  if S[1] = 'T' then S := S.Substring(1);
  if S.StartsWith('ML') or S.StartsWith('SL') then S := S.Substring(2);
  S := S.TrimRight(['_']);
  P := Pos('<', S);
  if P > 0 then S := S.Substring(0, P - 1);
  U := S.ToUpper;
  L := S.ToLower;
  Result := '';
  for i := 1 to Length(S) do
    if (S[i] = U[i]) and (i > 1) and (S[i - 1] = L[i - 1]) then
      Result := Result + ' ' + L[i]
    else if (S[i] = U[i]) and (i > 1) and (S[i - 1] = U[i - 1]) and (i < Length(S)) and (S[i + 1] = L[i + 1]) then
      Result := Result + ' ' + L[i]
    else
      Result := Result + L[i];
end;

{ Выражение по умолчанию не имеет собственного имени }
function TStatement.StatementName: string;
begin
  Result := '';
end;

{ По умолчанию выражение не выравнивается }
function TStatement.Aligned: TAlignMode;
begin
  Result := amNever;
end;

{ По умолчанию в группе не выравнивается }
function TStatement.SameTypeAligned: TAlignMode;
begin
  Result := amNever;
end;

{ Выражение по умолчанию видно в синтаксическом дереве }
function TStatement.Transparent: boolean;
begin
  Result := false;
end;

{ Выражение по умолчанию не группируется с однотипными }
function TStatement.Grouping: TStatementClass;
begin
  Result := nil;
end;

{ Обработчик печати по умолчанию }
procedure TStatement.InternalPrintSelf(APrinter: TPrinter);
begin
  raise Exception.CreateFmt('Cannot print %s - PrintSelf is absent', [ClassName]);
end;

function TStatement.InternalGetMatchSource: TBaseStatementList;
begin
  Result := nil;
end;

function TStatement.InternalGetMatchTarget: TBaseStatementList;
begin
  Result := nil;
end;

{ Реализация сопоставления полей }
class procedure TStatement.Match(ASource, ATarget: TStatement);
var
  SourceList, TargetList: TBaseStatementList;
  SourceItem, TargetItem: TStatement;
  Count, i: integer;
begin
  { Определим исходный список }
  if not Assigned(ASource) then exit;
  if ASource is TBaseStatementList
    then SourceList := TBaseStatementList(ASource)
    else SourceList := ASource.GetMatchSource;
  if not Assigned(SourceList) then exit;
  { Определим целевой список }
  if not Assigned(ATarget) then exit;
  if ATarget is TBaseStatementList
    then TargetList := TBaseStatementList(ATarget)
    else TargetList := ATarget.GetMatchTarget;
  if not Assigned(TargetList) then exit;
  { Сопоставим элементы списков }
  Count := Math.Min(SourceList.Count, TargetList.Count);
  if Count < ASource.Settings.MatchParamLimit then exit;
  for i := 0 to Count - 1 do
  begin
    SourceItem := SourceList.Item(i);
    TargetItem := TargetList.Item(i);
    if Assigned(SourceItem) then SourceItem.MatchedTo := TargetItem;
  end;
end;

{ Приоритет конструкции в синтаксическом анализаторе }
class function TStatement.Priority: integer;
begin
  Result := STD_PRIORITY;
end;

function TStatement.GetMatchSource: TBaseStatementList;
begin
  if not Assigned(Self) then
    Result := nil
  else if Self is TBaseStatementList then
    Result := TBaseStatementList(Self)
  else
    Result := Self.InternalGetMatchSource;
  Assert((Result = nil) or (TObject(Result) is TBaseStatementList));
end;

function TStatement.GetMatchTarget: TBaseStatementList;
begin
  if not Assigned(Self) then
    Result := nil
  else if Self is TBaseStatementList then
    Result := TBaseStatementList(Self)
  else
    Result := Self.InternalGetMatchTarget;
end;

procedure TStatement.MatchChildren;
begin
  { ничего не делаем }
end;

function TStatement.IsAligned: boolean;
begin
  Result := (HasSpecialComments = hscYes) or (Aligned = amYes);
end;

{$IFDEF DEBUG}

function TStatement.DebugInfo: string;
begin
  Result := Format('Конструкция: %s (%s)'#13#13, [Name, ClassName]);
  if Aligned <> amNever then
    Result := Result + Format('Выравнивание: %s'#13#13, [GetEnumName(TypeInfo(TAlignMode), Ord(Aligned))]);
  if (HasSpecialComments <> hscNo) then
    Result := Result + Format('Спецкомментарии: %s'#13#13, [GetEnumName(TypeInfo(THasSpecialComments), Ord(HasSpecialComments))]);
  if Assigned(FRulers) then
    Result := Result + Format('Собственное выравнивание: [%p]'#13#13, [pointer(FRulers)]);
  if not Assigned(FRulers) and (GetRulers <> nil) then
    Result := Result + Format('Унаследованное выравнивание: [%p]'#13#13, [pointer(GetRulers)]);
  if IsAligned then
    Result := Result + Format('Информация выравнивания:'#13#13'%s'#13#13, [Rulers.DebugInfo]);
end;

{$ENDIF}

function TStatement.HasCommentsAbove: boolean;
begin
  Result := Assigned(FirstToken) and
            (Assigned(FirstToken.CommentFarAbove) or Assigned(FirstToken.CommentAbove));
end;

function TStatement.InternalParse: boolean;
begin
  raise Exception.CreateFmt('Cannot parse %s - InternalParse is absent', [ClassName]);
end;

function TStatement.IsStrongKeyword(const AEpithet: string): boolean;
begin
  Result := AEpithet.Contains(' ') or Keywords.IsKeyword(AEpithet, Self);
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

function TStatement.Epithet: TEpithet;
var
  Token: TToken;
  P: TMark;
begin
  Result := nil;
  P := Source.Mark;
  Token := NextToken;
  if Token is TEpithet then
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

procedure TStatement.AddToFreeList(AObj: TObject);
begin
  if not Assigned(FreeList) then FreeList := TObjectList.Create(true);
  FreeList.Add(AObj);
end;

procedure TStatement.RemoveFromFreeList(AObj: TObject);
begin
  if Assigned(FreeList) then FreeList.Extract(AObj);
end;

procedure TStatement.ReplaceToken(var AOld: TToken; ANew: TToken);
var i: TCommentPosition;
begin
  if Assigned(AOld) and Assigned(ANew) then
    for i := Low(TCommentPosition) to High(TCommentPosition) do
      ANew.SetComment(i, AOld.GetComment(i));
  AOld := ANew;
  AddToFreeList(ANew);
end;

function TStatement.GetSettings: TFormatSettings;
begin
  if not Assigned(FSettings) and Assigned(Parent) then FSettings := Parent.Settings;
  Result := FSettings;
end;

procedure TStatement.SetHasSpecialComments(AHasSpecialComments: THasSpecialComments);
var S: TStatement;
begin
  FHasSpecialComments := AHasSpecialComments;
  { Если комментарии есть в низовой конструкции, нужно пробросить признак
    в вышестоящую выравниваемую }
  if AHasSpecialComments <> hscYes then exit;
  S := Self.Parent;
  while Assigned(S) and (S.Aligned = amNever) do S := S.Parent;
  if Assigned(S) then S.HasSpecialComments := hscYes;
end;

procedure TStatement.FixLastToken;
begin
  FLastToken := Source.Last;
end;

function TStatement.GetRulers: TRulers;
begin
  if IsAligned then
    begin
      if not Assigned(FRulers) then FRulers := TRulers.Create(Self);
      Result := FRulers;
    end
  else if Assigned(Parent) then
    Result := Parent.Rulers
  else
    Result := nil;
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
  FreeAndNil(Statements);
  FreeAndNil(Delimiters);
  inherited;
end;

function TStatementList<S>.StatementName: string;
begin
  if Self.ClassName.Contains('StatementList')
    then Result := 'list of ' + S.StatementType
    else Result := inherited;
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
      DelimiterOk := ParseDelimiter(Delimiter) and (P <> Source.Mark); { пустой разделитель не канает }
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
    if not StatementOk or (AllowUnexpected and not DelimiterOk) then
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
    APrinter.CancelNextLine;
    PrintDelimiter(APrinter, Delimiters[i], i >= Count - 1);
  end;
end;

procedure TStatementList<S>.PrintDelimiter(APrinter: TPrinter; ADelimiter: TObject; ALast: boolean);
begin
  APrinter.BeforePrintDelimiter;
  APrinter.PrintItem(ADelimiter);
  APrinter.AfterPrintDelimiter;
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

procedure TBaseStatementList.PrintSpecialCommentAfterDelimiter(const AComment: string);
begin
  SpecialCommentAfterDelimiter := AComment;
end;

{ TUnexpectedToken }

class function TUnexpectedToken.Priority: integer;
begin
  Result := MIN_PRIORITY;
end;

function TUnexpectedToken.StatementName: string;
begin
  Result := Format('*** НЕОЖИДАННАЯ КОНСТРУКЦИЯ *** [%s ''%s'']', [Token.TokenType, Token.InitialValue]);
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
  Result := true;
  _OpenBracket := Terminal('(');
  if not Assigned(_OpenBracket) then exit(false);
  if not T.Parse(Self, Source, _Stmt) and not AllowEmpty then exit(false);
  _CloseBracket := Terminal(')');
end;

function TBracketedStatement<T>.AllowEmpty: boolean;
begin
  Result := false;
end;

function TBracketedStatement<T>.MultiLine: boolean;
begin
  Result := true;
end;

function TBracketedStatement<T>.InternalGetMatchSource: TBaseStatementList;
begin
  Result := _Stmt.GetMatchSource;
end;

function TBracketedStatement<T>.InternalGetMatchTarget: TBaseStatementList;
begin
  Result := _Stmt.GetMatchTarget;
end;

procedure TBracketedStatement<T>.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_OpenBracket);
  if MultiLine then APrinter.PrintItem(_IndentNextLine);
  APrinter.PrintItem(_Stmt);
  if MultiLine then APrinter.PrintItem(_UndentNextLine);
  APrinter.PrintItem(_CloseBracket);
end;

function TBracketedStatement<T>.Transparent: boolean;
begin
  Result := true;
end;

{ TOptionalBracketedStatement<T> }

class function TOptionalBracketedStatement<T>.Candidates(AParent: TStatement): TArray<TStatementClass>;
begin
  Result := [T, TBracketedStatement<T>];
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

function TSingleLine<T>.Transparent: boolean;
begin
  Result := true;
end;

{ TStrictStatementList<S> }

function TStrictStatementList<S>.AllowUnexpected: boolean;
begin
  Result := false;
end;

end.
