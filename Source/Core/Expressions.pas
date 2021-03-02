////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//   Синтаксические конструкции арифметических-логических-прочих выражений    //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Expressions;

{ ----- Примечания -------------------------------------------------------------

  Выражения рассматриваются как последовательности элементов (term),
  разделённых знаками бинарных операций. Их представлением является
  контейнер TStatementList<TTerm>, в котором операции выполняют роль
  разделителей. Каждый элемент может быть одним из многих различных типов
  (идентификатор, число, подзапрос и так далее), а кроме того снабжён
  кучей префиксных и постфиксных модификаторов, описывающих унарный минус,
  суффикс %rowcount и прочие подобные вещи.

  Синтаксические конструкции "квалифицированный идентификатор", "вызов функции",
  "обращение к элементу таблицы" настолько похожи друг на друга и настолько
  могут комбинироваться в разных порядках и сочетаниях, что пришлось сделать
  один класс TQualifiedIndexedIdent, по факту описывающий и ту, и другую, и
  третью. Это оказалось не то что наиболее простым, а скорее единственным
  способом обеспечить корректную работу парсера.

------------------------------------------------------------------------------ }

interface

uses Classes, SysUtils, Math, Tokens, Statements, Printer,
  System.Generics.Collections, Utils;

type

  { Элемент выражения }
  TTerm = class(TStatement)
  strict private
    _Prefix: TToken;
    _Number: TNumber;
    _LiteralPrefix: TEpithet;
    _Literal: TLiteral;
    _SQLStatement: TStatement;
    _Ident: TStatement;
    _Suffix: TTerminal;
    _KeywordValue: TEpithet;
    _Select: TStatement;
    _Expression: TStatement;
    _Case: TStatement;
    _Cast: TStatement;
    _OuterJoin: TTerminal;
    _Postfix: TEpithet;
    FMultiLine: boolean;
  strict protected
    function InternalParse: boolean; override;
    function ParseSQLStatement: TStatement; virtual;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function IsSimpleIdent: boolean;
    property MultiLine: boolean read FMultiLine write FMultiLine;
  end;

  { Информация о форматировании выражения }
  TTermInfo = record
    Priority, SingleLineLen, MultiLineLen, PrevDelimiterLen, PostDelimiterLen: integer;
    HasOp, SingleLine, LineBreak, BreakBeforeDelimiter, BreakAfterDelimiter: boolean;
    TermRulerName, DelimRulerName: string;
  end;

  { Выражение }
  TExpression = class(TStatementList<TTerm>)
  strict private
    LineCount: integer;
    function GetMultiLine: boolean;
  strict protected
    TermInfo: array of TTermInfo;
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
    function ParseDelimiter(out AResult: TObject): boolean; override;
    function ParseBreak: boolean; override;
    function OnePerLine: boolean; override;
    function ForcedLineBreaks: boolean; virtual;
  public
    class function Candidates(AParent: TStatement): TArray<TStatementClass>; override;
    function Aligned: TAlignMode; override;
    property IsMultiLine: boolean read GetMultiLine;
  end;

implementation

uses Parser, Commons, DML, PLSQL, Keywords, FormatterPrinter, Select;

var
  Operations: TDictionary<String, integer>;

function HasOperation(AOperation: TToken; out APriority: integer): boolean;
var S: string;
begin
  if not Assigned(AOperation) then exit(false);
  S := AOperation.Value.ToLower;
  Result := Operations.ContainsKey(S);
  if Result then APriority := Operations[S];
end;

type

  { Выражение в скобках }
  TBracketedExpression = class(TBracketedStatement<TExpression>)
  public
    function MultiLine: boolean; override;
  end;

  { Выражение case }
  TCase = class(TStatement)
  strict private
    _Case: TEpithet;
    _Expression: TStatement;
    _Sections: TStatement;
    _End: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Секция выражения case }
  TCaseSection = class(TStatement)
  strict private
    _When: TEpithet;
    _Condition: TStatement;
    _Then: TEpithet;
    _Else: TEpithet;
    _Value: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
    function StatementName: string; override;
  end;

  { Секции выражения case }
  TCaseSections = class(TStatementList<TCaseSection>)
  strict protected
    function ParseBreak: boolean; override;
    function OnePerLine: boolean; override;
  end;

  { Выражение cast }
  TCast = class(TStatement)
  strict private
    _Cast: TEpithet;
    _OpenBracket: TTerminal;
    _Expression: TStatement;
    _As: TEpithet;
    _TypeRef: TStatement;
    _CloseBracket: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

{ TExpression }

function TTerm.InternalParse: boolean;
begin
  Result := false;
  { Префиксы }
  _Prefix := Keyword(['not', 'prior', 'distinct', 'unique', 'all']);
  if not Assigned(_Prefix) then
  begin
    _Prefix := Terminal(['-', '+']);
    if Assigned(_Prefix) then TTerminal(_Prefix).OpType := otUnary;
  end;
  try
    { Слагаемое может быть числом }
    _Number := Number;
    if Assigned(_Number) then exit(true);
    { Литералом }
    _Literal := Literal;
    if Assigned(_Literal) then exit(true);
    { Ключевым словом null, true или false }
    _KeywordValue := Keyword(['null', 'false', 'true']);
    if Assigned(_KeywordValue) then exit(true);
    { date/timestamp литералом }
    _LiteralPrefix := Keyword(['date', 'timestamp']);
    if Assigned(_LiteralPrefix) then
    begin
      _Literal := Literal;
      exit(true);
    end;
    { SQL-выражением }
    _SQLStatement := ParseSQLStatement;
    if Assigned(_SQLStatement) then exit(true);
    { Выражением case }
    if TCase.Parse(Self, Source, _Case) then exit(true);
    { Выражением cast }
    if TCast.Parse(Self, Source, _Cast) then exit(true);
    { Идентификатором или подобным выражением, включая вызов функции }
    if TQualifiedIndexedIdent.Parse(Self, Source, _Ident) then
    begin
      _Suffix := Terminal(['%rowcount', '%found', '%notfound', '%isopen']);
      exit(true);
    end;
    { Вложенным запросом }
    if TBracketedStatement<TSelect>.Parse(Self, Source, _Select) then exit(true);
    { Выражением в скобках }
    if TBracketedExpression.Parse(Self, Source, _Expression) then exit(true);
  finally
    { Суффиксы }
    _OuterJoin := Terminal('(+)');
    _Postfix := Keyword(['is null', 'is not null']);
  end;
end;

function TTerm.ParseSQLStatement: TStatement;
begin
  Result := nil; { предназначен для перекрытия в TSQLTerm }
end;

procedure TTerm.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Prefix, _Number, _LiteralPrefix, _Literal, _SQLStatement, _Ident,
    _Suffix, _KeywordValue, _Case, _Cast, _Select, _Expression, _OuterJoin, _Postfix]);
end;

function TTerm.IsSimpleIdent: boolean;
begin
  Result := Assigned(_Ident) and (_Ident as TQualifiedIndexedIdent).IsSimpleIdent and
    not Assigned(_Prefix) and not Assigned(_Suffix) and not Assigned(_Postfix);
end;

{ TExpression }

function TExpression.InternalParse: boolean;
begin
  Result := inherited;
  { Если на верхнем уровне встретили одинокое непонятное слово - не будем
    распознавать его как тривиальное выражение, пусть лучше будет unexpected token }
  if not Assigned(Parent) and (Count = 1) and TTerm(Item(0)).IsSimpleIdent then
    Result := false;
end;

procedure TExpression.InternalPrintSelf(APrinter: TPrinter);

  { Сбор информации о размерах }
  procedure CollectInfo;
  var
    DraftPrinter: TFormatterPrinter;
    i, DelimiterLen: integer;
    LastTerm: TTerm;
    LastToken: TToken;
  begin
    SetLength(TermInfo, Count);
    DraftPrinter := TDraftPrinter.Create(APrinter.Settings, true, [poAbove, poBelow, poBelowBOL, poFarAbove, poFarBelow], false, false);
    try
      DraftPrinter.BeginPrint;
      for i := 0 to Count - 1 do
      begin
        { Померяем ширину при печати в обычном режиме }
        DraftPrinter.Clear;
        DraftPrinter.PrintItem(Item(i));
        TermInfo[i].MultiLineLen := DraftPrinter.CurrentMaxWidth;
        { Померяем ширину при печати в однострочном режиме }
        DraftPrinter.Clear;
        DraftPrinter.SupressNextLine(true);
        DraftPrinter.PrintItem(Item(i));
        TermInfo[i].SingleLineLen := DraftPrinter.CurrentMaxWidth;
        DraftPrinter.SupressNextLine(false);
        { Если при печати были строчные коммментарии - в одну строку не уложимся и не будем пытаться }
        if (DraftPrinter.CurrentLine > 1) and not Settings.ChangeCommentType then TermInfo[i].SingleLineLen := MaxInt;
        { Померяем ширину разделителя }
        DraftPrinter.Clear;
        DraftPrinter.PrintItem(Delimiter(i));
        DelimiterLen := DraftPrinter.CurrentMaxWidth;
        { Заполним информацию о терминальном символе }
        TermInfo[i].Priority := 666;
        TermInfo[i].HasOp := HasOperation(TToken(Delimiter(i)), TermInfo[i].Priority);
        if TermInfo[i].HasOp then
        begin
          { У and, относящегося к between, опустим приоритет так, чтобы по нему не переносилось }
          if (i > 0) and Assigned(Delimiter(i)) and Assigned(Delimiter(i-1))
            and ((Delimiter(i) as TToken).Value = 'and')
            and ((Delimiter(i - 1) as TToken).Value.Contains('between')) then
            TermInfo[i].Priority := 666;
          { В зависимости от операции проставим перенос до или после операции }
          if TermInfo[i].Priority < 0 then
            begin
              TermInfo[i].BreakBeforeDelimiter := true;
              if i < Count - 1 then TermInfo[i + 1].PrevDelimiterLen := DelimiterLen;
            end
          else
            begin
              TermInfo[i].BreakAfterDelimiter := true;
              TermInfo[i].PostDelimiterLen := DelimiterLen;
            end;
        end;
        with TermInfo[i] do
          SingleLine := (SingleLineLen = MultiLineLen) or (SingleLineLen <= Settings.PreferredExpressionLength);
      end;
    finally
      DraftPrinter.EndPrint;
      DraftPrinter.Free;
    end;
    { Если выражение завершается строчным комментарием, отметим его флагом }
    LastTerm := TTerm(Item(Count - 1));
    if not Assigned(LastTerm) then exit;
    LastToken := LastTerm.LastToken;
    if not Assigned(LastToken) then exit;
    if not Assigned(LastToken.CommentAfter) then exit;
    LastToken.CommentAfter.FixBug71 := true;
  end;

  procedure PutLineBreaks(Start, Finish: integer); forward;

  { Примерка ширины печати в различных вариантах и разбиение на строки }
  procedure CheckForBreaks(Start: integer = 0; Finish: integer = -1);
  var
    Len, i: integer;
  begin
    if Finish < 0 then Finish := Count - 1;
    { Прикинем ширину, если печатать в одну строку }
    Len := 0;
    for i := Start to Finish do
    begin
      if TermInfo[i].SingleLine
        then Inc(Len, TermInfo[i].SingleLineLen)
        else Inc(Len, TermInfo[i].MultiLineLen);
      Inc(Len, TermInfo[i].PrevDelimiterLen + TermInfo[i].PostDelimiterLen);
      { Если сказано насильно разбивать по and/or, выполним это }
      if ForcedLineBreaks and
         (Start = 0) and
         (Finish = Count - 1) and
         (i > Start) and
         (TermInfo[i - 1].Priority < 0) then Len := MaxInt div 4;
    end;
    { Если не влезает - придётся разбивать на части }
    if (Len > Settings.PreferredExpressionLength) and (Start < Finish) then
      PutLineBreaks(Start, Finish);
  end;

  { Специальный алгоритм расстановки переносов в случае конкатенации }
  function DoConcatenationBreaks(Start, Finish: integer): boolean;
  var
    Texts: TDictionary<string, TList<integer>>;
    DraftPrinter: TFormatterPrinter;
    Text, Found, PreFound: string;
    D: TObject;
    i, Prev, Dist, NewDist: integer;
    CurPriority, MaxPriority: double;
    L1, L2: TList<integer>;
  begin
    Result := true;
    Texts := nil;
    DraftPrinter := nil;
    try
      { Пробежим по элементам выражения }
      for i := Start + 1 to Finish do
      begin
        D := Delimiter(i - 1);
        { Если это не конкатенация, просто выйдем }
        if not (D is TToken) or (TToken(D).Value <> '||') then exit(false);
        { В противном случае возьмём текст выражения }
        if not Assigned(DraftPrinter) then DraftPrinter := TDraftPrinter.Create(APrinter.Settings);
        DraftPrinter.BeginPrint;
        DraftPrinter.PrintItem(Item(i));
        Text := DraftPrinter.GetText;
        DraftPrinter.EndPrint;
        { И запомним его в списке }
        if not Assigned(Texts) then Texts := TObjectDictionary<string, TList<integer>>.Create([doOwnsValues]);
        if not Texts.ContainsKey(Text) then Texts.Add(Text, TList<integer>.Create());
        Texts[Text].Add(i);
      end;
      { Теперь попробуем найти самый повторяющийся текст. При одинаковой повторяемости
        (может быть, например, при || chr(13) || chr(10)) возьмём тот, что больше
        тяготеет к концу выражения, так как перенос нужно ставить после него }
      Found := '';
      MaxPriority := -1;
      for Text in Texts.Keys do
        if Texts[Text].Count > 1 then
        begin
          CurPriority := 0.0;
          for i in Texts[Text] do CurPriority := CurPriority + i / 1000 + 1;
          if CurPriority > MaxPriority then
          begin
            MaxPriority := CurPriority;
            Found := Text;
          end;
        end;
      if Found = '' then exit(false);
      { Расставим переносы по найденным элементам }
      Prev := Start;
      for i in Texts[Found] do
      begin
        TermInfo[i].LineBreak := true;
        if (Prev > Start) or (i < Finish) then CheckForBreaks(Prev, i);
        Prev := i + 1;
      end;
      if Prev > Start then CheckForBreaks(Prev, Finish);
      { А теперь самое интересное. Чтобы выровнять конструкции типа || chr(13) || chr(10),
        нам нужно, стартуя от Found = 'chr(10)', найти разделитель, стоящий перед
        chr(13) }
      PreFound := Found;
      Dist := 0;
      L2 := Texts[Found];
      { Пробежимся по всем текстам и найдём тот, который стоит перед каждым Found
        на одном и том же расстоянии влево, и это расстояние максимально }
      for Text in Texts.Keys do
      begin
        L1 := Texts[Text];
        if L1.Count <> L2.Count then continue;
        NewDist := L1[0] - L2[0];
        if NewDist >= Dist then continue;
        for i := 1 to L1.Count - 1 do
          if L1[i] - L2[i] <> NewDist then NewDist := MaxInt;
        if NewDist >= Dist then continue;
        PreFound := Text;
        Dist := NewDist;
      end;
      { Теперь остаётся поставить линейку на предыдущие разделители }
      for i in Texts[PreFound] do
        if i > 0 then TermInfo[i - 1].DelimRulerName := CONCAT_DELIM_RULER;
    finally
      FreeAndNil(Texts);
      FreeAndNil(DraftPrinter);
    end;
  end;

  { Расстановка переносов по самым низкоприоритетным операциям }
  procedure PutLineBreaks(Start, Finish: integer);
  var i, MinPriority, Prev: integer;
  begin
    { Определим наименьший приоритет операции в выбранном фрагменте }
    MinPriority := MaxInt;
    for i := Start to Finish - 1 do
      if TermInfo[i].Priority < MinPriority then MinPriority := TermInfo[i].Priority;
    { Если это операция конкатенации, используем специальный алгоритм }
    if DoConcatenationBreaks(Start, Finish) then exit;
    { И расставим переносы по таким операциям }
    Prev := Start;
    for i := Start to Finish do
    begin
      if not TermInfo[i].HasOp then continue;
      if TermInfo[i].Priority > MinPriority then continue;
      TermInfo[i].LineBreak := true;
      if (Prev > Start) or (i < Finish) then CheckForBreaks(Prev, i);
      Prev := i + 1;
    end;
    if Prev > Start then CheckForBreaks(Prev, Finish);
  end;

  { Проверка на одиночный длинный операнд }
  function IsLongTerm(Index: integer): boolean;
  var First, Last: boolean;
  begin
    First := (Index = 0);
    Last  := (Index = Count - 1);
    Result := { разделитель в конце }
              (Last or TermInfo[Index].BreakAfterDelimiter) and
              { перенос перед этим операндом }
              (First or TermInfo[Index - 1].LineBreak) and
              { перенос после этого операнда }
              (Last or TermInfo[Index].LineBreak) and
              { операнд достаточно длинный }
              (not TermInfo[Index].SingleLine or
              (TermInfo[Index].SingleLineLen > Settings.PreferredExpressionLength div 2)) and
              { и операция не запятая }
              (Last or
               (Delimiter(Index) is TToken) and (TToken(Delimiter(Index)).Value <> ','));
  end;

  { Улучшение читаемости выражений с длинными операндами }
  procedure BeautifyLongTerms;
  var i: integer;
  begin
    if not Settings.BeautifyLongOperands then exit;
    { Если у нас в строке один длинный операнд, для читаемости перенесём знак операции на следующую строку }
    for i := 0 to Count - 1 do
      if IsLongTerm(i) then
        TermInfo[i].BreakBeforeDelimiter := true;
    { Теперь у нас могли образоваться знаки операций, расположенные на отдельных строках.
      Уберём у них перенос перед следующими строками, где это оправдано }
    for i := 0 to Count - 2 do
      if TermInfo[i].BreakBeforeDelimiter and
         TermInfo[i].BreakAfterDelimiter and
         not IsLongTerm(i + 1) and
         not TermInfo[i + 1].BreakBeforeDelimiter and
         (TermInfo[i + 1].PostDelimiterLen > 0)
      then
        TermInfo[i].BreakAfterDelimiter := false;
  end;

  { Расстановка выравниваний для операций типа условий в where }
  procedure PlaceExprDelimRulers;
  var i: integer;
  begin
    { В коротких выражениях нечего выравнивать }
    if Count < 4 then exit;
    { Расстановка разрешена, если перенос идёт на каждой второй операции }
    for i := 0 to Count - 2 do
      if TermInfo[i].LineBreak xor (i mod 2 = 1) then exit;
    { Расставим линейки }
    for i := 0 to Count - 2 do
      if i mod 2 = 0 then TermInfo[i].DelimRulerName := EXPR_DELIM_RULER;
  end;

  { Печать получившегося выражения }
  procedure PrintExpression;
  var i: integer;
  begin
    APrinter.PushIndent;
    for i := 0 to Count - 1 do
    begin
      if TermInfo[i].SingleLine then
        APrinter.SupressNextLine(true)
      else if i > 0 then
        APrinter.NextLine;
      if Item(i) is TTerm then
        TTerm(Item(i)).MultiLine := not TermInfo[i].SingleLine;
      if TermInfo[i].TermRulerName <> '' then
        APrinter.PrintRulerItems(TermInfo[i].TermRulerName, []);
      APrinter.PrintItem(Item(i));
      if TermInfo[i].SingleLine then
        APrinter.SupressNextLine(false);
      if TermInfo[i].LineBreak and TermInfo[i].BreakBeforeDelimiter then APrinter.NextLine;
      if TermInfo[i].DelimRulerName <> ''
        then APrinter.PrintRulerItems(TermInfo[i].DelimRulerName, [Delimiter(i)])
        else APrinter.PrintItem(Delimiter(i));
      if TermInfo[i].LineBreak and TermInfo[i].BreakAfterDelimiter then APrinter.NextLine;
    end;
    APrinter.PopIndent;
  end;

begin
  if not Assigned(TermInfo) then
  begin
    CollectInfo;
    CheckForBreaks;
    BeautifyLongTerms;
    PlaceExprDelimRulers;
  end;
  PrintExpression;
end;

function TExpression.ParseDelimiter(out AResult: TObject): boolean;
var
  T: TToken;
  D: integer;
begin
  T := NextToken;
  if T is TEpithet then TEpithet(T).IsKeyword := true;
  if HasOperation(T, D) and ((T.Value <> ',') or (Self.Parent is TBracketedExpression)) then AResult := T;
  Result := Assigned(AResult);
  if AResult is TTerminal then TTerminal(AResult).OpType := otBinary;
end;

function TExpression.ParseBreak: boolean;
begin
  Result := true;
end;

function TExpression.OnePerLine: boolean;
begin
  Result := false;
end;

function TExpression.ForcedLineBreaks: boolean;
begin
  Result := false;
end;

class function TExpression.Candidates(AParent: TStatement): TArray<TStatementClass>;
begin
  while Assigned(AParent) and not (AParent is TDML) do AParent := AParent.Parent;
  if AParent is TDML
    then Result := [TSQLExpression]
    else Result := [TExpression];
end;

function TExpression.Aligned: TAlignMode;
begin
  Result := AlignMode(Settings.AlignExpressions and (Self.Count > 1));
end;

function TExpression.GetMultiLine: boolean;
var Printer: TFormatterPrinter;
begin
  if LineCount > 0 then exit(LineCount > 1);
  Printer := TDraftPrinter.Create(Settings, true, [], false, false);
  try
    Printer.BeginPrint;
    PrintSelf(Printer);
    Printer.EndPrint;
    LineCount := Printer.CurrentLine;
  finally
    FreeAndNil(Printer);
  end;
  Result := (LineCount > 1);
end;

{ TCase }

function TCase.InternalParse: boolean;
begin
  _Case := Keyword('case');
  if not Assigned(_Case) then exit(false);
  TExpression.Parse(Self, Source, _Expression);
  TCaseSections.Parse(Self, Source, _Sections);
  _End := Keyword('end');
  Result := true;
end;

procedure TCase.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Case, _IndentNextLine, _Expression, _NextLine]);
  if Assigned(_Expression) then APrinter.Undent;
  APrinter.PrintItems([_Sections, _NextLine]);
  if not Assigned(_Expression) then APrinter.Undent;
  APrinter.PrintItem(_End);
end;

{ TCaseSection }

function TCaseSection.InternalParse: boolean;
begin
  _When := Keyword('when');
  if Assigned(_When) then
    begin
      TExpression.Parse(Self, Source, _Condition);
      _Then := Keyword('then');
    end
  else
    _Else := Keyword('else');
  if not Assigned(_When) and not Assigned(_Else) then exit(false);
  TExpression.Parse(Self, Source, _Value);
  Result := true;
end;

function TCaseSection.StatementName: string;
begin
  Result := Concat([_When, _Else]);
end;

procedure TCaseSection.InternalPrintSelf(APrinter: TPrinter);
var _NextIfMultiLine: TObject;
begin
  if (_Condition is TExpression) and TExpression(_Condition).IsMultiLine
    then _NextIfMultiLine := _NextLine
    else _NextIfMultiLine := nil;
  if Assigned(_When)
    then APrinter.PrintItems([_When, _Condition, _NextIfMultiLine, _Then, _IndentNextLine, _Value, _Undent])
    else APrinter.PrintItems([_Else, _IndentNextLine, _Value, _Undent]);
end;

{ TCaseSections }

function TCaseSections.ParseBreak: boolean;
begin
  Result := not Assigned(Keyword(['when', 'else']));
end;

function TCaseSections.OnePerLine: boolean;
begin
  Result := true;
end;

{ TCast }

function TCast.InternalParse: boolean;
begin
  _Cast := Keyword('cast');
  if not Assigned(_Cast) then exit(false);
  _OpenBracket := Terminal('(');
  TExpression.Parse(Self, Source, _Expression);
  _As := Keyword('as');
  TTypeRef.Parse(Self, Source, _TypeRef);
  _CloseBracket := Terminal(')');
  Result := true;
end;

procedure TCast.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Cast, _OpenBracket, _Expression, _As, _TypeRef, _CloseBracket]);
end;

{ TBracketedExpression }

function TBracketedExpression.MultiLine: boolean;
begin
  Result := (InnerStatement is TExpression) and TExpression(InnerStatement).IsMultiLine;
end;

initialization
  { Заполняем список ключевых слов для выражений }
  Keywords.RegisterKeywords(TExpression, ['case', 'when']);
  { Заполняем список операций с их приоритетами (Внимание! приоритеты имеются в
    виду с точки зрения расстановки переносов и не обязаны соответствовать
    чему-то другому)}
  Operations := TDictionary<String, integer>.Create;
  Operations.Add('or', -2);
  Operations.Add('and', -1);
  Operations.Add('&', 0); // хак для бага 73
  Operations.Add('between', 2);
  Operations.Add('not between', 2);
  Operations.Add('like', 3);
  Operations.Add('not like', 3);
  Operations.Add('escape', 4);
  Operations.Add('in', 3);
  Operations.Add('not in', 3);
  Operations.Add('=', 4);
  Operations.Add('<', 4);
  Operations.Add('>', 4);
  Operations.Add('<=', 4);
  Operations.Add('>=', 4);
  Operations.Add('<>', 4);
  Operations.Add('!=', 4);
  Operations.Add('^=', 4);
  Operations.Add('||', 5);
  Operations.Add('*', 6);
  Operations.Add('/', 6);
  Operations.Add('+', 7);
  Operations.Add('-', 7);
  Operations.Add(',', 8);
  Operations.Add('multiset except', 6);
  Operations.Add('multiset except all', 6);
  Operations.Add('multiset except distinct', 6);
  Operations.Add('multiset intersect', 6);
  Operations.Add('multiset intersect all', 6);
  Operations.Add('multiset intersect distinct', 6);
  Operations.Add('multiset union', 6);
  Operations.Add('multiset union all', 6);
  Operations.Add('multiset union distinct', 6);

finalization
  FreeAndNil(Operations);

end.
