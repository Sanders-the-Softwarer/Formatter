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

uses Classes, SysUtils, Math, Tokens, Statements, PrinterIntf,
  System.Generics.Collections, Utils;

type

  { Элемент выражения }
  TTerm = class(TStatement)
  strict private
    _Prefix: TToken;
    _Number: TNumber;
    _Literal: TLiteral;
    _SQLStatement: TStatement;
    _Ident: TStatement;
    _Suffix: TTerminal;
    _KeywordValue: TEpithet;
    _Select: TStatement;
    _OpenBracket: TTerminal;
    _Expression: TStatement;
    _CloseBracket: TTerminal;
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
    Priority, SingleLineLen, MultiLineLen, PrevDelimiterLen, PostDelimiterLen, RulerNumber: integer;
    HasOp, SingleLine, LineBreak, BreakBeforeDelimiter: boolean;
    RulerChar: char;
  end;

  { Выражение }
  TExpression = class(TStatementList<TTerm>)
  strict private
    class var FlagCreatedRight: boolean;
  strict private
    CompleteKeywords: TKeywords;
    LineCount: integer;
    function GetMultiLine: boolean;
  strict protected
    TermInfo: array of TTermInfo;
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
    function ParseDelimiter(out AResult: TObject): boolean; override;
    function ParseBreak: boolean; override;
    function GetKeywords: TKeywords; override;
    function OnePerLine: boolean; override;
    function ForcedLineBreaks: boolean; virtual;
  public
    class procedure CreatedRight;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property IsMultiLine: boolean read GetMultiLine;
  end;

implementation

uses Parser, Commons, DML, PLSQL, FormatterPrinter;

var
  ExpressionKeywords: TKeywords;
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
    { SQL-выражением }
    _SQLStatement := ParseSQLStatement;
    if Assigned(_SQLStatement) then exit(true);
    { Выражением case }
    if TCase.Parse(Self, Source, _Case) then exit(true);
    { Выражением cast }
    if TCast.Parse(Self, Source, _Cast) then exit(true);
    { Идентификатором или подобным выражением, включая вызов функции }
    TQualifiedIndexedIdent.Parse(Self, Source, _Ident);
    if Assigned(_Ident) then
    begin
      _Suffix := Terminal(['%rowcount', '%found', '%notfound', '%isopen']);
      exit(true);
    end;
    { Вложенным запросом }
    if TInnerSelect.Parse(Self, Source, _Select) then exit(true);
    { Выраженим в скобках }
    _OpenBracket := Terminal('(');
    if Assigned(_OpenBracket) then
    begin
      TParser.ParseExpression(Self, Source, _Expression);
      _CloseBracket := Terminal(')');
      exit(true);
    end;
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
  APrinter.PrintItems([_Prefix, _Number, _Literal, _SQLStatement, _Ident, _Suffix, _KeywordValue, _Case, _Cast]);
  if Assigned(_Select) then
    APrinter.PrintItem(_Select);
  if Assigned(_Expression) then
  begin
    APrinter.PrintItem(_OpenBracket);
    if MultiLine then APrinter.PrintItem(_IndentNextLine);
    APrinter.PrintItem(_Expression);
    if MultiLine then APrinter.PrintItem(_UndentNextLine);
    APrinter.PrintItem(_CloseBracket);
  end;
  APrinter.PrintItems([_OuterJoin, _Postfix]);
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
    i: integer;
  begin
    SetLength(TermInfo, Count);
    DraftPrinter := TFormatterPrinter.Create(APrinter.Settings, true, [poAbove, poBelow, poFarAbove, poFarBelow], false);
    try
      DraftPrinter.BeginPrint;
      for i := 0 to Count - 1 do
      begin
        DraftPrinter.NextLine;
        DraftPrinter.PrintItem(Item(i));
        TermInfo[i].MultiLineLen := DraftPrinter.CurrentCol;
        DraftPrinter.NextLine;
        DraftPrinter.SupressNextLine(true);
        DraftPrinter.PrintItem(Item(i));
        TermInfo[i].SingleLineLen := DraftPrinter.CurrentCol;
        DraftPrinter.SupressNextLine(false);
        DraftPrinter.NextLine;
        DraftPrinter.PrintItem(Delimiter(i));
        TermInfo[i].Priority := 666;
        TermInfo[i].HasOp := HasOperation(TToken(Delimiter(i)), TermInfo[i].Priority);
        if TermInfo[i].HasOp then
        begin
          if (i > 0) and Assigned(Delimiter(i)) and Assigned(Delimiter(i-1))
            and ((Delimiter(i) as TToken).Value = 'and')
            and ((Delimiter(i - 1) as TToken).Value.Contains('between')) then
            TermInfo[i].Priority := 666;
          TermInfo[i].BreakBeforeDelimiter := (TermInfo[i].Priority < 0);
          if not TermInfo[i].BreakBeforeDelimiter then
            TermInfo[i].PostDelimiterLen := DraftPrinter.CurrentCol
          else if i < Count - 1 then
            TermInfo[i + 1].PrevDelimiterLen := DraftPrinter.CurrentCol;
        end;
        with TermInfo[i] do
          SingleLine := (SingleLineLen = MultiLineLen) or (SingleLineLen <= Settings.PreferredExpressionLength);
      end;
    finally
      DraftPrinter.EndPrint;
      DraftPrinter.Free;
    end;
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

  { Разметка выравниваний }
  procedure PutRulers(Start, Finish: integer);
  var i, Prio, Min, Max: integer;
  begin
    if not Self.Aligned then exit;
    { Проверим, что непереносимые операции одного приоритета }
    Min := MaxInt;
    Max := -MaxInt;
    for i := Start to Finish do
      if not TermInfo[i].LineBreak and TermInfo[i].HasOp then
      begin
        Prio := TermInfo[i].Priority;
        if Min > Prio then Min := Prio;
        if Max < Prio then Max := Prio;
      end;
    if Min <> Max then exit;
    { Проверим, что они идут строго по одной в строке }
    for i := Start to Finish - 1 do
      if TermInfo[i].LineBreak xor ((i - Start) mod 2 = 1) then
        exit;
    { Разметим линейки }
    for i := Start to Finish do
    begin
      TermInfo[i].RulerNumber := Finish; { Start не подходит, так как может быть нулём }
      if not TermInfo[i].LineBreak and TermInfo[i].SingleLine
        then TermInfo[i].RulerChar := 'L'
        else TermInfo[i].RulerChar := 'R';
    end;
  end;

  { Специальный алгоритм расстановки переносов в случае конкатенации }
  function DoConcatenationBreaks(Start, Finish: integer): boolean;
  var
    Texts: TDictionary<string, TList<integer>>;
    DraftPrinter: TFormatterPrinter;
    Text, Found: string;
    D: TObject;
    i, Prev: integer;
    CurPriority, MaxPriority: double;
  begin
    Result := true;
    Texts := nil;
    DraftPrinter := nil;
    try
      { Пробежим по элементам выражения }
      for i := Start to Finish - 1 do
      begin
        D := Delimiter(i);
        { Если это не конкатенация, просто выйдем }
        if not (D is TToken) or (TToken(D).Value <> '||') then exit(false);
        { В противном случае возьмём текст выражения }
        if not Assigned(DraftPrinter) then DraftPrinter := TFormatterPrinter.Create(APrinter.Settings);
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
        (может быть, например, при || chr(13) || chr(10)) возмём тот, что больше
        тяготеет к концу выражения (чтобы не влепить разбиение между элементами
        из примера выше)}
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
      { Расставим выравнивания }
      PutRulers(Start, Finish);
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
    { Расставим выравнивания }
    PutRulers(Start, Finish);
  end;

  { Печать получившегося выражения }
  procedure PrintExpression;

    var
      i, Cnt: integer;
      RulerName: string;

    procedure NewLineCombo;
    begin
      APrinter.NextLine;
//      APrinter.StartRuler(Settings.AlignExpressions and Self.Aligned);
      Cnt := 0;
      RulerName := 'expr-0';
    end;

  begin
    APrinter.PushIndent;
//    APrinter.StartRuler(Settings.AlignExpressions and Self.Aligned);
    Cnt := 0;
    RulerName := 'expr-0';
    for i := 0 to Count - 1 do
    begin
      if TermInfo[i].SingleLine then
        APrinter.SupressNextLine(true)
      else if i > 0 then
        NewLineCombo;
      if Item(i) is TTerm then
        TTerm(Item(i)).MultiLine := not TermInfo[i].SingleLine;
      if Cnt <> 1 then
        begin
//          APrinter.PrintRulerItem(RulerName, Item(i));
          APrinter.PrintItem(Item(i));
          Inc(Cnt);
          RulerName := Format('expr-%d', [Cnt]);
        end
      else
        APrinter.PrintItem(Item(i));
      if TermInfo[i].SingleLine then
        APrinter.SupressNextLine(false)
      else
        APrinter.Undent;
      if TermInfo[i].LineBreak and TermInfo[i].BreakBeforeDelimiter then NewLineCombo
      else if TermInfo[i].LineBreak and ((i = 0) or (TermInfo[i - 1].LineBreak) and
              (not TermInfo[i].SingleLine or TermInfo[i].SingleLine and (TermInfo[i].SingleLineLen > Settings.PreferredExpressionLength div 2)))
              and (Delimiter(i) is TToken)
              and (TToken(Delimiter(i)).Value <> ',')
        then NewLineCombo;
//      APrinter.PrintRulerItem(RulerName, Delimiter(i));
      APrinter.PrintItem(Delimiter(i));
      Inc(Cnt);
      RulerName := Format('expr-%d', [Cnt]);
      if TermInfo[i].LineBreak and not TermInfo[i].BreakBeforeDelimiter then NewLineCombo;
    end;
    APrinter.PopIndent;
  end;

begin
  if not Assigned(TermInfo) then
  begin
    CollectInfo;
    CheckForBreaks;
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
  if HasOperation(T, D) and ((T.Value <> ',') or (Self.Parent is TTerm)) then AResult := T;
  Result := Assigned(AResult);
  if AResult is TTerminal then TTerminal(AResult).OpType := otBinary;
end;

function TExpression.ParseBreak: boolean;
begin
  Result := true;
end;

function TExpression.GetKeywords: TKeywords;
begin
  if not Assigned(CompleteKeywords) then
    CompleteKeywords := TKeywords.Create(ExpressionKeywords, inherited);
  Result := CompleteKeywords;
end;

function TExpression.OnePerLine: boolean;
begin
  Result := false;
end;

function TExpression.ForcedLineBreaks: boolean;
begin
  Result := false;
end;

class procedure TExpression.CreatedRight;
begin
  FlagCreatedRight := true;
end;

procedure TExpression.AfterConstruction;
begin
  inherited;
  if FlagCreatedRight
    then FlagCreatedRight := false
    else raise Exception.Create('Expression must be created using TParser.ParseExpression, do it!');
end;

procedure TExpression.BeforeDestruction;
begin
  FreeAndNil(CompleteKeywords);
  inherited;
end;

function TExpression.GetMultiLine: boolean;
var Printer: TFormatterPrinter;
begin
  if LineCount > 0 then exit(LineCount > 1);
  Printer := TFormatterPrinter.Create(Settings, true, [], false);
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
  TParser.ParseExpression(Self, Source, _Expression);
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
      TParser.ParseExpression(Self, Source, _Condition);
      _Then := Keyword('then');
    end
  else
    _Else := Keyword('else');
  if not Assigned(_When) and not Assigned(_Else) then exit(false);
  TParser.ParseExpression(Self, Source, _Value);
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
  TParser.ParseExpression(Self, Source, _Expression);
  _As := Keyword('as');
  TTypeRef.Parse(Self, Source, _TypeRef);
  _CloseBracket := Terminal(')');
  Result := true;
end;

procedure TCast.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Cast, _OpenBracket, _Expression, _As, _TypeRef, _CloseBracket]);
end;

initialization
  { Заполняем список ключевых слов для выражений }
  ExpressionKeywords := TKeywords.Create(['case', 'when']);
  { Заполняем список операций с их приоритетами (Внимание! приоритеты имеются в
    виду с точки зрения расстановки переносов и не обязаны соответствовать
    чему-то другому)}
  Operations := TDictionary<String, integer>.Create;
  Operations.Add('or', -2);
  Operations.Add('and', -1);
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
  FreeAndNil(ExpressionKeywords);
  FreeAndNil(Operations);

end.
