////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//   Синтаксические конструкции арифметических-логических-прочих выражений    //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
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

uses Classes, SysUtils, Math, Tokens, Statements, Printers_, Attributes,
  System.Generics.Collections;

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

  { Выражение }
  TExpression = class(TStatementList<TTerm>)
  strict private
    class var FlagCreatedRight: boolean;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
    function ParseDelimiter(out AResult: TObject): boolean; override;
    function ParseBreak: boolean; override;
    function GetKeywords: TStrings; override;
    function OnePerLine: boolean; override;
    function ForcedLineBreaks: boolean; virtual;
  public
    class procedure CreatedRight;
    procedure AfterConstruction; override;
  end;

  { Квалифицированный идентификатор }
  TQualifiedIdent = class(TStatement)
  strict private
    _Dot: TTerminal;
    _Name: TEpithet;
    _Next: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
    function IsSimpleIdent: boolean;
  end;

  { Индексированный квалифицированный идентификатор }
  TQualifiedIndexedIdent = class(TStatement)
  strict private
    _Indexes: TStatement;
    _Dot: TTerminal;
    _Ident: TStatement;
    _Next: TStatement;
  strict protected
    function TopStatement: boolean;
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function IsSimpleIdent: boolean;
  end;

  { Вызов функции }
  TFunctionCall = class(TStatement)
  strict private
    _Name: TStatement;
    _OpenBracket: TTerminal;
    _Arguments: TStatement;
    _CloseBracket: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Аргумент вызова подпрограммы }
  TArgument = class(TStatement)
  strict private
    _Ident: TEpithet;
    _Assignment: TTerminal;
    _Expression: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Аргументы вызова подпрограммы }
  [Aligned]
  TArguments = class(TCommaList<TArgument>)
  strict protected
    function ParseBreak: boolean; override;
    function OnePerLine: boolean; override;
  end;

implementation

uses Parser, DML, PLSQL;

var
  Keywords: TStringList;
  Operations: TDictionary<String, integer>;

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
  begin
    APrinter.NextLine;
    APrinter.PrintItem(_Select);
    APrinter.NextLine;
  end;
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

  type
    TTermInfo = record
      SingleLineLen, MultiLineLen, PrevDelimiterLen: integer;
      SingleLine, LineBreak: boolean;
    end;

  var
    TermInfo: array of TTermInfo;

  { Сбор информации о размерах }
  procedure CollectInfo;
  var
    DraftPrinter: TPrinter;
    i: integer;
  begin
    SetLength(TermInfo, Count);
    DraftPrinter := APrinter.MakeDraftPrinter;
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
        if i > 0 then TermInfo[i - 1].PrevDelimiterLen := DraftPrinter.CurrentCol;
        if TermInfo[i].SingleLineLen = TermInfo[i].MultiLineLen then TermInfo[i].SingleLine := true;
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
    SLLen, MLLen, i: integer;
  begin
    if Finish < 0 then Finish := Count - 1;
    { Прикинем ширину, если печатать в одну строку }
    SLLen := 0;
    MLLen := 0;
    for i := Start to Finish do
    begin
      Inc(SLLen, TermInfo[i].SingleLineLen + TermInfo[i].PrevDelimiterLen);
      Inc(MLLen, TermInfo[i].MultiLineLen + TermInfo[i].PrevDelimiterLen);
      { Если сказано насильно разбивать по and/or, выполним это }
      if ForcedLineBreaks and
         (Start = 0) and
         (Finish = Count - 1) and
         (i > Start) and
         Assigned(Delimiter(i - 1)) and
         (Operations[TToken(Delimiter(i - 1)).Value] < 0) then
      begin
        SLLen := MaxInt div 4;
        MLLen := MaxInt div 4;
      end;
    end;
    { Если выражение влезает по ширине даже при печати в одну строку - так и поступим }
    if SLLen <= Settings.PreferredExpressionLength then
      for i := Start to Finish do
        TermInfo[i].SingleLine := true
    { Если же не влезает - придётся разбивать на части }
    else if (MLLen > Settings.PreferredExpressionLength) and (Start < Finish) then
      PutLineBreaks(Start, Finish);
  end;

  { Расстановка переносов по самым низкоприоритетным операциям }
  procedure PutLineBreaks(Start, Finish: integer);
  var
    i, MinPriority, CurPriority, Prev: integer;
    Delimiter_: string;
  begin
    { Определим наименьший приоритет операции в выбранном фрагменте }
    MinPriority := MaxInt;
    for i := Start to Finish - 1 do
    begin
      if not Assigned(Delimiter(i)) then continue;
      Delimiter_ := (Delimiter(i) as TToken).Value;
      if not Operations.ContainsKey(Delimiter_) then continue;
      CurPriority := Operations[Delimiter_];
      if CurPriority < MinPriority then MinPriority := CurPriority;
    end;
    { И расставим переносы по таким операциям }
    Prev := Start;
    for i := Start to Finish do
    begin
      if not Assigned(Delimiter(i)) then continue;
      Delimiter_ := (Delimiter(i) as TToken).Value;
      if not Operations.ContainsKey(Delimiter_) then continue;
      CurPriority := Operations[Delimiter_];
      if CurPriority > MinPriority then continue;
      TermInfo[i].LineBreak := true;
      if (Prev > Start) or (i < Finish) then CheckForBreaks(Prev, i);
      Prev := i + 1;
    end;
    if Prev > Start then CheckForBreaks(Prev, Finish);
  end;

  { Печать получившегося выражения }
  procedure PrintExpression;
  var i: integer;
  begin
    for i := 0 to Count - 1 do
    begin
      if TermInfo[i].SingleLine
        then APrinter.SupressNextLine(true)
        else APrinter.PrintItem(_IndentNextLine);
      if Item(i) is TTerm then
        TTerm(Item(i)).MultiLine := not TermInfo[i].SingleLine;
      APrinter.PrintItem(Item(i));
      if TermInfo[i].SingleLine
        then APrinter.SupressNextLine(false)
        else APrinter.Undent;
      if TermInfo[i].LineBreak then APrinter.NextLine;
      APrinter.PrintItem(Delimiter(i));
    end;
  end;

begin
  CollectInfo;
  CheckForBreaks;
  PrintExpression;
end;

function TExpression.ParseDelimiter(out AResult: TObject): boolean;
var
  T: TToken;
begin
  T := NextToken;
  if T is TEpithet then TEpithet(T).IsKeyword := true;
  if Operations.ContainsKey(T.Value) and ((T.Value <> ',') or (Self.Parent is TTerm)) then AResult := T;
  Result := Assigned(AResult);
  if AResult is TTerminal then TTerminal(AResult).OpType := otBinary;
end;

function TExpression.ParseBreak: boolean;
begin
  Result := true;
end;

function TExpression.GetKeywords: TStrings;
begin
  Result := Keywords;
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

{ TQualifiedIdent }

function TQualifiedIdent.InternalParse: boolean;
begin
  if Parent is TQualifiedIdent then _Dot := Terminal('.');
  _Name := Identifier;
  Result := Assigned(_Name) and (Assigned(_Dot) = (Parent is TQualifiedIdent));
  if Result then TQualifiedIdent.Parse(Self, Source, _Next);
end;

procedure TQualifiedIdent.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Dot, _Name, _Next]);
end;

function TQualifiedIdent.StatementName: string;
begin
  if Assigned(_Dot)
    then Result := _Dot.Value
    else Result := '';
  if Assigned(_Name) then
    Result := Result + _Name.Value;
  if Assigned(_Next) then
    Result := Result + _Next.StatementName;
end;

function TQualifiedIdent.IsSimpleIdent: boolean;
begin
  Result := not Assigned(_Next);
end;

{ TQualifiedIndexedIdent }

function TQualifiedIndexedIdent.TopStatement: boolean;
begin
  Result := not (Parent is TQualifiedIndexedIdent);
end;

function TQualifiedIndexedIdent.InternalParse: boolean;
begin
  if not TopStatement then TOptionalBracketedStatement<TArguments>.Parse(Self, Source, _Indexes);
  if Assigned(_Indexes) then _Dot := Terminal('.');
  if TopStatement or Assigned(_Dot) then TQualifiedIdent.Parse(Self, Source, _Ident);
  if Assigned(_Indexes) or Assigned(_Ident) then TQualifiedIndexedIdent.Parse(Self, Source, _Next);
  Result := Assigned(_Indexes) or Assigned(_Ident);
end;

procedure TQualifiedIndexedIdent.InternalPrintSelf(APrinter: TPrinter);
begin
  if TopStatement then APrinter.SupressNextLine(true);
  APrinter.PrintItems([_Indexes, _Dot, _Ident, _Next]);
  if TopStatement then APrinter.SupressNextLine(false);
end;

function TQualifiedIndexedIdent.IsSimpleIdent: boolean;
begin
  Result := Assigned(_Ident) and not Assigned(_Next) and (_Ident as TQualifiedIdent).IsSimpleIdent;
end;

{ TFunctionCall }

function TFunctionCall.InternalParse: boolean;
begin
  TQualifiedIdent.Parse(Self, Source, _Name);
  _OpenBracket := Terminal('(');
  if not Assigned(_Name) or not Assigned(_OpenBracket) then exit(false);
  TArguments.Parse(Self, Source, _Arguments);
  _CloseBracket := Terminal(')');
  Result := true;
end;

procedure TFunctionCall.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Name, _IndentNextLine, _OpenBracket, _IndentNextLine, _Arguments, _UndentNextLine, _CloseBracket, _Undent]);
end;

{ TArgument }

function TArgument.InternalParse: boolean;
var P: integer;
begin
  P := Source.Mark;
  _Ident := Identifier;
  _Assignment := Terminal('=>');
  if not Assigned(_Ident) or not Assigned(_Assignment) then
  begin
    _Ident := nil;
    _Assignment := nil;
    Source.Restore(P);
  end;
  Result := TParser.ParseExpression(Self, Source, _Expression);
end;

procedure TArgument.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Ident);
  if Assigned(_Ident) then
    APrinter.Ruler('argument', Settings.AlignVariables);
  APrinter.PrintItem(_Assignment);
  APrinter.PrintItem(_Expression);
end;

{ TArguments }

function TArguments.ParseBreak: boolean;
begin
  Result := Any([Terminal([';', ')'])]);
end;

function TArguments.OnePerLine: boolean;
begin
  Result := Self.Count > Settings.ArgumentSingleLineParamLimit;
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
  APrinter.PrintItems([_Case, _Expression, _Sections, _End]);
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
begin
  APrinter.PrintItems([_When, _Condition, _Then, _Else, _Value]);
end;

{ TCaseSections }

function TCaseSections.ParseBreak: boolean;
begin
  Result := not Assigned(Keyword(['when', 'else']));
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
  Keywords := TStringList.Create;
  Keywords.Sorted := true;
  Keywords.Duplicates := dupIgnore;
  Keywords.CaseSensitive := false;
  Keywords.Add('case');
  Keywords.Add('when');
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
  FreeAndNil(Keywords);
  FreeAndNil(Operations);

end.
