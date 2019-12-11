////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                           Лексический  анализатор                          //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Tokenizer;

{ ----- Примечания -------------------------------------------------------------

  Наиболее сложной, видимо, задачей форматирования является работа с коммента-
  риями. Они могут находиться в любом месте кода, мешают синтаксическому
  анализу, но при этом должны быть сохранены и выведены в итоговый текст,
  причём правильно расположенными относительно синтаксических конструкций.
  Для решения этой задачи между лексическим и синтаксическим анализом введён
  специальный шаг обработки комментариев. Идея в том, что комментарий убирается
  из потока и привязывается к значимой лексеме, рядом с которой находится.
  Далее лексема войдёт составной частью в распознанную синтаксическую конструк-
  цию, и когда дело дойдёт до вывода форматированного текста - вместе с лексемой
  будет выведен и привязанный к ней комментарий.

  Предполагается, что будет анализироваться и сохраняться положение комментария
  относительно лексемы - сверху, снизу, левее или правее. Пока что для простоты
  реализации все комментарии, кроме стартового, привязываются к предыдущей
  значимой лексеме. После реализации других частей кода это место будет
  доработано.

  Для удобства работы с многословными синтаксическими конструкциями (end if,
  full outer join, when not matched then и т. п.) предусмотрен специальный
  поток, который находит подобные идиомы и собирает их воедино.

------------------------------------------------------------------------------ }

interface

uses Classes, System.Character, System.SysUtils, System.Generics.Collections,
  Tokens, Streams;

type

  { Лексический анализатор превращает поток символов в поток лексем }
  TTokenizer = class(TNextStream<TPositionedChar, TToken>)
  strict protected
    function InternalNext: TToken; override;
  end;

  { Дополнительный класс пропускает незначащие пробелы }
  TWhitespaceSkipper = class(TNextStream<TToken, TToken>)
  strict private
    procedure Skip;
  strict protected
    function InternalEof: boolean; override;
    function InternalNext: TToken; override;
  end;

  { Дополнительный класс склеивает комбинации типа 'end' 'if' в общий 'end if' }
  TMerger = class(TNextStream<TToken, TToken>)
  strict protected
    function InternalNext: TToken; override;
  end;

  { Класс выкусывает комментарии из основного потока и привязывает их к значимым лексемам }
  TCommentProcessor = class(TNextStream<TToken, TToken>)
  strict private
    T1, T2, T3: TToken;
    Started: boolean;
    procedure ReadNext;
    function Applied(C: TComment; T: TToken; Strong: boolean): boolean;
    procedure Compress;
  strict protected
    function InternalEof: boolean; override;
    function InternalNext: TToken; override;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//       Лексический анализатор превращает поток символов в поток лексем      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TTokenizer.InternalNext: TToken;

  var
    StartPosition, LastPosition: TMark;
    Start: TPositionedChar;
    TokenValue: string;
    LastChar: char;

  { Восстановление начального состояния анализатора }
  procedure Restore;
  begin
    Source.Restore(StartPosition);
    TokenValue := '';
    LastChar := #0;
  end;

  { Чтение следующего символа лексемы }
  function NextChar: char;
  begin
    if Source.Eof then exit(#0);
    LastPosition := Source.Mark;
    Result := Source.Next.Value;
    if LastChar <> #0 then TokenValue := TokenValue + LastChar;
    LastChar := Result;
  end;

  { Откат к предпоследнему считанному символу }
  procedure RefuseLastChar;
  begin
    if Source.Eof then exit;
    Source.Restore(LastPosition);
    LastChar := #0;
  end;

  { Подтверждение последнего считанного символа }
  procedure ApplyLastChar;
  begin
    if Source.Eof then exit;
    TokenValue := TokenValue + LastChar;
    LastPosition := Source.Mark;
    LastChar := #0;
  end;

  { Считывание незначащих пробелов }
  function ParseWhitespace: boolean;
  begin
    Restore;
    Result := false;
    while NextChar in [#9, #13, ' '] do Result := true;
    RefuseLastChar;
  end;

  { Считывание имени файла }
  function ParseFilename: boolean;
  begin
    Restore;
    Result := true;
    repeat until NextChar in [#13, #0];
    RefuseLastChar;
  end;

  { Считывание идентификатора }
  function ParseIdent: boolean;
  var
    C, F: char;
  begin
    Restore;
    F := NextChar;
    if F = '"' then
      repeat
        C := NextChar
      until C in ['"', #0]
    else if TCharacter.IsLetter(F) then
      repeat
        C := NextChar;
      until not TCharacter.IsLetter(C) and not TCharacter.IsDigit(C) and not (C in ['$', '#', '_'])
    else
      begin
        RefuseLastChar;
        exit(false);
      end;
    if F = '"' then ApplyLastChar else RefuseLastChar;
    Result := true;
  end;

  { Считывание строчного комментария }
  function ParseLineComment: boolean;
  begin
    Restore;
    Result := (NextChar = '-') and (NextChar = '-');
    if not Result then exit;
    repeat until NextChar in [#13, #0];
    RefuseLastChar;
  end;

  { Считывание скобочного комментария }
  function ParseBracedComment: boolean;
  var PrevStar: boolean;
  begin
    Restore;
    Result := (NextChar = '/') and (NextChar = '*');
    if not Result then exit;
    PrevStar := false;
    while NextChar <> #0 do
      if PrevStar and (LastChar = '/')
        then break
        else PrevStar := (LastChar = '*');
    { Если достигнут конец файла, будем интерпретировать это как неожиданно кончившийся комментарий - так лучше чем как неизвестную лексему }
    ApplyLastChar;
  end;

  { Считывание числа }
  function ParseNumber: boolean;
  begin
    Restore;
    { Число должно начинаться с цифры }
    Result := TCharacter.IsDigit(NextChar);
    if not Result then exit;
    { Прочитаем целую часть числа }
    repeat until not TCharacter.IsDigit(NextChar);
    { Если за ней следует точка, нужно тщательно отделить вещественное число от конструкции for i in 1..10 }
    if LastChar = '.' then
    begin
      if not TCharacter.IsDigit(NextChar) then Restore;
      repeat until not TCharacter.IsDigit(NextChar);
    end;
    RefuseLastChar;
  end;

  { Считывание литерала }
  function ParseLiteral: boolean;
  var
    Odd: boolean;
    C: char;
  begin
    Restore;
    Result := (NextChar = '''');
    if not Result then exit;
    Odd := false;
    repeat
      C := NextChar;
      if C = #0 then
        break
      else if C = '''' then
        Odd := not Odd
      else if Odd then
        break;
    until false;
    if C <> #0 then RefuseLastChar else ApplyLastChar;
  end;

  { Считывание многосимвольных лексем }
  function ParseTerminal: boolean;
  const
    N = 33;
    Tokens: array [1..N] of string = (':=', '||', '>=', '<=', '<>', '^=', '!=',
                                      '=>', '..', '.', ',', ';', '(', ')', '+',
                                      '-', '*', '/', '%', '@@', '@', '=', '<',
                                      '>', '(+)', ':', '&', '%type', '%rowtype',
                                      '%rowcount', '%found', '%notfound',
                                      '%isopen');
  var
    Value: string;
    Starts, Exact, PrevExact: boolean;    
    i: integer;
  begin
    Restore;
    Exact := false;
    repeat
      Value := Value + NextChar;
      PrevExact := Exact;
      Exact := false;
      Starts := false;
      for i := 1 to N do
      begin
        Exact := Exact or (Tokens[i] = Value);
        Starts := Starts or ((Tokens[i] <> Value) and (Tokens[i].StartsWith(Value)));
      end;
    until not Starts;
    Result := Exact or PrevExact;
    if Exact then
      ApplyLastChar
    else if PrevExact then
      RefuseLastChar
{    else
      raise Exception.Create('Tokenizer: internal error #1');}
  end;

begin
  { Сохраним начальную позицию }
  StartPosition := Source.Mark;
  Start := Source.Next;
  { В зависимости от первого символа }
  if ParseWhitespace then
    Result := TWhitespace.Create(TokenValue, Start)
  else if ParseLineComment then
    Result := TComment.Create(TokenValue, Start)
  else if ParseBracedComment then
    Result := TComment.Create(TokenValue, Start)
  else if ParseTerminal then
    Result := TTerminal.Create(TokenValue, Start)
  else if ParseIdent then
    Result := TEpithet.Create(TokenValue, Start)
  else if ParseNumber then
    Result := TNumber.Create(TokenValue, Start)
  else if ParseLiteral then
    Result := TLiteral.Create(TokenValue, Start)
  else
    begin
      Restore;
      Result := TUnknownToken.Create(Source.Next);
    end;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//           Дополнительный класс для пропуска незначимых пробелов            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TWhitespaceSkipper.InternalEof: boolean;
begin
  Skip;
  Result := Source.Eof;
end;

function TWhitespaceSkipper.InternalNext: TToken;
begin
  Skip;
  Result := Transit(Source.Next);
end;

procedure TWhitespaceSkipper.Skip;
begin
  repeat
    Source.SaveMark;
    if Source.Eof then
      exit
    else if Source.Next is TWhitespace then
      continue
    else
      Source.Restore;
  until true;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                          Обработчик комментариев                           //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TCommentProcessor.InternalEof: boolean;
begin
  Compress;
  Result := not Assigned(T1);
end;

function TCommentProcessor.InternalNext: TToken;
begin
  Compress;
  Result := Transit(T1);
  T1 := T2;
  T2 := T3;
  T3 := nil;
end;

procedure TCommentProcessor.ReadNext;
begin
  if Source.Eof then
    { неоткуда }
  else if not Assigned(T1) then
    T1 := Source.Next
  else if not Assigned(T2) then
    T2 := Source.Next
  else if not Assigned(T3) then
    T3 := Source.Next
  else
    { пока больше не нужно }
end;

function TCommentProcessor.Applied(C: TComment; T: TToken; Strong: boolean): boolean;
begin
  Result := Assigned(C) and Assigned(T) and (not Strong or (C.Col = T.Col)) and (Abs(C.Line - T.Line) <= 1);
end;

procedure TCommentProcessor.Compress;
begin
  { Если начинаем с комментария, пока возможно будем привязывать его к следующей лексеме }
  ReadNext; ReadNext;
  while (T1 is TComment) and Applied(T1 as TComment, T2, true) do
  begin
    T2.AddCommentAbove(T1 as TComment);
    T1 := T2;
    T2 := T3;
    T3 := nil;
    ReadNext;
  end;
  { Теперь, когда в начале (возможно) не комментарий, комментарий из середины привяжем либо к предыдущей, либо к следующей }
  ReadNext;
  while T2 is TComment do
  begin
    if Applied(T2 as TComment, T3, true)
      then T3.AddCommentAbove(T2 as TComment)
      else T1.AddCommentBelow(T2 as TComment);
    T2 := T3;
    T3 := nil;
    ReadNext;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//        Объединение отдельных лексем в составные типа full outer join       //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TMerger.InternalNext: TToken;

  var
    T1, T2, T3, T4: TToken;
    P2, P3, P4: TMark;

  function Check(var AResult: TToken; const S1, S2: string; const S3: string = ''; const S4: string = ''): boolean;
  var S: string;
  begin
    if not (T1 is TEpithet) or not SameText(S1, T1.Value) then exit(false);
    if not (T2 is TEpithet) or not SameText(S2, T2.Value) then exit(false);
    if (S3 <> '') and not (T3 is TEpithet) and not SameText(S3, T3.Value) then exit(false);
    if (S4 <> '') and not (T4 is TEpithet) and not SameText(S4, T4.Value) then exit(false);
    S := S1 + ' ' + S2;
    if S3 <> ''
      then S := S + ' ' + S3
      else Source.Restore(P3);
    if S4 <> ''
      then S := S + ' ' + S4
      else
        if S3 <> '' then Source.Restore(P4);
    AResult := TEpithet.Create(S, T1.Line, T1.Col);
    Result := true;
  end;

begin
  { Прочитаем лексемы }
  T1 := Source.Next;
  P2 := Source.Mark;
  if not Source.Eof then T2 := Source.Next else T2 := nil;
  P3 := Source.Mark;
  if not Source.Eof then T3 := Source.Next else T3 := nil;
  P4 := Source.Mark;
  if not Source.Eof then T4 := Source.Next else T4 := nil;
  { И попробуем их скомбинировать }
  if Check(Result, 'bulk', 'collect', 'into') then exit;
  if Check(Result, 'cross', 'apply') then exit;
  if Check(Result, 'full', 'join') then exit;
  if Check(Result, 'full', 'outer', 'join') then exit;
  if Check(Result, 'end', 'case') then exit;
  if Check(Result, 'end', 'if') then exit;
  if Check(Result, 'end', 'loop') then exit;
  if Check(Result, 'inner', 'join') then exit;
  if Check(Result, 'instead', 'of') then exit;
  if Check(Result, 'is', 'not', 'null') then exit;
  if Check(Result, 'is', 'null') then exit;
  if Check(Result, 'left', 'join') then exit;
  if Check(Result, 'left', 'outer', 'join') then exit;
  if Check(Result, 'multiset', 'except') then exit;
  if Check(Result, 'multiset', 'except', 'all') then exit;
  if Check(Result, 'multiset', 'except', 'distinct') then exit;
  if Check(Result, 'multiset', 'intersect') then exit;
  if Check(Result, 'multiset', 'intersect', 'all') then exit;
  if Check(Result, 'multiset', 'intersect', 'distinct') then exit;
  if Check(Result, 'multiset', 'union') then exit;
  if Check(Result, 'multiset', 'union', 'all') then exit;
  if Check(Result, 'multiset', 'union', 'distinct') then exit;
  if Check(Result, 'not', 'between') then exit;
  if Check(Result, 'not', 'in') then exit;
  if Check(Result, 'not', 'like') then exit;
  if Check(Result, 'outer', 'apply') then exit;
  if Check(Result, 'right', 'join') then exit;
  if Check(Result, 'right', 'outer', 'join') then exit;
  if Check(Result, 'self', 'as', 'result') then exit;
  if Check(Result, 'type', 'body') then exit;
  if Check(Result, 'union', 'all') then exit;
  if Check(Result, 'when', 'matched', 'then') then exit;
  if Check(Result, 'when', 'not', 'matched', 'then') then exit;
  { Раз не удалось - возвращаем первую лексему }
  Source.Restore(P2);
  Result := Transit(T1);
end;

end.
