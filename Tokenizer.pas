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

  Для ключевого слова end предусмотрена особая обработка - делается попытка
  соединить его со следующим ключевым словом в одну комбинацию (end if, end
  loop и т. п.) Это позволяет проще и надёжнее описывать грамматику, в том
  числе в случае ошибок парсинга анализ программного блока не завершается на
  первом попавшемся end, что позволяет корректнее продолжить обработку.

------------------------------------------------------------------------------ }

interface

uses Classes, System.Character, System.SysUtils, System.Generics.Collections,
  Tokens, Streams;

type

  { Лексический анализатор превращает поток символов в поток лексем }
  TTokenizer = class(TNextStream<TPositionedChar, TToken>)
  protected
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
    PrevToken: TToken;
    procedure Process;
  strict protected
    function InternalEof: boolean; override;
    function InternalNext: TToken; override;
  end;

implementation

var
  Keywords: TStringList;

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

  { Считывание идентификатора }
  function ParseIdent: boolean;
  var
    C: char;
  begin
    Restore;
    repeat
      C := NextChar;
      if C = '"' then
        repeat
          C := NextChar
        until C in ['"', #0]
      else if TCharacter.IsLetter(C) then
        repeat
          C := NextChar;
        until not TCharacter.IsLetter(C) and not TCharacter.IsDigit(C) and not (C in ['$', '#', '_'])
      else
        begin
          RefuseLastChar;
          exit(false);
        end;
    until C <> '.';
    RefuseLastChar;
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
    Result := TCharacter.IsDigit(NextChar);
    if not Result then exit;
    repeat until not TCharacter.IsDigit(NextChar);
    if LastChar = '.' then
      repeat until not TCharacter.IsDigit(NextChar);
    RefuseLastChar;
  end;

  { Считывание литерала }
  function ParseLiteral: boolean;
  var PrevApo, PrevPrevApo: boolean;
  begin
    Restore;
    Result := (NextChar = '''');
    if not Result then exit;
    PrevApo := false;
    PrevPrevApo := false;
    while NextChar <> #0 do
      if (LastChar <> '''') and PrevApo and not PrevPrevApo then
        break
      else
        begin
          PrevPrevApo := PrevApo;
          PrevApo := (LastChar = '''');
        end;
    { Если достигнут конец файла, будем интерпретировать это как неожиданно кончившийся литерал - так лучше чем как неизвестную лексему }
    RefuseLastChar;
  end;

  { Считывание многосимвольных лексем }
  function ParseTerminal: boolean;
  const
    N = 26;
    Tokens: array [1..N] of string = (':=', '||', '>=', '<=', '<>', '^=', '!=', '=>', '.', ',', ';', '(', ')', '+', '-', '*', '/', '%', '@', '=', '<', '>', '(+)', '%type', '%rowtype', '%rowcount');
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
      RefuseLastChar;
  end;
  
begin
  { Сохраним начальную позицию }
  StartPosition := Source.Mark;
  Start := Source.Next;
  { В зависимости от первого символа }
  if ParseWhitespace then
    Result := TWhitespace.Create(TokenValue, Start)
  else if ParseIdent then
    if Keywords.IndexOf(TokenValue) >= 0
      then Result := TKeyword.Create(TokenValue, Start)
      else Result := TIdent.Create(TokenValue, Start)
  else if ParseLineComment then
    Result := TComment.Create(TokenValue, Start)
  else if ParseBracedComment then
    Result := TComment.Create(TokenValue, Start)
  else if ParseNumber then
    Result := TNumber.Create(TokenValue, Start)
  else if ParseLiteral then
    Result := TLiteral.Create(TokenValue, Start)
  else if ParseTerminal then
    Result := TTerminal.Create(TokenValue, Start)
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
  Result := inherited InternalEof;
end;

function TWhitespaceSkipper.InternalNext: TToken;
begin
  Skip;
  Result := Transit(Source.Next);
end;

procedure TWhitespaceSkipper.Skip;
begin
  repeat
    Source.SaveMark
  until Source.Eof or not (Source.Next is TWhitespace);
  if not Source.Eof then Source.Restore;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                          Обработчик комментариев                           //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TCommentProcessor.InternalEof: boolean;
begin
  Process;
  Result := Source.Eof;
end;

function TCommentProcessor.InternalNext: TToken;
begin
  Process;
  Result := Transit(Source.Next);
  PrevToken := Result;
end;

procedure TCommentProcessor.Process;
var
  Token: TToken;
  Prev: TList<TComment>;
  i: integer;
begin
  Prev := nil;
  while not Source.Eof do
  begin
    Source.SaveMark;
    Token := Source.Next;
    if Token is TComment then
      if Assigned(PrevToken) then
        PrevToken.AddCommentBelow(Token as TComment)
      else
        begin
          if not Assigned(Prev) then Prev := TList<TComment>.Create;
          Prev.Add(Token as TComment);
        end
    else
      begin
        if Assigned(Prev) then
          for i := 0 to Prev.Count - 1 do
            Token.AddCommentAbove(Prev[i]);
        Source.Restore;
        exit;
      end;
  end;
end;

{ TEndMerger }

function TMerger.InternalNext: TToken;

  var
    T1, T2, T3, T4: TToken;
    P2, P3, P4: TMark;

  function Check(var AResult: TToken; const S1, S2: string; const S3: string = ''; const S4: string = ''): boolean;
  var S: string;
  begin
    if not (T1 is TKeyword) or not SameText(S1, T1.Value) then exit(false);
    if not (T2 is TKeyword) or not SameText(S2, T2.Value) then exit(false);
    if (S3 <> '') and not (T3 is TKeyword) and not SameText(S3, T3.Value) then exit(false);
    if (S4 <> '') and not (T4 is TKeyword) and not SameText(S4, T4.Value) then exit(false);
    S := S1 + ' ' + S2;
    if S3 <> ''
      then S := S + ' ' + S3
      else Source.Restore(P3);
    if S4 <> ''
      then S := S + ' ' + S4
      else
        if S3 <> '' then Source.Restore(P4);
    AResult := TKeyword.Create(S, T1.Line, T1.Col);
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
  if Check(Result, 'end', 'if') then exit;
  if Check(Result, 'end', 'loop') then exit;
  if Check(Result, 'is', 'null') then exit;
  if Check(Result, 'is', 'not', 'null') then exit;
  if Check(Result, 'not', 'like') then exit;
  if Check(Result, 'when', 'matched', 'then') then exit;
  if Check(Result, 'when', 'not', 'matched', 'then') then exit;
  { Раз не удалось - возвращаем первую лексему }
  Source.Restore(P2);
  Result := Transit(T1);
end;

initialization
  Keywords := TStringList.Create;
  Keywords.Sorted := true;
  Keywords.Duplicates := dupIgnore;
  Keywords.CaseSensitive := false;

  Keywords.Add('and');
  Keywords.Add('as');
  Keywords.Add('begin');
  Keywords.Add('body');
  Keywords.Add('byte');
  Keywords.Add('case');
  Keywords.Add('char');
  Keywords.Add('constant');
  Keywords.Add('create');
  Keywords.Add('delete');
  Keywords.Add('else');
  Keywords.Add('end');
  Keywords.Add('exception');
  Keywords.Add('false');
  Keywords.Add('from');
  Keywords.Add('full');
  Keywords.Add('function');
  Keywords.Add('if');
  Keywords.Add('in');
  Keywords.Add('into');
  Keywords.Add('insert');
  Keywords.Add('is');
  Keywords.Add('join');
  Keywords.Add('like');
  Keywords.Add('loop');
  Keywords.Add('matched');
  Keywords.Add('merge');
  Keywords.Add('nocopy');
  Keywords.Add('not');
  Keywords.Add('null');
  Keywords.Add('on');
  Keywords.Add('or');
  Keywords.Add('out');
  Keywords.Add('package');
  Keywords.Add('procedure');
  Keywords.Add('raise');
  Keywords.Add('replace');
  Keywords.Add('return');
  Keywords.Add('returning');
  Keywords.Add('select');
  Keywords.Add('set');
  Keywords.Add('table');
  Keywords.Add('then');
  Keywords.Add('true');
  Keywords.Add('update');
  Keywords.Add('using');
  Keywords.Add('values');
  Keywords.Add('when');
  Keywords.Add('where');

finalization
  FreeAndNil(Keywords);

end.
