////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                           �����������  ����������                          //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Tokenizer;

{ ----- ���������� -------------------------------------------------------------

  �������� �������, ������, ������� �������������� �������� ������ � ��������-
  �����. ��� ����� ���������� � ����� ����� ����, ������ ���������������
  �������, �� ��� ���� ������ ���� ��������� � �������� � �������� �����,
  ������ ��������� �������������� ������������ �������������� �����������.
  ��� ������� ���� ������ ����� ����������� � �������������� �������� �����
  ����������� ��� ��������� ������������. ���� � ���, ��� ����������� ���������
  �� ������ � ������������� � �������� �������, ����� � ������� ���������.
  ����� ������� ����� ��������� ������ � ������������ �������������� ��������-
  ���, � ����� ���� ����� �� ������ ���������������� ������ - ������ � ��������
  ����� ������� � ����������� � ��� �����������.

  ��������������, ��� ����� ��������������� � ����������� ��������� �����������
  ������������ ������� - ������, �����, ����� ��� ������. ���� ��� ��� ��������
  ���������� ��� �����������, ����� ����������, ������������� � ����������
  �������� �������. ����� ���������� ������ ������ ���� ��� ����� �����
  ����������.

  ��� �������� ������ � ������������� ��������������� ������������� (end if,
  full outer join, when not matched then � �. �.) ������������ �����������
  �����, ������� ������� �������� ������ � �������� �� �������.

------------------------------------------------------------------------------ }

interface

uses Classes, System.Character, System.SysUtils, System.Generics.Collections,
  Tokens, Streams;

type

  { ����������� ���������� ���������� ����� �������� � ����� ������ }
  TTokenizer = class(TNextStream<TPositionedChar, TToken>)
  strict protected
    function InternalNext: TToken; override;
  end;

  { �������������� ����� ���������� ���������� ������� }
  TWhitespaceSkipper = class(TNextStream<TToken, TToken>)
  strict private
    procedure Skip;
  strict protected
    function InternalEof: boolean; override;
    function InternalNext: TToken; override;
  end;

  { �������������� ����� ��������� ���������� ���� 'end' 'if' � ����� 'end if' }
  TMerger = class(TNextStream<TToken, TToken>)
  strict protected
    function InternalNext: TToken; override;
  end;

  { ����� ���������� ����������� �� ��������� ������ � ����������� �� � �������� �������� }
  TCommentProcessor = class(TNextStream<TToken, TToken>)
  strict private
    PrevToken: TToken;
    procedure Process;
  strict protected
    function InternalEof: boolean; override;
    function InternalNext: TToken; override;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//       ����������� ���������� ���������� ����� �������� � ����� ������      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TTokenizer.InternalNext: TToken;

  var
    StartPosition, LastPosition: TMark;
    Start: TPositionedChar;
    TokenValue: string;
    LastChar: char;

  { �������������� ���������� ��������� ����������� }
  procedure Restore;
  begin
    Source.Restore(StartPosition);
    TokenValue := '';
    LastChar := #0;
  end;

  { ������ ���������� ������� ������� }
  function NextChar: char;
  begin
    if Source.Eof then exit(#0);
    LastPosition := Source.Mark;
    Result := Source.Next.Value;
    if LastChar <> #0 then TokenValue := TokenValue + LastChar;
    LastChar := Result;
  end;

  { ����� � �������������� ���������� ������� }
  procedure RefuseLastChar;
  begin
    if Source.Eof then exit;
    Source.Restore(LastPosition);
    LastChar := #0;
  end;

  { ������������� ���������� ���������� ������� }
  procedure ApplyLastChar;
  begin
    if Source.Eof then exit;
    TokenValue := TokenValue + LastChar;
    LastPosition := Source.Mark;
    LastChar := #0;
  end;

  { ���������� ���������� �������� }
  function ParseWhitespace: boolean;
  begin
    Restore;
    Result := false;
    while NextChar in [#9, #13, ' '] do Result := true;
    RefuseLastChar;
  end;

  { ���������� ����� ����� }
  function ParseFilename: boolean;
  begin
    Restore;
    Result := true;
    repeat until NextChar in [#13, #0];
    RefuseLastChar;
  end;

  { ���������� �������������� }
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

  { ���������� ��������� ����������� }
  function ParseLineComment: boolean;
  begin
    Restore;
    Result := (NextChar = '-') and (NextChar = '-');
    if not Result then exit;
    repeat until NextChar in [#13, #0];
    RefuseLastChar;
  end;

  { ���������� ���������� ����������� }
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
    { ���� ��������� ����� �����, ����� ���������������� ��� ��� ���������� ����������� ����������� - ��� ����� ��� ��� ����������� ������� }
    ApplyLastChar;
  end;

  { ���������� ����� }
  function ParseNumber: boolean;
  begin
    Restore;
    { ����� ������ ���������� � ����� }
    Result := TCharacter.IsDigit(NextChar);
    if not Result then exit;
    { ��������� ����� ����� ����� }
    repeat until not TCharacter.IsDigit(NextChar);
    { ���� �� ��� ������� �����, ����� ��������� �������� ������������ ����� �� ����������� for i in 1..10 }
    if LastChar = '.' then
    begin
      if not TCharacter.IsDigit(NextChar) then Restore;
      repeat until not TCharacter.IsDigit(NextChar);
    end;
    RefuseLastChar;
  end;

  { ���������� �������� }
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
    { ���� ��������� ����� �����, ����� ���������������� ��� ��� ����������
      ����������� ������� - ��� ����� ��� ��� ����������� ������� }
    RefuseLastChar;
  end;

  { ���������� ��������������� ������ }
  function ParseTerminal: boolean;
  const
    N = 31;
    Tokens: array [1..N] of string = (':=', '||', '>=', '<=', '<>', '^=', '!=',
                                      '=>', '..', '.', ',', ';', '(', ')', '+',
                                      '-', '*', '/', '%', '@@', '@', '=', '<',
                                      '>', '(+)', '%type', '%rowtype',
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
  { �������� ��������� ������� }
  StartPosition := Source.Mark;
  Start := Source.Next;
  { � ����������� �� ������� ������� }
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
//           �������������� ����� ��� �������� ���������� ��������            //
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
//                          ���������� ������������                           //
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
  { ��������� ������� }
  T1 := Source.Next;
  P2 := Source.Mark;
  if not Source.Eof then T2 := Source.Next else T2 := nil;
  P3 := Source.Mark;
  if not Source.Eof then T3 := Source.Next else T3 := nil;
  P4 := Source.Mark;
  if not Source.Eof then T4 := Source.Next else T4 := nil;
  { � ��������� �� �������������� }
  if Check(Result, 'bulk', 'collect', 'into') then exit;
  if Check(Result, 'cross', 'apply') then exit;
  if Check(Result, 'full', 'join') then exit;
  if Check(Result, 'full', 'outer', 'join') then exit;
  if Check(Result, 'end', 'case') then exit;
  if Check(Result, 'end', 'if') then exit;
  if Check(Result, 'end', 'loop') then exit;
  if Check(Result, 'inner', 'join') then exit;
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
  if Check(Result, 'not', 'in') then exit;
  if Check(Result, 'not', 'like') then exit;
  if Check(Result, 'outer', 'apply') then exit;
  if Check(Result, 'right', 'join') then exit;
  if Check(Result, 'right', 'outer', 'join') then exit;
  if Check(Result, 'union', 'all') then exit;
  if Check(Result, 'when', 'matched', 'then') then exit;
  if Check(Result, 'when', 'not', 'matched', 'then') then exit;
  { ��� �� ������� - ���������� ������ ������� }
  Source.Restore(P2);
  Result := Transit(T1);
end;

end.
