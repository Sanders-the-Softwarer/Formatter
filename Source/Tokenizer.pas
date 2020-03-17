////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                           �����������  ����������                          //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
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

  { ����� ���������� "���������������", ����������� � ����� �� ����� �������� �������������� }
  TSkipSpecCommentProcessor = class(TNextStream<TToken, TToken>)
  strict private
    T: TToken;
    procedure Skip;
  strict protected
    function InternalEof: boolean; override;
    function InternalNext: TToken; override;
  end;

  { ����� ���������� ����������� �� ��������� ������ � ����������� �� � �������� �������� }
  TCommentProcessor = class(TNextStream<TToken, TToken>)
  strict private
    LeftPos: TDictionary<integer, integer>;
    T1, T2, T3: TToken;
    function SourceNext: TToken;
    procedure ReadNext;
    function AppliedAfter(C: TComment; T: TToken): boolean;
    function AppliedAbove(C: TComment; T: TToken): boolean;
    function AppliedFarAbove(C: TComment; T: TToken): boolean;
    function AppliedFarAboveStrong(C: TComment; T: TToken): boolean;
    function AppliedFarBelowStrong(C: TComment; T: TToken): boolean;
    function AppliedBelow(C: TComment; T: TToken): boolean;
    procedure Compress;
  strict protected
    function InternalEof: boolean; override;
    function InternalNext: TToken; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
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
  function ParseIdent(AFromCurrent: boolean = false): boolean;
  var
    C, F: char;
  begin
    if not AFromCurrent then Restore;
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

  { ���������� Q-�������� }
  function ParseQLiteral: boolean;
  var
    C, L: char;
  begin
    Restore;
    C := NextChar;
    if C in ['N', 'n'] then C := NextChar;
    if not (C in ['Q', 'q']) then exit(false);
    if NextChar <> '''' then exit(false);
    L := NextChar;
    case L of
      '{': L := '}';
      '[': L := ']';
      '(': L := ')';
      '<': L := '>';
    end;
    repeat
      C := NextChar;
    until C in [L, #0];
    NextChar;
    ApplyLastChar;
    Result := true;
  end;

  { ���������� �������� }
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

  { ���������� ����� }
  function ParseLabel: boolean;
  begin
    Restore;
    Result := (NextChar = '<') and
              (NextChar = '<') and
              ParseIdent(true) and
              (NextChar = '>') and
              (NextChar = '>');
    if Result then ApplyLastChar;
  end;

  { ���������� ��������������� ������ }
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
    Starts, Exact, Found, PrevExact: boolean;
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
        Value := Value.ToLower;
        Found := (Tokens[i] = Value);
        Exact := Exact or Found;
        Starts := Starts or not Found and Tokens[i].StartsWith(Value);
      end;
    until not Starts;
    Result := Exact or PrevExact;
    if Exact then
      ApplyLastChar
    else if PrevExact then
      RefuseLastChar;
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
  else if ParseLabel then
    Result := TLabel.Create(TokenValue, Start)
  else if ParseTerminal then
    Result := TTerminal.Create(TokenValue, Start)
  else if ParseQLiteral then
    Result := TLiteral.Create(TokenValue, Start)
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

procedure TCommentProcessor.AfterConstruction;
begin
  inherited;
  LeftPos := TDictionary<integer, integer>.Create;
end;

procedure TCommentProcessor.BeforeDestruction;
begin
  FreeAndNil(LeftPos);
  inherited;
end;

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

function TCommentProcessor.SourceNext: TToken;
begin
  Result := Source.Next;
  if not LeftPos.ContainsKey(Result.Line) then LeftPos.Add(Result.Line, Result.Col);
end;

procedure TCommentProcessor.ReadNext;
begin
  if Source.Eof then
    { �������� }
  else if not Assigned(T1) then
    T1 := SourceNext
  else if not Assigned(T2) then
    T2 := SourceNext
  else if not Assigned(T3) then
    T3 := SourceNext
  else
    { ���� ������ �� ����� }
end;

function TCommentProcessor.AppliedAfter(C: TComment; T: TToken): boolean;
begin
  Result := Assigned(C) and Assigned(T) and (C.Line = T.Line) and (C.Col > T.Col);
end;

function TCommentProcessor.AppliedAbove(C: TComment; T: TToken): boolean;
begin
  Result := Assigned(C) and Assigned(T) and (C.Col = T.Col) and (T.Line - C.Line = C.Height);
end;

function TCommentProcessor.AppliedFarAbove(C: TComment; T: TToken): boolean;
begin
  Result := Assigned(C) and Assigned(T) and (T.Line - C.Line > C.Height);
end;

function TCommentProcessor.AppliedFarAboveStrong(C: TComment; T: TToken): boolean;
begin
  Result := Assigned(C) and Assigned(T) and (T.Line - C.Line > C.Height) and (C.Col = T.Col);
end;

function TCommentProcessor.AppliedFarBelowStrong(C: TComment; T: TToken): boolean;
begin
  Result := Assigned(C) and Assigned(T) and (C.Line > T.Line + 1) and (LeftPos[T.Line] = C.Col);
end;

function TCommentProcessor.AppliedBelow(C: TComment; T: TToken): boolean;
begin
  Result := Assigned(C) and Assigned(T) and (C.Line = T.Line + 1);
end;

procedure TCommentProcessor.Compress;
var C: TComment;
begin
  { ���� �������� � �����������, ���� �������� ����� ����������� ��� � ��������� ������� }
  ReadNext; ReadNext;
  while (T1 is TComment) and Assigned(T2) do
  begin
    C := T1 as TComment;
    if AppliedAbove(C, T2)
      then T2.CommentAbove := C
      else T2.CommentFarAbove := C;
    T1 := T2;
    T2 := T3;
    T3 := nil;
    ReadNext;
  end;
  { ������, ����� � ������ (��������) �� �����������, ����������� �� �������� �������� ���� � ����������, ���� � ��������� }
  ReadNext;
  while T2 is TComment do
  begin
    C := T2 as TComment;
    if AppliedAfter(C, T1) then
      T1.CommentAfter := C
    else if AppliedAbove(C, T3) then
      T3.CommentAbove := C
    else if AppliedBelow(C, T1) then
      T1.CommentBelow := C
    else if AppliedFarAboveStrong(C, T3) then
      T3.CommentFarAbove := C
    else if AppliedFarBelowStrong(C, T1) then
      T1.CommentFarBelow := C
    else if AppliedFarAbove(C, T3) then
      T3.CommentFarAbove := C
    else
      T1.CommentFarBelow := C;
    T2 := T3;
    T3 := nil;
    ReadNext;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//        ����������� ��������� ������ � ��������� ���� full outer join       //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TMerger.InternalNext: TToken;

  var
    T1, T2, T3, T4: TToken;
    P2, P3, P4: TMark;

  function Check(var AResult: TToken; const S1, S2: string; const S3: string = ''; const S4: string = ''): boolean;
  var S: string;
  begin
    if not (T1 is TEpithet) or not SameStr(S1, T1.Value) then exit(false);
    if not (T2 is TEpithet) or not SameStr(S2, T2.Value) then exit(false);
    if (S3 <> '') and (not (T3 is TEpithet) or not Assigned(T3) or not SameStr(S3, T3.Value)) then exit(false);
    if (S4 <> '') and (not (T4 is TEpithet) or not Assigned(T4) or not SameStr(S4, T4.Value)) then exit(false);
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
    T1.Printed := true;
    T2.Printed := true;
    TEpithet(T1).IsKeyword := true;
    TEpithet(T2).IsKeyword := true;
    if S3 <> '' then begin T3.Printed := true; TEpithet(T3).IsKeyword := true; end;
    if S4 <> '' then begin T4.Printed := true; TEpithet(T4).IsKeyword := true; end;
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
  if Check(Result, 'authid', 'current_user') then exit;
  if Check(Result, 'authid', 'definer') then exit;
  if Check(Result, 'bulk', 'collect', 'into') then exit;
  if Check(Result, 'cross', 'apply') then exit;
  if Check(Result, 'full', 'join') then exit;
  if Check(Result, 'full', 'natural', 'join') then exit;
  if Check(Result, 'full', 'outer', 'join') then exit;
  if Check(Result, 'end', 'case') then exit;
  if Check(Result, 'end', 'if') then exit;
  if Check(Result, 'end', 'loop') then exit;
  if Check(Result, 'identified', 'by') then exit;
  if Check(Result, 'in', 'out') then exit;
  if Check(Result, 'inner', 'join') then exit;
  if Check(Result, 'instead', 'of') then exit;
  if Check(Result, 'is', 'not', 'null') then exit;
  if Check(Result, 'is', 'null') then exit;
  if Check(Result, 'left', 'join') then exit;
  if Check(Result, 'left', 'natural', 'join') then exit;
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
  if Check(Result, 'partition', 'by') then exit;
  if Check(Result, 'public', 'synonym') then exit;
  if Check(Result, 'right', 'join') then exit;
  if Check(Result, 'right', 'natural', 'join') then exit;
  if Check(Result, 'right', 'outer', 'join') then exit;
  if Check(Result, 'self', 'as', 'result') then exit;
  if Check(Result, 'subpartition', 'by') then exit;
  if Check(Result, 'type', 'body') then exit;
  if Check(Result, 'union', 'all') then exit;
  if Check(Result, 'when', 'matched', 'then') then exit;
  if Check(Result, 'when', 'not', 'matched', 'then') then exit;
  if Check(Result, 'with', 'admin', 'option') then exit;
  if Check(Result, 'with', 'grant', 'option') then exit;
  if Check(Result, 'with', 'hierarchy', 'option') then exit;
  if Check(Result, 'with', 'time', 'zone') then exit;
  { ��� �� ������� - ���������� ������ ������� }
  Source.Restore(P2);
  Result := Transit(T1);
end;

{ TSkipSpecCommentProcessor }

function TSkipSpecCommentProcessor.InternalEof: boolean;
begin
  Skip;
  Result := not Assigned(T);
end;

function TSkipSpecCommentProcessor.InternalNext: TToken;
begin
  Skip;
  Result := Transit(T);
  T := nil;
end;

procedure TSkipSpecCommentProcessor.Skip;
begin
  while not Assigned(T) and not Source.Eof do
  begin
    T := Source.Next;
    if (T is TComment) and T.Value.StartsWith('/*/') and T.Value.EndsWith('/*/') then
    begin
      T.Printed := true; { ����� �� �������� ����������� }
      T := nil;
    end;
  end;
end;

end.
