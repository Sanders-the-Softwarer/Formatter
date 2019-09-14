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

interface

uses System.Character, System.SysUtils, Tokens, Streams;

type
  { ����������� ���������� ���������� ����� �������� � ����� ������ }
  TTokenizer = class(TNextStream<TPositionedChar, TToken>)
  protected
    function InternalNext: TToken; override;
  end;

  { �������������� ����� ���������� ���������� ������� }
  TSkipWhitespaceTokenizer = class(TNextStream<TToken, TToken>)
  strict private
    procedure Skip;
  strict protected
    function InternalEof: boolean; override;
    function InternalNext: TToken; override;
  end;

implementation

{ TTokenizer }

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

  { ���������� �������������� }
  function ParseSimpleIdent: boolean;
  var C: char;
  begin
    Restore;
    Result := TCharacter.IsLetter(NextChar);
    if not Result then exit;
    repeat
      C := NextChar;
    until not TCharacter.IsLetter(C) and not TCharacter.IsDigit(C) and not (C in ['$', '#', '_']);
    RefuseLastChar;
  end;

  { ���������� �������������� � �������� }
  function ParseQuotedIdent: boolean;
  var C: char;
  begin
    Restore;
    Result := (NextChar = '"');
    if not Result then exit;
    repeat
      C := NextChar;
      if C = #0 then exit(false);
    until C = '"';
    ApplyLastChar;
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
    Result := TCharacter.IsDigit(NextChar);
    if not Result then exit;
    repeat until not TCharacter.IsDigit(NextChar);
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
    while NextChar <> #0 do
      if (LastChar <> '''') and PrevApo and not PrevPrevApo then
        break
      else
        begin
          PrevPrevApo := PrevApo;
          PrevApo := (LastChar = '''');
        end;
    { ���� ��������� ����� �����, ����� ���������������� ��� ��� ���������� ����������� ������� - ��� ����� ��� ��� ����������� ������� }
    RefuseLastChar;
  end;

  { ���������� ��������������� ������ }
  function ParseMultiChar: boolean;
  const
    N = 23;
    Tokens: array [1..N] of string = (':=', '||', '>=', '<=', '<>', '^=', '!=', '=>', '.', ',', ';', '(', ')', '+', '-', '*', '/', '%', '@', '=', '<', '>', '(+)');
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
  { �������� ��������� ������� }
  StartPosition := Source.Mark;
  Start := Source.Next;
  { � ����������� �� ������� ������� }
  if ParseWhitespace then
    Result := TWhitespace.Create(TokenValue, Start)
  else if ParseSimpleIdent then
    Result := TSimpleIdent.Create(TokenValue, Start)
  else if ParseQuotedIdent then
    Result := TQuotedIdent.Create(TokenValue, Start)
  else if ParseLineComment then
    Result := TComment.Create(TokenValue, Start)
  else if ParseBracedComment then
    Result := TComment.Create(TokenValue, Start)
  else if ParseNumber then
    Result := TNumber.Create(TokenValue, Start)
  else if ParseLiteral then
    Result := TLiteral.Create(TokenValue, Start)
  else if ParseMultiChar then
    Result := TCharToken.Create(TokenValue, Start)
  else
    begin
      Restore;
      Result := TUnknownToken.Create(Source.Next);
    end;
end;

{ TSkipWhitespaceTokenizer }

function TSkipWhitespaceTokenizer.InternalEof: boolean;
begin
  Skip;
  Result := inherited InternalEof;
end;

function TSkipWhitespaceTokenizer.InternalNext: TToken;
begin
  Skip;
  Result := Transit(Source.Next);
end;

procedure TSkipWhitespaceTokenizer.Skip;
begin
  repeat
    Source.SaveMark
  until Source.Eof or not (Source.Next is TWhitespace);
  if not Source.Eof then Source.Restore;
end;

end.
