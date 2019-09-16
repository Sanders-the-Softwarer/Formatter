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

------------------------------------------------------------------------------ }

interface

uses Classes, System.Character, System.SysUtils, System.Generics.Collections, Tokens, Streams;

type

  { ����������� ���������� ���������� ����� �������� � ����� ������ }
  TTokenizer = class(TNextStream<TPositionedChar, TToken>)
  protected
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

var
  Keywords: TStringList;

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
    PrevPrevApo := false;
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
  function ParseTerminal: boolean;
  const
    N = 25;
    Tokens: array [1..N] of string = (':=', '||', '>=', '<=', '<>', '^=', '!=', '=>', '.', ',', ';', '(', ')', '+', '-', '*', '/', '%', '@', '=', '<', '>', '(+)', '%type', '%rowtype');
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
    if Keywords.IndexOf(TokenValue) >= 0
      then Result := TKeyword.Create(TokenValue, Start)
      else Result := TSimpleIdent.Create(TokenValue, Start)
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
//           �������������� ����� ��� �������� ���������� ��������            //
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

initialization
  Keywords := TStringList.Create;
  Keywords.Sorted := true;
  Keywords.Duplicates := dupIgnore;
  Keywords.CaseSensitive := false;
  Keywords.Add('create');
  Keywords.Add('or');
  Keywords.Add('replace');
  Keywords.Add('package');
  Keywords.Add('body');
  Keywords.Add('is');
  Keywords.Add('procedure');
  Keywords.Add('function');
  Keywords.Add('declare');
  Keywords.Add('begin');
  Keywords.Add('end');
  Keywords.Add('select');
  Keywords.Add('from');
  Keywords.Add('where');
  Keywords.Add('into');
  Keywords.Add('group');
  Keywords.Add('by');
  Keywords.Add('order');
  Keywords.Add('is');
  Keywords.Add('as');
//  Keywords.Add('null');
  Keywords.Add('true');
  Keywords.Add('false');
  Keywords.Add('return');
  Keywords.Add('within');
  Keywords.Add('cast');
  Keywords.Add('case');
  Keywords.Add('when');
  Keywords.Add('then');
  Keywords.Add('else');
  Keywords.Add('if');
  Keywords.Add('elsif');
  Keywords.Add('exception');
  Keywords.Add('keep');
  Keywords.Add('for');
  Keywords.Add('loop');
  Keywords.Add('in');
  Keywords.Add('out');
  Keywords.Add('nocopy');
  Keywords.Add('update');
  Keywords.Add('set');
  Keywords.Add('delete');
  Keywords.Add('default');

finalization
  FreeAndNil(Keywords);

end.
