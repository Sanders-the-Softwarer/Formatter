////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//           ������� ������ ��������� ��� ��������������� �����������         //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Statements;

{ ----- ���������� -------------------------------------------------------------

  ����� TStatement �������� ������� ��� ����������� ��������������� �����������.
  � ���������������� �������� ������ �������� ������ � �� � ��� �����������,
  � ������� InternalParse, � �� ����� ��� ��� �������������� ���������� ������
  ��������� ������� ������� ��������� ����������� �������� ������. �����
  ������������ ������������ ������ ��������������� �������, �������� ��������
  ������, ��������� �������.

  ���������� TStatement ����������� ������ ���������� � ������ ������������
  ��� �������� ����� ������������ ������ TStatement ������ ������-���� �����
  ������������������� ����. ��� �� ������ ��������� ��������� �������, ��������
  ��������� �� ���������� � � ������������ �������, �� � ��� �����������
  ��������� ������������ �������� ������������� �������� ����������.

  ����� TStatementList ������������ ��� �������� ������������������ ����������
  ����������� - ��������, ���������� ����������� ��� ���������� � �����
  begin .. end. ��� ������������� ����� ��� ��������� � ������������ �������,
  ��������� �� ����� ��������� ������������ �������� "� ����� ���� �����������
  ������?", "� ���, ���� �������� ������� �����?", "� ���, ���� �����������
  ����������� �����������?"

  ����� TUnexpectedToken ������������ ��� ������ �� ��������, ����� �� �������
  ������ ����������� ���-��, ���� �� ������ ����� � ���� ����� ���� ����������
  �� ������. �� ��������� �� �������� ������ ���� ������� ���, ������ ���
  ������ ����� � ������ ���������� � ��������� ������� ���������� ������,
  ����� ��������� ���������� ����������� � �������� �����. ��������� �����
  ������, ������� ����� ������� �� ��������� ����� ������������ � ���������
  ��������.

------------------------------------------------------------------------------ }

interface

uses SysUtils, System.Generics.Collections, Streams, Tokens, Printers_;

type

  { ������� ����� �������������� ����������� }
  TStatement = class
  strict private
    FParent: TStatement;
    FSettings: TParserSettings;
    function GetSettings: TParserSettings;
  strict protected
    Source: TBufferedStream<TToken>;
    function InternalParse: boolean; virtual; abstract;
    function NextToken: TToken;
    function Keyword(const AKeyword: string): TKeyword; overload;
    function Keyword(const AKeywords: array of string): TKeyword; overload;
    function Identifier: TIdent;
    function Number: TNumber;
    function Literal: TLiteral;
    function Terminal(const ATerminal: string): TTerminal; overload;
    function Terminal(const ATerminals: array of string): TTerminal; overload;
  public
    class function Parse(AParent: TStatement; Tokens: TBufferedStream<TToken>; out AResult: TStatement): boolean;
  public
    constructor Create(AParent: TStatement; ASource: TBufferedStream<TToken>);
    function Name: string; virtual;
    procedure PrintSelf(APrinter: TPrinter); virtual; abstract;
    property Parent: TStatement read FParent;
    property Settings: TParserSettings read GetSettings write FSettings;
  end;

  { ������� ����� ��� ������� ���������� ����������� (����������, ��������� � �. �.) }
  TStatementList = class(TStatement)
  strict private
    FStatements: TList<TStatement>;
    FDelimiters: TList<TToken>;
  strict protected
    function InternalParse: boolean; override;
    function ParseStatement(out AResult: TStatement): boolean; virtual; abstract;
    function ParseDelimiter(out AResult: TToken): boolean; virtual;
    function ParseBreak: boolean; virtual; abstract;
    function MultiLine: boolean; virtual;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure PrintSelf(APrinter: TPrinter); override;
    function Count: integer;
    function Item(Index: integer): TStatement;
    function Any(Found: array of TObject): boolean;
  end;

  { ������� ����� ��� ������ ���������� �����������, ���������� �������� }
  TCommaList = class(TStatementList)
  strict
  private
    function ParseDelimiter(out AResult: TToken): boolean; override;
  end;

  { ����������� ������� - ����� ��� �����������, ������� �� ������� ��������� }
  TUnexpectedToken = class(TStatement)
  strict private
    _Token: TToken;
  strict protected
    function InternalParse: boolean; override;
  public
    function Name: string; override;
    procedure PrintSelf(APrinter: TPrinter); override;
    property Token: TToken read _Token;
  end;

implementation

{ TStatement }

constructor TStatement.Create(AParent: TStatement; ASource: TBufferedStream<TToken>);
begin
  FParent := AParent;
  Source := ASource;
end;

class function TStatement.Parse(AParent: TStatement; Tokens: TBufferedStream<TToken>; out AResult: TStatement): boolean;
var SavedPosition: TMark;
begin
  AResult := Self.Create(AParent, Tokens);
  SavedPosition := Tokens.Mark;
  Result := AResult.InternalParse;
  if not Result then
  begin
    Tokens.Restore(SavedPosition);
    FreeAndNil(AResult);
  end;
end;

function TStatement.Name: string;
begin
  raise Exception.CreateFmt('Method %s.Name should be declared', [ClassName]);
end;

function TStatement.NextToken: TToken;
begin
  if Source.Eof
    then Result := UnexpectedEOF
    else Result := Source.Next;
end;

function TStatement.Keyword(const AKeyword: string): TKeyword;
begin
  Result := Keyword([AKeyword]);
end;

function TStatement.Keyword(const AKeywords: array of string): TKeyword;
var
  Token: TToken;
  P: TMark;
  i: integer;
begin
  Result := nil;
  P := Source.Mark;
  Token := NextToken;
  if Token is TKeyword then
    for i := Low(AKeywords) to High(AKeywords) do
      if SameText(Token.Value, AKeywords[i]) then
        Result := Token as TKeyword;
  if not Assigned(Result) then
    Source.Restore(P);
end;

function TStatement.Identifier: TIdent;
var
  Token: TToken;
  P: TMark;
begin
  Result := nil;
  P := Source.Mark;
  Token := NextToken;
  if Token is TIdent
    then Result := Token as TIdent
    else Source.Restore(P);
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

function TStatement.GetSettings: TParserSettings;
begin
  if Assigned(Parent)
    then Result := Parent.Settings
    else Result := FSettings;
end;

{ TStatementList }

procedure TStatementList.AfterConstruction;
begin
  inherited;
  FStatements := TList<TStatement>.Create;
  FDelimiters := TList<TToken>.Create;
end;

procedure TStatementList.BeforeDestruction;
begin
  inherited;
  FreeAndNil(FStatements);
  FreeAndNil(FDelimiters);
end;

function TStatementList.InternalParse: boolean;
var
  P: TMark;
  S: TStatement;
  T: TToken;
  B: boolean;
begin
  repeat
    P := Source.Mark;
    { ���� ������� ��������� ����������� - �������� ������ }
    if ParseStatement(S) then
    begin
      P := Source.Mark;
      FStatements.Add(S);
      if ParseDelimiter(T) then
        begin
          FDelimiters.Add(T);
          continue;
        end
      else
        FDelimiters.Add(nil);
    end;
    { ���� ��������� ����������� ����������� - ������� }
    Source.Restore(P);
    B := ParseBreak;
    Source.Restore(P);
    if B then break;
    { ���� �� ��, �� ������ - ��������� ����������� ����������� � ������ ��� ����������� ����������� }
    if TUnexpectedToken.Parse(Self, Source, S)
      then begin FStatements.Add(S); FDelimiters.Add(nil); end
      else break;
  until false;
  Result := (Count > 0);
end;

procedure TStatementList.PrintSelf(APrinter: TPrinter);
var i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    APrinter.PrintItem(FStatements[i]);
    APrinter.PrintItem(FDelimiters[i]);
    if MultiLine then APrinter.NextLine else APrinter.Space;
  end;
end;

function TStatementList.ParseDelimiter(out AResult: TToken): boolean;
begin
  Result := true;
  AResult := nil;
end;

function TStatementList.MultiLine: boolean;
begin
  Result := true;
end;

function TStatementList.Count: integer;
begin
  Result := FStatements.Count;
end;

function TStatementList.Item(Index: integer): TStatement;
begin
  Result := FStatements[Index];
end;

function TStatementList.Any(Found: array of TObject): boolean;
var i: integer;
begin
  Result := false;
  for i := Low(Found) to High(Found) do Result := Result or Assigned(Found[i]);
end;

{ TCommaList }

function TCommaList.ParseDelimiter(out AResult: TToken): boolean;
begin
  AResult := Terminal(',');
  Result  := Assigned(AResult);
end;

{ TUnexpectedToken }

function TUnexpectedToken.Name: string;
begin
  try
    Result := Format('*** ����������� ����������� *** [%s ''%s'']', [Token.TokenType, Token.Value]);
  except
    Result := '*** ����������� ����������� ***';
  end;
end;

function TUnexpectedToken.InternalParse: boolean;
begin
  _Token := NextToken;
  Result := not (_Token is TUnexpectedEOF);
end;

procedure TUnexpectedToken.PrintSelf(APrinter: TPrinter);
begin
  APrinter.Space;
  APrinter.PrintItem(_Token);
  APrinter.NextLine;
end;

end.
