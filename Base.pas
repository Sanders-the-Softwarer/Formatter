unit Base;

interface

uses
  SysUtils, System.Generics.Collections;

type

  { Принтер - абстрактный интерфейс форматированного вывода }
  TPrinter = class
  private
    FData: string;
  public
    procedure NextLine;
    procedure Write(const Msg: string); overload;
    procedure Write(const Msg: string; Params: array of const); overload;
    procedure WriteLn(const Msg: string); overload;
    procedure WriteLn(const Msg: string; Params: array of const); overload;
    property Data: string read FData;
  end;

  { Узел - абстрактный класс для блока информации, передаваемого между уровнями в ходе лексического и синтаксического анализа }
  TNode = class
  private
    FLine, FCol: integer;
  public
    constructor Create(ALine, ACol: integer);
    property Line: integer read FLine;
    property Col : integer read FCol;
    procedure Describe(APrinter: TPrinter); virtual; abstract;
  end;

  { Поток - абстрактный класс, описывающий обработчик, выдающий свой результат как последовательность узлов }
  TNodeStream = class
  private
    Nodes: TObjectList<TNode>;
  protected
    function InternalNext: TNode; virtual; abstract;
  public
    destructor Destroy; override;
  public
    function Eof: boolean; virtual; abstract;
    function Next: TNode;
  end;

  { Поток, обрабатывающий результат предыдущего потока }
  TNextStream = class(TNodeStream)
  strict private
    FSource: TNodeStream;
  protected
    property Source: TNodeStream read FSource;
  public
    constructor Create(ASource: TNodeStream); virtual;
    destructor Destroy; override;
    function Eof: boolean; override;
  end;

  { Символ - узел с очередным символом входного потока }
  TChar = class(TNode)
  strict private
    FValue: char;
  protected
    property Value: char read FValue;
  public
    constructor Create(AValue: char; ALine, ACol: integer);
    procedure Describe(APrinter: TPrinter); override;
    procedure ConvertToSpace;
  end;

  { Поток, разбивающий строку на отдельные символы }
  TStringStream = class(TNodeStream)
  private
    FValue: string;
    FIndex: integer;
  protected
    function InternalNext: TNode; override;
  public
    constructor Create(const AValue: string);
    function Eof: boolean; override;
  end;

  { Поток, подсчитывающий позицию каждого символа во входном тексте }
  TPositionStream = class(TNextStream)
  private
    Line, Col: integer;
    EOLN, SkipLF: boolean;
  protected
    function InternalNext: TNode; override;
  public
    constructor Create(ASource: TNodeStream); override;
  end;

  { Поток, буферизующий входные узлы и позволяющий вернуться и просканировать их ещё раз }
  TBufferedStream = class(TNextStream)
  private
    Buffer, Returned: TList<TNode>;
    WasEof: boolean;
  protected
    function InternalNext: TNode; override;
  public
    constructor Create(ASource: TNodeStream); override;
    destructor Destroy; override;
    function Eof: boolean; override;
    function Next: TChar; reintroduce;
    function Accept: TList<TNode>;
    function AcceptReturnOne: TList<TNode>;
    procedure Refuse;
  end;

  { Поток, осуществляющий лексический анализ и разбиение входного текста на лексемы }
  TTokenizer = class(TNextStream)
  private
    function ParseWhitespace: boolean;
    function ParseNumber: boolean;
    function ParseIdent: boolean;
    function ParseAssignment: boolean;
    function ParseConcat: boolean;
    function ParseLiteral: boolean;
    function ParseComment: boolean;
    function ParseComparision: boolean;
  protected
    function InternalNext: TNode; override;
    function Source: TBufferedStream;
  public
    constructor Create(AStream: TNodeStream); override;
  end;

  { Узел - лексема }
  TToken = class(TNode)
  private
    FList: TList<TNode>;
  protected
    function TokenName: string; virtual; abstract;
  public
    constructor Create(AList: TList<TNode>);
    destructor Destroy; override;
    procedure Describe(APrinter: TPrinter); override;
  end;

  TUnknownToken = class(TToken)
  protected
    function TokenName: string; override;
  end;

  TWhitespace = class(TToken)
  protected
    function TokenName: string; override;
  end;

  TIdent = class(TToken)
  protected
    function TokenName: string; override;
  end;

  TNumber = class(TToken)
  protected
    function TokenName: string; override;
  end;

  TLiteral = class(TToken)
  protected
    function TokenName: string; override;
  end;

  TComment = class(TToken)
  protected
    function TokenName: string; override;
  end;

  TComparision = class(TToken)
  protected
    function TokenName: string; override;
  end;

  TCharToken = class(TToken)
  protected
    function TokenName: string; override;
    procedure Describe(APrinter: TPrinter); override;
  end;

implementation

{ TPrinter }

procedure TPrinter.NextLine;
begin
  FData := FData + #13#10;
end;

procedure TPrinter.Write(const Msg: string);
begin
  FData := FData + Msg;
end;

procedure TPrinter.Write(const Msg: string; Params: array of const);
begin
  Write(Format(Msg, Params));
end;

procedure TPrinter.WriteLn(const Msg: string);
begin
  Write(Msg);
  NextLine;
end;

procedure TPrinter.WriteLn(const Msg: string; Params: array of const);
begin
  WriteLn(Format(Msg, Params));
end;

{ TNode }

constructor TNode.Create(ALine, ACol: integer);
begin
  FLine := ALine;
  FCol  := ACol;
end;

{ TNodeStream }

destructor TNodeStream.Destroy;
begin
  FreeAndNil(Nodes);
  inherited;
end;

function TNodeStream.Next: TNode;
begin
  Result := InternalNext;
  if not Assigned(Nodes) then Nodes := TObjectList<TNode>.Create(true);
  Nodes.Add(Result);
end;

{ TNextStream }

constructor TNextStream.Create(ASource: TNodeStream);
begin
  FSource := ASource;
end;

destructor TNextStream.Destroy;
begin
  FreeAndNil(FSource);
  inherited;
end;

function TNextStream.Eof: boolean;
begin
  Result := Source.Eof;
end;

{ TChar }

constructor TChar.Create(AValue: char; ALine, ACol: integer);
begin
  inherited Create(ALine, ACol);
  FValue := AValue;
end;

procedure TChar.Describe(APrinter: TPrinter);
begin
  if Col > 0
    then APrinter.WriteLn('Символ ''%s'', код %d, строка %d позиция %d', [Value, Ord(Value), Line, Col])
    else APrinter.WriteLn('Символ ''%s'', код %d', [Value, Ord(Value)]);
end;

procedure TChar.ConvertToSpace;
begin
  FValue := ' ';
end;

{ TStringStream }

constructor TStringStream.Create(const AValue: string);
begin
  FValue := AValue;
end;

function TStringStream.Eof: boolean;
begin
  Result := FIndex >= Length(FValue);
end;

function TStringStream.InternalNext: TNode;
begin
  Inc(FIndex);
  Result := TChar.Create(FValue[FIndex], -1, -1);
end;

{ TPositionStream }

constructor TPositionStream.Create(ASource: TNodeStream);
begin
  inherited;
  EOLN := true;
end;

function TPositionStream.InternalNext: TNode;
var
  C: TChar;
  Ch: char;
begin
  C := Source.Next as TChar;
  { Если пропускаем #10 после #13 }
  if SkipLF then
  begin
    if C.Value = #10 then
      if Eof
        then C.ConvertToSpace
        else C := Source.Next as TChar;
    SkipLF := false;
  end;
  { Если началась новая строка }
  if EOLN then
  begin
    Inc(Line);
    Col := 0;
    EOLN := false;
  end;
  { Генерим результат и флаги }
  Inc(Col);
  Ch := C.Value;
  SkipLF := (Ch = #13);
  if Ch = #10 then Ch := #13;
  EOLN   := (Ch = #13);
  Result := TChar.Create(Ch, Line, Col);
end;

{ TBufferedStream }

constructor TBufferedStream.Create(ASource: TNodeStream);
begin
  Buffer := TList<TNode>.Create;
  Returned := TList<TNode>.Create;
  inherited Create(TPositionStream.Create(ASource));
end;

destructor TBufferedStream.Destroy;
begin
  inherited;
  FreeAndNil(Buffer);
  FreeAndNil(Returned);
end;

function TBufferedStream.Eof: boolean;
begin
  Result := (Returned.Count = 0) and Source.Eof;
  WasEof := Result;
end;

function TBufferedStream.Next: TChar;
begin
  Result := inherited Next as TChar;
end;

function TBufferedStream.Accept: TList<TNode>;
begin
  Result := Buffer;
  Buffer := TList<TNode>.Create;
end;

function TBufferedStream.AcceptReturnOne: TList<TNode>;
begin
  if not WasEof then
  begin
    Returned.Insert(0, Buffer.Last);
    Buffer.Remove(Buffer.Last);
  end;
  Result := Accept;
end;

procedure TBufferedStream.Refuse;
begin
  Buffer.AddRange(Returned);
  Returned.Clear;
  Returned.AddRange(Buffer);
  Buffer.Clear;
end;

function TBufferedStream.InternalNext: TNode;
begin
  if Returned.Count = 0
    then Result := Source.Next
    else Result := Returned.First;
  Returned.Remove(Result);
  Buffer.Add(Result);
end;

{ TTokenizer }

constructor TTokenizer.Create(AStream: TNodeStream);
begin
  inherited Create(TBufferedStream.Create(AStream));
end;

function TTokenizer.InternalNext: TNode;
var
  FirstLetter: char;
begin
  Result := nil;
  FirstLetter := Source.Next.Value;
  Source.Refuse;
  if FirstLetter in [#13, #10, ' ', #9] then
    if ParseWhitespace
      then Result := TWhitespace.Create(Source.AcceptReturnOne)
      else Source.Refuse;
  if not Assigned(Result) and (FirstLetter in ['a'..'z', 'A'..'Z']) then
    if ParseIdent
      then Result := TIdent.Create(Source.AcceptReturnOne)
      else Source.Refuse;
  if not Assigned(Result) and (FirstLetter in ['-', '/']) then
    if ParseComment
      then Result := TComment.Create(Source.AcceptReturnOne)
      else Source.Refuse;
  if not Assigned(Result) and (FirstLetter in ['0'..'9']) then
    if ParseNumber
      then Result := TNumber.Create(Source.AcceptReturnOne)
      else Source.Refuse;
  if not Assigned(Result) and (FirstLetter = ':') then
    if ParseAssignment
      then Result := TCharToken.Create(Source.Accept)
      else Source.Refuse;
  if not Assigned(Result) and (FirstLetter = '|') then
    if ParseConcat
      then Result := TCharToken.Create(Source.Accept)
      else Source.Refuse;
  if not Assigned(Result) and (FirstLetter in ['<', '=', '>', '!', '^']) then
    if ParseComparision
      then Result := TComparision.Create(Source.AcceptReturnOne)
      else Source.Refuse;
  if not Assigned(Result) and (FirstLetter = '''') then
    if ParseLiteral
      then Result := TLiteral.Create(Source.AcceptReturnOne)
      else Source.Refuse;
  if not Assigned(Result) and (FirstLetter in ['.', ',', ';', '(', ')', '+', '-', '*', '/', '%', '@']) then
    begin
      Source.Next;
      Result := TCharToken.Create(Source.Accept);
    end;
  if not Assigned(Result) then
  begin
    Source.Next;
    Result := TUnknownToken.Create(Source.Accept);
  end;
  if (Result is TWhitespace) and not Eof then
  begin
    Result.Free;
    Result := InternalNext;
  end;
end;

function TTokenizer.Source: TBufferedStream;
begin
  Result := inherited Source as TBufferedStream;
end;

function TTokenizer.ParseIdent: boolean;
begin
  Result := false;
  while not Source.Eof and (Source.Next.Value in ['a'..'z', 'A'..'Z', '_', '$', '0'..'9']) do Result := true;
end;

function TTokenizer.ParseNumber: boolean;
begin
  Result := Source.Next.Value in ['0'..'9'];
  if Result then
    while not Source.Eof and (Source.Next.Value in ['0'..'9']) do;
end;

function TTokenizer.ParseWhitespace: boolean;
begin
  Result := false;
  while not Source.Eof and (Source.Next.Value in [' ', #9, #10, #13]) do Result := true;
end;

function TTokenizer.ParseAssignment: boolean;
begin
  Result := (Source.Next.Value = ':') and not Source.Eof and (Source.Next.Value = '=');
end;

function TTokenizer.ParseConcat: boolean;
begin
  Result := (Source.Next.Value = '|') and not Source.Eof and (Source.Next.Value = '|');
end;

function TTokenizer.ParseLiteral: boolean;
begin
  Result := false;
  if Source.Next.Value <> '''' then exit;
  while not Source.Eof do
  begin
    if Source.Next.Value <> '''' then continue;
    if Source.Next.Value <> '''' then exit(true);
  end;
end;

function TTokenizer.ParseComment: boolean;
var C: TChar;
begin
  C := Source.Next;
  if C.Value = '-' then
    begin
      Result := not Source.Eof and (Source.Next.Value = '-');
      if Result then
        while not Source.Eof and (Source.Next.Value <> #13) do;
    end
  else if C.Value = '/' then
    begin
      if Source.Eof or (Source.Next.Value <> '*') then exit(false);
      while not Source.Eof do
      begin
        if Source.Next.Value <> '*' then continue;
        if Source.Next.Value = '/' then
        begin
          if not Source.Eof then Source.Next;
          exit(true);
        end;
      end;
      Result := false;
    end
  else
    Result := false;
end;

function TTokenizer.ParseComparision: boolean;
var
  C: TChar;
  S: string;
begin
  while not Source.Eof do
  begin
    C := Source.Next;
    if C.Value in ['=', '<', '>', '!', '^']
      then S := S + C.Value
      else break;
  end;
  Result := (S = '=') or (S = '>') or (S = '<') or (S = '>=') or (S = '<=') or (S = '<>') or (S = '!=') or (S = '^=') or (S = '=>');
end;

{ TToken }

constructor TToken.Create(AList: TList<TNode>);
begin
  inherited Create(AList.First.Line, AList.First.Col);
  FList := AList;
end;

destructor TToken.Destroy;
begin
  inherited;
  FreeAndNil(FList);
end;

procedure TToken.Describe(APrinter: TPrinter);
var i: integer;
begin
  APrinter.Write(TokenName + ' ''');
  for i := 0 to FList.Count - 1 do
  begin
    Assert(FList[i] is TChar);
    APrinter.Write((FList[i] as TChar).Value);
  end;
  APrinter.WriteLn(''', строка %d позиция %d', [Line, Col]);
end;

{ TUnknownToken }

function TUnknownToken.TokenName: string;
begin
  Result := 'Неизвестная лексема';
end;

{ TWhitespace }

function TWhitespace.TokenName: string;
begin
  Result := 'Незначимые пробелы';
end;

{ TIdent }

function TIdent.TokenName: string;
begin
  Result := 'Идентификатор';
end;

{ TNumber }

function TNumber.TokenName: string;
begin
  Result := 'Число';
end;

{ TCharToken }

procedure TCharToken.Describe(APrinter: TPrinter);
var i: integer;
begin
  if FList.Count = 1 then
    FList[0].Describe(APrinter)
  else
    begin
      APrinter.Write('Лексема ');
      for i := 0 to FList.Count - 1 do APrinter.Write((FList[i] as TChar).Value);
      APrinter.WriteLn(' строка %d позиция %d', [Self.Line, Self.Col]);
    end;
end;

function TCharToken.TokenName: string;
begin
  Result := '';
end;

{ TLiteral }

function TLiteral.TokenName: string;
begin
  Result := 'Литерал';
end;

{ TComment }

function TComment.TokenName: string;
begin
  Result := 'Комментарий';
end;

{ TComparision }

function TComparision.TokenName: string;
begin
  Result := 'Сравнение';
end;

end.

