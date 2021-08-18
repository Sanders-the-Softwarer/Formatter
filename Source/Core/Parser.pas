////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                          �������������� ����������                         //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Parser;

{ ----- ���������� -------------------------------------------------------------

  �������������� ���������� �������� ������� ���������� ���������. ��
  ���������� ����������� �������� ������, ������ �� ������� �������� ���
  ����� ����� ������ �� ��������� ���������. ����� �� ���������� ���������
  �������� ������������ - ��������� ��� ���� �������� � ����, �����
  ��������� ��������� ����������� �������� ������, ������� ������, ��
  ���������� �� ��� ��������� ������� ����� � �������� �����. ���� ������
  �������� � ������ DDL, DML, PLSQL, Expressions, � ������ Parser � �����
  TParser �������� �� ���� ������ ������ �����.

  ����� ����, ��� �������������� � �������� ����� ����� � ������ �����������
  TEpithet, ���������� ����������� ���������� ������������� �����������,
  ������������ � ��������������� - �������� ����������, ���������� ������������
  � �. �. ��� ����, ����� ����� �� �����������, � ������� TParse.ParseXXXXX
  ������� ����� ������� �����, ����� � ������������ ����������� ����������� �
  ������� �� ����� ��������� � ����� ���������, ��������, ������� �����������
  ���� (����� �������� ����� type �� ���� �������� ���������� ��� ���
  ����������), ��� ������� - ����������� ���������� (����� �������� �����
  exception �� ���� �������� ���������� ��� ��� ����������), � ������ �����,
  ����� �� ��������� ������ ��������� - ���������� ���������� ����������.

------------------------------------------------------------------------------ }

interface

uses Windows, System.SysUtils, Streams, Tokens, Statements, Printer,
  System.Generics.Collections, System.Generics.Defaults;

type

  { ���������� ��� ��������������� ����������� }
  TParserInfo = class
  strict protected
    Statements: TList<TStatementClass>;
    Prepared: boolean;
  public
    constructor Create;
    destructor Destroy; override;
  public
    { ���������� ���������� � ����������� ����������� }
    procedure Add(AStatement: TStatementClass); overload;
    { ���������� ���������� �� ����������� ������ }
    procedure Add(AInfo: TParserInfo); overload;
    { ������� ������ "����������" ��� ������� ��������� �������������� ����������� }
    function Candidates: TList<TStatementClass>;
  public
    { ��������� ���������� }
    class function InstanceFor(const AName: string): TParserInfo;
  end;

  { �������������� ���������� }
  TParser = class(TNextStream<TToken, TStatement>)
  strict private
    Settings: TFormatSettings;
  strict protected
    ParserInfo: TParserInfo;
    { ������ �������� ������ ���������� ��������������� ������������� }
    function Parse(AParent: TStatement; out AResult: TStatement): boolean; overload;
    { ���������� ���������� ��������� ������� }
    function InternalNext: TStatement; override;
  public
    constructor Create(AStream: TBufferedStream<TToken>; ASettings: TFormatSettings; AParserInfo: TParserInfo);
    { ������ ����� ����������� �� ���������� ������ }
    class function Parse(AStream: TBufferedStream<TToken>; ASettings: TFormatSettings; AParserInfo: TParserInfo; AParent: TStatement; out AResult: TStatement): boolean; overload;
  end;

  { ����������� ��� ����������� ��������� ������ ���� }
  TSameTypeList = class(TStatement)
  strict private
    FStatements: array of TStatement;
  strict protected
    procedure InternalPrintSelf(APrinter: TPrinter); override;
    function Aligned: TAlignMode; override;
  public
    constructor Create(S: TStatement); reintroduce;
    procedure Add(S: TStatement);
    function ItemType: TStatementClass;
    function Transparent: boolean; override;
  end;

  { �����, ������������ ���������� ��������� � ����� }
  TSameTypeLinker = class(TNextStream<TStatement, TStatement>)
  strict private
    function CanGroup(S1, S2: TStatement): boolean;
  strict protected
    function InternalNext: TStatement; override;
  end;

const
  { ���������� �������������� ����������� ��� ������� }
  MAX_PRIORITY    =  1000;
  HIGHER_PRIORITY =   100;
  STD_PRIORITY    =     0;
  LOWER_PRIORITY  =  -100;
  LOWEST_PRIORITY =  -900;
  MIN_PRIORITY    = -1000;

implementation

type
  { "������" ��������� }
  TEOFStatement = class(TStatement)
  strict protected
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

{ TParser }

constructor TParser.Create(AStream: TBufferedStream<TToken>; ASettings: TFormatSettings; AParserInfo: TParserInfo);
begin
  Assert(AParserInfo <> nil);
  inherited Create(AStream);
  Settings := ASettings;
  ParserInfo := AParserInfo;
end;

{ ������ ����� ����������� �� ���������� ������ }
class function TParser.Parse(AStream: TBufferedStream<TToken>; ASettings: TFormatSettings; AParserInfo: TParserInfo; AParent: TStatement; out AResult: TStatement): boolean;
begin
  with TParser.Create(AStream, ASettings, AParserInfo) do
  try
    Result := Parse(AParent, AResult);
  finally
    Free;
  end;
end;

{ ������ �������� ������ ���������� ��������������� ������������� }
function TParser.Parse(AParent: TStatement; out AResult: TStatement): boolean;
var i: integer;
begin
  Assert(ParserInfo <> nil);
  Result := true;
  with ParserInfo.Candidates do
    for i := 0 to Count - 1 do
      if Items[i].Parse(AParent, Self.Source, AResult) then exit;
  Result := false;
end;

{ ���������� ���������� ��������� ������� }
function TParser.InternalNext: TStatement;
var MarkBefore: TMark;
begin
  MarkBefore := Source.Mark;
  if not Parse(nil, Result) then exit(TEOFStatement.Create(nil, Source));
  Result.Settings := Self.Settings;
  if MarkBefore = Source.Mark then raise Exception.Create('There is no progress in parsing input stream, parser aborted');
end;

{ TEOFStatement }

procedure TEOFStatement.InternalPrintSelf(APrinter: TPrinter);
begin
  { ������ �� ������ }
end;

{ TSameTypeLinker }

{ ��������� ����� ������� � ������, ���� ��� ������ � ������ � �� ������� ������������ ������������ }
function TSameTypeLinker.CanGroup(S1, S2: TStatement): boolean;
begin
  Result := Assigned(S1) and Assigned(S2) and (S1.Grouping <> nil) and (S1.Grouping = S2.Grouping) and not S2.HasCommentsAbove;
end;

function TSameTypeLinker.InternalNext: TStatement;
var
  S1, S2: TStatement;
  List: TSameTypeList;
begin
  { ���� � ��� ��� ���� �� ���� ������������ ���������, ������ ����� ��������� }
  S1 := Source.Next;
  if Source.Eof then exit(Transit(S1));
  Source.SaveMark;
  S2 := Source.Next;
  if not CanGroup(S1, S2) then
  begin
    Source.Restore;
    exit(Transit(S1));
  end;
  { � ��������� ������ �������� ������ }
  List := TSameTypeList.Create(S1);
  List.Add(S2);
  { � �������� � }
  while not Source.Eof do
  begin
    Source.SaveMark;
    S2 := Source.Next;
    if not CanGroup(S1, S2) then
    begin
      Source.Restore;
      break;
    end;
    List.Add(S2);
  end;
  { ����� ���������� ������ ��� ���� ������� }
  Result := List;
end;

{ TSameTypeList }

constructor TSameTypeList.Create(S: TStatement);
begin
  inherited Create(nil, nil);
  Add(S);
end;

procedure TSameTypeList.Add(S: TStatement);
var L: integer;
begin
  L := Length(FStatements);
  Assert((L = 0) or (S.Grouping = ItemType));
  SetLength(FStatements, L + 1);
  FStatements[L] := S;
  S.Parent := Self;
end;

procedure TSameTypeList.InternalPrintSelf(APrinter: TPrinter);
var i: integer;
begin
  for i := Low(FStatements) to High(FStatements) do
    APrinter.PrintItems([FStatements[i], _NextLine]);
end;

function TSameTypeList.Aligned: TAlignMode;
begin
  Result := FStatements[0].SameTypeAligned;
end;

function TSameTypeList.ItemType: TStatementClass;
begin
  Result := FStatements[0].Grouping;
end;

function TSameTypeList.Transparent: boolean;
begin
  Result := true;
end;

{ TParserInfo }

var
  ParserInstances: TDictionary<string, TParserInfo>;

{ ��������� ���������� }
class function TParserInfo.InstanceFor(const AName: string): TParserInfo;
begin
  if ParserInstances.TryGetValue(AName, Result) then exit;
  Result := TParserInfo.Create;
  ParserInstances.Add(AName, Result);
end;

constructor TParserInfo.Create;
begin
  inherited;
  Statements := TList<TStatementClass>.Create;
end;

destructor TParserInfo.Destroy;
begin
  FreeAndNil(Statements);
  inherited;
end;

{ ���������� ���������� � ����������� ����������� }
procedure TParserInfo.Add(AStatement: TStatementClass);
begin
  Prepared := false;
  Assert(AStatement <> nil);
  with Statements do
    if not Contains(AStatement) then Add(AStatement);
end;

{ ���������� ���������� �� ����������� ������ }
procedure TParserInfo.Add(AInfo: TParserInfo);
begin
  Prepared := false;
  Assert(AInfo <> nil);
  Statements.AddRange(AInfo.Statements);
end;

{ ������� ������ "����������" ��� ������� ��������� �������������� ����������� }

type
  TStatementClassComparer = class(TComparer<TStatementClass>)
  public
    function Compare(const Left, Right: TStatementClass): integer; override;
  end;

var
  StatementClassComparer: TStatementClassComparer;

function TParserInfo.Candidates: TList<TStatementClass>;
begin
  if not Prepared then
  begin
    Statements.Sort(StatementClassComparer);
    Prepared := true;
  end;
  Result := Statements;
end;

function TStatementClassComparer.Compare(const Left, Right: TStatementClass): integer;
begin
  Result := Right.Priority - Left.Priority;
  if Result = 0 then Result := CompareStr(Left.ClassName, Right.ClassName); { ������� ������� �����������, ����� �� ��������� ��������� ������ }
end;

initialization
  ParserInstances := TObjectDictionary<string, TParserInfo>.Create([doOwnsValues]);
  StatementClassComparer := TStatementClassComparer.Create;

finalization
  FreeAndNil(ParserInstances);
  FreeAndNil(StatementClassComparer);

end.
