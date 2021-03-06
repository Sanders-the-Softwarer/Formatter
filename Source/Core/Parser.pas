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
  System.Generics.Collections;

type
  { �������������� ���������� }
  TParser = class(TNextStream<TToken, TStatement>)
  strict private
    Settings: TFormatSettings;
  strict protected
    function InternalNext: TStatement; override;
  public
    constructor Create(AStream: TBufferedStream<TToken>; ASettings: TFormatSettings);
    class function ParseDeclaration(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class function ParseType(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class function ParseAny(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
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
  strict protected
    function InternalNext: TStatement; override;
  end;

implementation

uses DDL, DML, PLSQL, SQLPlus, Expressions, Select, Label_;

type
  { "������" ��������� }
  TEOFStatement = class(TStatement)
  strict protected
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

{ TParser }

constructor TParser.Create(AStream: TBufferedStream<TToken>; ASettings: TFormatSettings);
begin
  inherited Create(AStream);
  Settings := ASettings;
end;

{ ������ ���������� (����������, ��������, �����, ��������, ����� � �. �. }
class function TParser.ParseDeclaration(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := TType.Parse(AParent, ASource, AResult) or
            TCursor.Parse(AParent, ASource, AResult) or
            TPragma.Parse(AParent, ASource, AResult) or
            TSubroutineForwardDeclaration.Parse(AParent, ASource, AResult) or
            TSubroutine.Parse(AParent, ASource, AResult) or
            TExceptionDeclaration.Parse(AParent, ASource, AResult) or
            TVariableDeclarations.Parse(AParent, ASource, AResult);
end;

{ ������ �����, ����������� ������������ type }
class function TParser.ParseType(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := TRecord.Parse(AParent, ASource, AResult) or
            TObject_.Parse(AParent, ASource, AResult) or
            TPLSQLTable.Parse(AParent, ASource, AResult) or
            TRefCursor.Parse(AParent, ASource, AResult);
end;

{ ������ ������������ ������� ����������� ����������� }
class function TParser.ParseAny(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := DDLParser.Parse(AParent, ASource, AResult) or
            DMLParser.Parse(AParent, ASource, AResult) or
            TStandaloneAnonymousBlock.Parse(AParent, ASource, AResult) { ����� �� PLSQL } or
            PLSQLParser.Parse(AParent, ASource, AResult) or
            SQLPlusParser.Parse(AParent, ASource, AResult) or
            ParseDeclaration(AParent, ASource, AResult) { ������ ���� � ����� ��-�� variable declaration } or
            ParseType(AParent, ASource, AResult) or
            TExpression.Parse(AParent, ASource, AResult);
end;

{ ���������� ���������� ��������� ������� �������� � ������ ParseAny }
function TParser.InternalNext: TStatement;
var
  MarkBefore, MarkAfter: TMark;
begin
  MarkBefore := Source.Mark;
  if ParseAny(nil, Source, Result) or
     TUnexpectedToken.Parse(nil, Source, Result) then
    begin
      MarkAfter := Source.Mark;
      if MarkBefore = MarkAfter then raise Exception.Create('There is no progress in parsing input stream, parser aborted');
      Result.Settings := Self.Settings;
    end
  else
    Result := TEOFStatement.Create(nil, Source);
end;

{ TEOFStatement }

procedure TEOFStatement.InternalPrintSelf(APrinter: TPrinter);
begin
  { ������ �� ������ }
end;

{ TSameTypeLinker }

function TSameTypeLinker.InternalNext: TStatement;
var
  S: TStatement;
  List: TSameTypeList;
begin
  { ��������� ��������� ������� }
  S := Source.Next;
  { ���� ������� �� ������������, ������ ��� ������ }
  if S.Grouping = nil then exit(Transit(S));
  { � ��������� ������ ������ ����� ������ }
  List := TSameTypeList.Create(S);
  { ������� ��������� �������� ���� �� ������, �� �������� ������������� }
  while not Source.Eof do
  begin
    Source.SaveMark;
    S := Source.Next;
    if (List.ItemType <> S.Grouping) or S.HasCommentsAbove then
    begin
      Source.Restore;
      break;
    end;
    List.Add(S);
  end;
  { ��, ������ ��� �� ����� }
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

end.
