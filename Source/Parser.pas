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

uses Windows, System.SysUtils, Streams, Tokens, Statements, PrinterIntf,
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
    class function ParseDML(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class function ParseDDL(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class function ParsePLSQL(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class function ParseSQLPlus(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class function ParseDeclaration(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class function ParseType(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class function ParseAny(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class function ParseExpression(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
  end;

  { ����������� ��� ����������� ��������� ������ ���� }
  TSameTypeList = class(TStatement)
  strict private
    FStatements: array of TStatement;
  strict protected
    procedure InternalPrintSelf(APrinter: TPrinter); override;
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

uses DDL, DML, PLSQL, SQLPlus, Expressions;

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

{ ������ �������������� ����������� DML }
class function TParser.ParseDML(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := TSelect.Parse(AParent, ASource, AResult) or
            TInsert.Parse(AParent, ASource, AResult) or
            TUpdate.Parse(AParent, ASource, AResult) or
            TDelete.Parse(AParent, ASource, AResult) or
             TMerge.Parse(AParent, ASource, AResult) or
            TCommit.Parse(AParent, ASource, AResult) or
            TRollback.Parse(AParent, ASource, AResult) or
            TSavepoint.Parse(AParent, ASource, AResult);
end;

{ ������ �������������� ����������� DDL }
class function TParser.ParseDDL(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := TCreate.Parse(AParent, ASource, AResult) or
            TDrop.Parse(AParent, ASource, AResult) or
            TComments.Parse(AParent, ASource, AResult) or
            TGrants.Parse(AParent, ASource, AResult);
end;

{ ������ ���������� PL/SQL }
class function TParser.ParsePLSQL(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := TPragma.Parse(AParent, ASource, AResult) or
            TReturn.Parse(AParent, ASource, AResult) or
            TNull.Parse(AParent, ASource, AResult) or
            TRaise.Parse(AParent, ASource, AResult) or
            TIf.Parse(AParent, ASource, AResult) or
            TCase.Parse(AParent, ASource, AResult) or
            TLoop.Parse(AParent, ASource, AResult) or
            TFor.Parse(AParent, ASource, AResult) or
            TWhile.Parse(AParent, ASource, AResult) or
            TForAll.Parse(AParent,ASource, AResult) or
            TOpenFor.Parse(AParent, ASource, AResult) or
            TFetch.Parse(AParent, ASource, AResult) or
            TExit.Parse(AParent, ASource, AResult) or
            TPipeRow.Parse(AParent, ASource, AResult) or
            TClose.Parse(AParent, ASource, AResult) or
            TExecuteImmediate.Parse(AParent, ASource, AResult) or
            TAnonymousBlock.Parse(AParent, ASource, AResult) or
            TAssignment.Parse(AParent, ASource, AResult) or
            TProcedureCall.Parse(AParent, ASource, AResult) or
            TStandaloneComment.Parse(AParent, ASource, AResult);
end;

{ ������ ���������� SQL*Plus }
class function TParser.ParseSQLPlus(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := TClear.Parse(AParent, ASource, AResult) or
            TWhenever.Parse(AParent, ASource, AResult) or
            TSet.Parse(AParent, ASource, AResult) or
            TAt.Parse(AParent, ASource, AResult) or
            TSpool.Parse(AParent, ASource, AResult) or
            TCall.Parse(AParent, ASource, AResult);
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
  Result := ParseDDL(AParent, ASource, AResult) or
            ParseDML(AParent, ASource, AResult) or
            TStandaloneAnonymousBlock.Parse(AParent, ASource, AResult) { ����� �� PLSQL } or
            ParsePLSQL(AParent, ASource, AResult) or
            ParseSQLPlus(AParent, ASource, AResult) or
            ParseDeclaration(AParent, ASource, AResult) { ������ ���� � ����� ��-�� variable declaration } or
            ParseType(AParent, ASource, AResult) or
            ParseExpression(AParent, ASource, AResult);
end;

class function TParser.ParseExpression(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
var
  S: TStatement;
  SQL: boolean;
begin
  { ����� � ������ ����������� SQL-�������� }
  SQL := false;
  S := AParent;
  while not SQL and Assigned(S) do
    if S is TDML
      then SQL := true
      else S := S.Parent;
  { � � ����������� �� ���������� ���������� ������ ����� ��������� }
  TExpression.CreatedRight;
  if SQL
    then Result := TSQLExpression.Parse(AParent, ASource, AResult)
    else Result := TExpression.Parse(AParent, ASource, AResult);
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
  { ���� ������� �� ������������, ������ ��� ����� }
  if not S.Grouping then exit(Transit(S));
  { � ��������� ������ ����� ����� ������ }
  List := TSameTypeList.Create(S);
  { ��������� ��������� �������� ���� �� ������ � ������� �� � ������ }
  while not Source.Eof do
  begin
    Source.SaveMark;
    S := Source.Next;
    if List.ItemType = S.ClassType then
      List.Add(S)
    else
      begin
        Source.Restore;
        break;
      end;
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

function TSameTypeList.ItemType: TStatementClass;
begin
  Result := TStatementClass(FStatements[0].ClassType);
end;

function TSameTypeList.Transparent: boolean;
begin
  Result := false;
end;

end.
