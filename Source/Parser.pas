////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                          �������������� ����������                         //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
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

uses Windows, System.SysUtils, Streams, Tokens, Statements, Printers_;

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
            TComments.Parse(AParent, ASource, AResult);
end;

{ ������ ���������� PL/SQL }
class function TParser.ParsePLSQL(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := TReturn.Parse(AParent, ASource, AResult) or
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
begin
  if ParseAny(nil, Source, Result) or
     TUnexpectedToken.Parse(nil, Source, Result)
    then Result.Settings := Self.Settings
    else Result := TEOFStatement.Create(nil, Source);
end;

{ TEOFStatement }

procedure TEOFStatement.InternalPrintSelf(APrinter: TPrinter);
begin
  { ������ �� ������ }
end;

end.
