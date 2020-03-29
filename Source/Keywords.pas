////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                            ������  �������� ����                           //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Keywords;

{ ----- ���������� -------------------------------------------------------------

  ��������� ��������� ���������� � ���, ��� �� ���������� ������-���� ����������
  ������ �������� ���� �����. � ������ ��������� ���� � �� �� ����� ����� ���
  ���� ��������, ��� � ���. ��������, � PL/SQL ����� ����������� ��������
  delete, � ��� ���������� �� ������ ������� ��������� delete.

  �� ���� ������� ������ TKeyword � TIdentifier ��������� ����������� ��������
  ���� �� �����, �������� ������� ����� ����� TEpithet � �� ���� ������, ���
  � ��� �������� �������� ������, � ��� -  ���. ���, � ���� �������, ���������
  ������������� �������� ��������������� �������, ��� ��� ���� � �� ��
  ������������������ <������> <������> ����� ��������������� ���������,
  ��������:

    i integer;
    e exception;
    pragma serially_reusable;
    procedure delete;

  � ����� ������������ ��������� ������: � ������ ��������� (����������
  ����������� �������������� ������������) ������������ ���� ������ ��������
  ����. �����������, �� ������� ������ �������� ����, ��������� ��� ��
  ������������ � ����� ��� ���������.

  ����� ������ �������� ���� � ������� �������������� ����������� ���������
  ���������� ���������. ������� � ��� ������ ��������� ��������, ������� ��
  ���� ������� �������� �������.

  P.S. "������������" � ��������� ������� ������ ���������� ������: � ������
  �������� ������� � � ������ ��������� � �������������� ������.

------------------------------------------------------------------------------ }

interface

uses Classes, SysUtils, System.Generics.Collections, Tokens, Statements;

{ ��������, �������� �� ����� �������� � ��������� ��������� ����������� }
function IsKeyword(const AValue: string; AStatement: TStatement): boolean; overload;

{ ����������� ������ �������� ����, ����������� � �������� ����������� }
procedure RegisterKeywords(AStatementClass: TStatementClass; AKeywords: array of string);

{ ����������� "�������� ��������" �������������� �����������, �� �������
  ������������ �������� ���� }
procedure RegisterOrphan(AStatementClass: TStatementClass);

implementation

var
  { ������ �������-����� }
  Orphans: TList<TClass>;
  { ������ �������� ����, ������������������ �� ������� }
  Reserved: TDictionary<TClass, TStringList>;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//           ����������� �������� ���� � �������������� �����������           //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure RegisterOrphan(AStatementClass: TStatementClass);
begin
  Assert(AStatementClass <> nil);
  if Orphans.IndexOf(AStatementClass) < 0 then Orphans.Add(AStatementClass);
end;

function KeywordList: TStringList;
begin
  Result := TStringList.Create;
  Result.Sorted := true;
  Result.CaseSensitive := false;
  Result.Duplicates := dupIgnore;
end;

procedure RegisterKeywords(AStatementClass: TStatementClass; AKeywords: array of string);
var Keyword: string;
begin
  if not Reserved.ContainsKey(AStatementClass) then Reserved.Add(AStatementClass, KeywordList);
  with Reserved[AStatementClass] do for Keyword in AKeywords do Add(Keyword);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//   ��������, �������� �� ����� �������� � ��������� ��������� �����������   //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function IsOrphan(AClass: TClass): boolean;
begin
  Result := (Orphans.IndexOf(AClass) >= 0);
end;

function IsKeyword(const AValue: string; AClass: TClass): boolean; overload;
begin
  Assert(AClass <> nil);
  Result := Reserved.ContainsKey(AClass) and (Reserved[AClass].IndexOf(AValue) >= 0)
            or
            (AClass <> TStatement) and IsKeyword(AValue, AClass.ClassParent);
end;

function IsKeyword(const AValue: string; AStatement: TStatement): boolean; overload;
var AClass: TClass;
begin
  if not Assigned(AStatement) then exit(false);
  AClass := AStatement.ClassType;
  Result := IsKeyword(AValue, AStatement.ClassType) or
            not IsOrphan(AClass) and IsKeyword(AValue, AStatement.Parent);
end;

initialization
  Orphans := TList<TClass>.Create;
  Reserved := TObjectDictionary<TClass, TStringList>.Create([doOwnsValues]);

finalization
  FreeAndNil(Orphans);
  FreeAndNil(Reserved);

end.
