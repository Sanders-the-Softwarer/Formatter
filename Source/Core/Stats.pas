////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                           ������ ����� ����������                          //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Stats;

{ ----- ���������� -------------------------------------------------------------

  ���������� ��� ���������� � ������� ��������������� ������� ��� ������ ������,
  ����������� ������ ��������, ������� � ������ �������������� ��� � �����
  �������������, �������, ������ �����, �� ������� ���� ������������ ���������.
  ��� ���� ������ �� ����� �������� ������ ���������� ������ ��� ������������
  �����. ��� �� �����, � ���� ������� �� ��� ���������� ���������� � ���, ���
  ��-�� ������������ ���������� �������� ��������� ���������� ��������
  ���������� ����� � ��� �� ��������, ��� �������� ���� � ��������������
  ���������.

  ������ ����� ���������� ������������ ��� ��������� �������������� ����������
  � ������ ������� � ������� �������������. �� �������� �������� ����������
  ������������ ������� � �� ��������� ������������� ���������, ��������� �
  ����� �������� � �������������.

------------------------------------------------------------------------------ }

interface

uses Classes, SysUtils, System.Generics.Collections;

type
  Statistics = class
  strict private
    class var Measures: TDictionary<string, int64>;
  public
    class constructor Create;
    class destructor Destroy;
    { ����� ���������� }
    class procedure Clear;
    { ���������� ���������� �� ������� }
    class procedure Increase(const AMeasure: string);
    { ����� ����������� ���������� }
    class function Output: string;
  end;

implementation

uses Utils;

{ Statistics }

class constructor Statistics.Create;
begin
  Measures := TDictionary<string, int64>.Create;
end;

class destructor Statistics.Destroy;
begin
  FreeAndNil(Measures);
end;

{ ����� ���������� }
class procedure Statistics.Clear;
begin
  Measures.Clear;
end;

{ ���������� ���������� �� ������� }
class procedure Statistics.Increase(const AMeasure: string);
begin
  if not GetIsDebug then exit;
  if Measures.ContainsKey(AMeasure)
    then Measures[AMeasure] := Measures[AMeasure] + 1
    else Measures.Add(AMeasure, 1);
end;

{ ����� ����������� ���������� }
class function Statistics.Output: string;
var Key: string;
begin
  if not GetIsDebug then exit;
  with TStringList.Create do
  try
    for Key in Measures.Keys do
      Add(Key + '=' + IntToStr(Measures[Key]));
    Sort;
    Result := Text;
  finally
    Free;
  end;
end;

end.
