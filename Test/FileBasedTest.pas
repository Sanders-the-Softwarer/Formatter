////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                          ������� �����  ����������                         //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit FileBasedTest;

{ ----- ���������� -------------------------------------------------------------

  ��� ������� ��� ������������� ����������, ��������� ��������� �� ���������
  ���������� �������������� ��� ��� ���� ����������� � ��������� ���������.
  ��� ��� � Delphi �������� �������� ��������� ��������� � SQL-�����, ������
  ������ ��������� � ��������� �������� � ���� ����������� ������, � �����
  ����� ��������� ��� ����� � ���������, ��� ��������� ��������� ��������
  ��������� � ��������.

  ����� ������ ������� �� �������� �����. ���� ������ ����� ����� ���� ������
  (� ���� ������ ������ �������������� �����), �� ����������� - ������� � ��
  ����� �������, ��������, ��������� ����� �������������� ��� ��� ������,
  ������� ��������� ������ ��.

------------------------------------------------------------------------------ }

interface

uses Classes, SysUtils, TestFramework, Printers_, Controller;

type
  { ����� ����������, ����������� ���������� �������������� ����� � ����������� }
  TFileBasedTest = class(TTestCase)
  protected
    Settings: TFormatSettings;
  protected
    { �������� ������ � �������� �������������� }
    procedure CheckFile(const AFileName: string); virtual;
    { ������ ��������� ������ }
    procedure Invoke(AMethod: TTestMethod); override;
    { ������ ����� � ������ }
    function LoadFile(const AFileName: string): string;
    { ���������� ������ � ���������� ��� ��������� ���� }
    function Beautify(const S: string): string;
    { ��������� ���������������� ������ � ��������� }
    procedure Check(AText, AExpected: string);
  public
    { ���������� � ���������� ����� }
    procedure SetUp; override;
    { ������ ����� ������������ ����� }
    procedure TearDown; override;
  end;

implementation

{ ���������� � ���������� ����� }
procedure TFileBasedTest.SetUp;
begin
  Settings := TFormatSettings.ForTest; { �� ��������� ��������� ��, ��� �������� � ���������� ���������� }
end;

{ ������ ����� ������������ ����� }
procedure TFileBasedTest.TearDown;
begin
  FreeAndNil(Settings);
end;

{ �������� ������ � �������� �������������� }
procedure TFileBasedTest.CheckFile(const AFileName: string);
begin
  Check(LoadFile('.\�������� ������\' + AFileName + '.in'), LoadFile('.\�������� ������\' + AFileName + '.out'));
end;

{ ������ ��������� ������ }
procedure TFileBasedTest.Invoke(AMethod: TTestMethod);
begin
  AMethod; { ���� ���� ����� ����� ������ ��� ��������� �����, ���� ��������� }
  CheckFile(GetName); { � ������ �� ����� ����� ������� ����� }
end;

{ ������ ����� � ������ }
function TFileBasedTest.LoadFile(const AFileName: string): string;
var i: integer;
begin
  with TStringList.Create do
  try
    LoadFromFile(AFileName);
    for i := 0 to Count - 1 do Strings[i] := TrimRight(Strings[i]);
    { � ������������ 1/2 �������� ���� �� �������� �� ����� ��������� �������
      ������, ��������� �� ������ �� ������ �� ����� �������� }
    if (Random >= 0.5) and AFileName.EndsWith('.in')
      then Result := Text
      else Result := TrimRight(Text);
  finally
    Free;
  end;
end;

{ ���������� ������ � ���������� ��� ��������� ���� }
function TFileBasedTest.Beautify(const S: string): string;
const
  Liner = #13#10'---------->>----------'#13#10;
begin
  Result := Liner + StringReplace(S, ' ', #183, [rfReplaceAll]) + Liner;
end;

{ ��������� ���������������� ������ � ��������� }
procedure TFileBasedTest.Check(AText, AExpected: string);
var Actual: string;
begin
  Controller.MakeFormatted(AText, Settings, Actual);
  CheckEquals(Beautify(AExpected), Beautify(Actual));
end;

end.

