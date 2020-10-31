////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                          ������� �����  ����������                         //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
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

uses Classes, SysUtils, StrUtils, TestFramework, Printer, Controller;

type
  { ����� ����������, ����������� ���������� �������������� ����� � ����������� }
  TFileBasedTest = class(TTestCase)
  protected
    Settings: TFormatSettings;
    Skip: boolean;
  protected
    { ��������� �������� ���������� � ���������� ��������-��������� ������ }
    function GetDir: string; virtual;
    function GetExtIn: string; virtual;
    function GetExtOut: string; virtual;
    function GetFileNameToSave(const AFileName: string): string; virtual;
  protected
    { �������� ������ � �������� �������������� }
    procedure CheckFile(AFileName: string);
    { ������ ��������� ������ }
    procedure Invoke(AMethod: TTestMethod); override;
    { ������ ����� � ������ }
    function LoadFile(const AFileName: string): string;
    { ���������� ������ � ���������� ��� ��������� ���� }
    function Beautify(const S: string): string;
    { ��������� ���������������� ������ � ��������� }
    procedure Check(AText, AExpected, ASaveToFile: string);
    { �������� ���� �� ��������� ���� }
    procedure PostponeTill(AYear, AMonth, ADay: integer);
  protected
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

{ ��������� �������� ���������� � ���������� ��������-��������� ������ }

function TFileBasedTest.GetDir: string;
begin
  Result := '..\����\';
end;

function TFileBasedTest.GetExtIn: string;
begin
  Result := '.in';
end;

function TFileBasedTest.GetExtOut: string;
begin
  Result := '.out';
end;

function TFileBasedTest.GetFileNameToSave(const AFileName: string): string;
begin
  Result := '';
end;

{ �������� ������ � �������� �������������� }
procedure TFileBasedTest.CheckFile(AFileName: string);
var Dir, FileNameIn, FileNameOut, FileNameSave, TextIn, TextOut: string;
begin
  if AFileName.StartsWith('_') then AFileName := AFileName.Substring(1);
  Dir := IncludeTrailingPathDelimiter(GetDir);
  FileNameIn  := Dir + AFileName + GetExtIn;
  FileNameOut := Dir + AFileName + GetExtOut;
  FileNameSave := GetFileNameToSave(Dir + AFileName);
  TextIn      := LoadFile(FileNameIn);
  if FileExists(FileNameOut)
    then TextOut := LoadFile(FileNameOut)
    else TextOut := TextIn;
  Check(TextIn, TextOut, FileNameSave);
end;

{ ������ ��������� ������ }
procedure TFileBasedTest.Invoke(AMethod: TTestMethod);
begin
  Skip := false;
  AMethod; { ���� ���� ����� ����� ������ ��� ��������� �����, ���� ��������� }
  if not Skip then CheckFile(GetName); { � ������ �� ����� ����� ������� ����� }
end;

{ ������ ����� � ������ }
function TFileBasedTest.LoadFile(const AFileName: string): string;
var i: integer;
begin
  with TStringList.Create do
  try
    LoadFromFile(AFileName);
    for i := 0 to Count - 1 do Strings[i] := TrimRight(Strings[i]);
    Result := TrimRight(Text);
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
procedure TFileBasedTest.Check(AText, AExpected, ASaveToFile: string);
var Actual: string;
begin
  { � ������������ 1/2 �������� ���� �� �������� �� ����� ��������� �������
    ������, ��������� �� ������ �� ������ �� ����� �������� }
  if Random >= 0.5
    then AText := TrimRight(AText) + #13
    else AText := TrimRight(AText);
  Controller.MakeFormatted(AText, Settings, Actual);
  if ASaveToFile <> '' then
    with TStringList.Create do
    try
      Text := Actual;
      SaveToFile(ASaveToFile);
    finally
      Free;
    end;
  CheckEquals(Beautify(AExpected), Beautify(Actual));
end;

{ �������� ���� �� ��������� ���� }
procedure TFileBasedTest.PostponeTill(AYear, AMonth, ADay: integer);
begin
  Skip := false; //Date < EncodeDate(AYear, AMonth, ADay);
end;

end.

