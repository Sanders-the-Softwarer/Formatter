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
  ��������� � ��������

------------------------------------------------------------------------------ }

interface

uses Classes, SysUtils, TestFramework, Printers_, Parser, Tokenizer, Streams;

type
  { ����� ����������, ����������� ���������� �������������� ����� � ����������� }
  TFileBasedTest = class(TTestCase)
  private
    { ������ ����� � ������ }
    function LoadFile(const AFileName: string): string;
    { ���������� ������ � ���������� ��� ��������� ���� }
    function Beautify(const S: string): string;
    { ��������� ���������������� ������ � ��������� }
    procedure Check(AText, AExpected: string);
  protected
    Settings: TFormatSettings;
  protected
    { �������� ������ � �������� �������������� }
    procedure CheckFile(const AFileName: string);
    { ������ ��������� ������ }
    procedure Invoke(AMethod: TTestMethod); override;
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
  Settings := TFormatSettings.Default;
end;

{ ������ ����� ������������ ����� }
procedure TFileBasedTest.TearDown;
begin
  FreeAndNil(Settings);
end;

{ �������� ������ � �������� �������������� }
procedure TFileBasedTest.CheckFile(const AFileName: string);
begin
  Check(LoadFile(AFileName + '.in'), LoadFile(AFileName + '.out'));
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
    LoadFromFile('.\�������� ������\' + AFileName);
    for i := 0 to Count - 1 do Strings[i] := TrimRight(Strings[i]);
    Result := TrimRight(Text);
  finally
    Free;
  end;
end;

{ ���������� ������ � ���������� ��� ��������� ���� }
function TFileBasedTest.Beautify(const S: string): string;
const
  Liner = #13'---------->>----------'#13;
begin
  Result := Liner + StringReplace(S, ' ', #183, [rfReplaceAll]) + Liner;
end;

{ ��������� ���������������� ������ � ��������� }
procedure TFileBasedTest.Check(AText, AExpected: string);
var
  Parser: TParser;
  Printer: TPrinter;
begin
  try
    Parser := TParser.Create(
                TCommentProcessor.Create(
                  TMerger.Create(
                    TWhitespaceSkipper.Create(
                      TTokenizer.Create(
                        TPositionStream.Create(
                          TStringStream.Create(AText)))))), Settings);
    Printer := TPrinter.CreateFormatterPrinter(nil);
    Printer.Settings := Settings;
    Parser.PrintAll(Printer);
    CheckEquals(Beautify(AExpected), Beautify(Printer.GetText));
  finally
    FreeAndNil(Parser);
    FreeAndNil(Printer);
  end;
end;

end.

