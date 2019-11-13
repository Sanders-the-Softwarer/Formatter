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
    { �������� ������ � �������� �������������� }
    procedure CheckFile(const AFileName: string);
    { ������ ��������� ������ }
    procedure Invoke(AMethod: TTestMethod); override;
  end;

implementation

{ �������� ������ � �������� �������������� }
procedure TFileBasedTest.CheckFile(const AFileName: string);
begin
  Check(LoadFile(AFileName + '.in'), LoadFile(AFileName + '.out'));
end;

{ ������ ��������� ������ }
procedure TFileBasedTest.Invoke(AMethod: TTestMethod);
begin
  { �� ������ ��� ����� ������ ���, �� ���� ��������� ����� }
  CheckFile(GetName);
end;

{ ������ ����� � ������ }
function TFileBasedTest.LoadFile(const AFileName: string): string;
begin
  with TStringList.Create do
  try
    LoadFromFile('.\�������� ������\' + AFileName);
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
  Settings: TFormatSettings;
  Parser: TParser;
  Printer: TPrinter;
begin
  try
    Settings := TFormatSettings.Default;
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
    FreeAndNil(Settings);
    FreeAndNil(Parser);
    FreeAndNil(Printer);
  end;
end;

end.

