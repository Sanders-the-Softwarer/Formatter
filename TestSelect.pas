unit TestSelect;

interface

uses
  Classes, SysUtils, DUnitX.TestFramework, Printers_, Parser, Streams, Tokenizer;

type

  [TestFixture]
  _Select = class(TObject)
  protected
    function LoadFile(const AFileName: string): string;
    function Beautify(const S: string): string;
    procedure Check(AText, AExpected: string);
  public
    [Test]
    [TestCase('Простейший_Запрос', 'Простейший_Запрос')]
    [TestCase('Запрос_С_Алиасом',  'Запрос_С_Алиасом')]
    [TestCase('Запрос_С_With',     'Запрос_С_With')]
    procedure _Из_Файла(const AFileName: string);
  end;

implementation

procedure _Select._Из_Файла(const AFileName: string);
begin
  Check(LoadFile(AFileName + '.in'), LoadFile(AFileName + '.out'));
end;

function _Select.LoadFile(const AFileName: string): string;
begin
  with TStringList.Create do
  try
    LoadFromFile('.\Тестовые Данные\' + AFileName);
    Result := Text.TrimEnd([#13, #10]);
  finally
    Free;
  end;
end;

function _Select.Beautify(const S: string): string;
const
  Liner = #13'---------->>----------'#13;
begin
  Result := Liner + StringReplace(S, ' ', #183, [rfReplaceAll]) + Liner;
end;

procedure _Select.Check(AText, AExpected: string);
var
  Settings: TFormatSettings;
  Parser: TParser;
  Printer: TPrinter;
begin
  try
    Settings := TFormatSettings.Default;
    Parser := TParser.Create(TCommentProcessor.Create(TMerger.Create(TWhitespaceSkipper.Create(TTokenizer.Create(TPositionStream.Create(TStringStream.Create(AText)))))), Settings);
    Printer := TPrinter.CreateFormatterPrinter(nil);
    Printer.Settings := Settings;
    Parser.PrintAll(Printer);
    Assert.AreEqual(Beautify(AExpected), Beautify(Printer.GetText));
  finally
    FreeAndNil(Settings);
    FreeAndNil(Parser);
    FreeAndNil(Printer);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(_Select);
end.
