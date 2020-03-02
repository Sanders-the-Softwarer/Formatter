unit TestRulers;

interface

uses SysUtils, TestFramework, System.Generics.Collections;

implementation

uses Printers_, Tokens, Statements;

type
  _Rulers = class(TTestCase)
  private
    Printer: TPrinter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Простое_Выравнивание;
    procedure Выравнивание_С_Пропусками;
    procedure Без_Выравнивания;
  end;

  TCheckStatement = class(TStatement)
  private
    FlagAligned, FlagSkip: boolean;
    Tokens: TObjectList<TToken>;
    function Token(const S: string): TToken;
  public
    constructor Create(AAligned, ASkip: boolean);
    destructor Destroy; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
    function Aligned: boolean; override;
  end;

{ _Rulers }

procedure _Rulers.SetUp;
begin
  inherited;
  Printer := Printers_.CreateFormatterPrinter;
  Printer.BeginPrint;
end;

procedure _Rulers.TearDown;
begin
  FreeAndNil(Printer);
  inherited;
end;

procedure _Rulers.Простое_Выравнивание;
var
  Stmt: TCheckStatement;
  S: string;
begin
  Stmt := TCheckStatement.Create(true, false);
  Printer.PrintItem(Stmt);
  FreeAndNil(Stmt);
  S := Printer.GetText;
  CheckEquals('| aaa1 b1   cc1'#13#10 +
              '| aa2  bbb2 c2'#13#10  +
              '| a3   bb3  ccc3', S);
end;

procedure _Rulers.Выравнивание_С_Пропусками;
var
  Stmt: TCheckStatement;
  S: string;
begin
  Stmt := TCheckStatement.Create(true, true);
  Printer.PrintItem(Stmt);
  FreeAndNil(Stmt);
  S := Printer.GetText;
  CheckEquals('| aaa1      cc1'#13#10 +
              '| aa2  bbb2'#13#10  +
              '|      bb3  ccc3', S);
end;

procedure _Rulers.Без_Выравнивания;
var
  Stmt: TCheckStatement;
  S: string;
begin
  Stmt := TCheckStatement.Create(false, false);
  Printer.PrintItem(Stmt);
  FreeAndNil(Stmt);
  S := Printer.GetText;
  CheckEquals('| aaa1 b1 cc1'#13#10 +
              '| aa2 bbb2 c2'#13#10  +
              '| a3 bb3 ccc3', S);
end;

{ TCheckStatement }

constructor TCheckStatement.Create(AAligned, ASkip: boolean);
begin
  FlagAligned := AAligned;
  FlagSkip    := ASkip;
  Tokens := TObjectList<TToken>.Create(true);
end;

destructor TCheckStatement.Destroy;
begin
  FreeAndNil(Tokens);
  inherited;
end;

function TCheckStatement.Aligned: boolean;
begin
  Result := FlagAligned;
end;

procedure TCheckStatement.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.StartRuler(true);
  APrinter.PrintRulerItem('@', Token('|'));
  APrinter.PrintRulerItem('a', Token('aaa1'));
  if FlagSkip
    then APrinter.PrintRulerItem('b', nil)
    else APrinter.PrintRulerItem('b', Token('b1'));
  APrinter.PrintRulerItem('c', Token('cc1'));
  APrinter.NextLine;
  APrinter.StartRuler(true);
  APrinter.PrintRulerItem('@', Token('|'));
  APrinter.PrintRulerItem('a', Token('aa2'));
  APrinter.PrintRulerItem('b', Token('bbb2'));
  if FlagSkip
    then APrinter.PrintRulerItem('c', nil)
    else APrinter.PrintRulerItem('c', Token('c2'));
  APrinter.NextLine;
  APrinter.StartRuler(true);
  APrinter.PrintRulerItem('@', Token('|'));
  if FlagSkip
    then APrinter.PrintRulerItem('a', nil)
    else APrinter.PrintRulerItem('a', Token('a3'));
  APrinter.PrintRulerItem('b', Token('bb3'));
  APrinter.PrintRulerItem('c', Token('ccc3'));
end;

function TCheckStatement.Token(const S: string): TToken;
begin
  Result := TToken.Create(S, -1, -1);
  Tokens.Add(Result);
end;

initialization
  RegisterTest(_Rulers.Suite);

end.

