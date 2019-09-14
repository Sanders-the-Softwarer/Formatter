////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор  исходников                         //
//                                                                            //
//                           Классы символов и лексем                         //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Tokens;

interface

uses Printer;

type
  { Базовый класс лексем. Для отладки снабжён методом Describe }
  TBaseToken = class
  public
    procedure Describe(Printer: TPrinter); virtual; abstract;
  end;

  { Просто прочитанный символ }
  TChar = class(TBaseToken)
  strict private
    FValue: char;
  public
    constructor Create(AValue: char);
    property Value: char read FValue;
    procedure Describe(Printer: TPrinter); override;
  end;

  { Прочитанный символ, дополненный положением в тексте }
  TPositionedChar = class(TChar)
  strict private
    FLine, FCol: integer;
  public
    constructor Create(AValue: char; ALine, ACol: integer);
    procedure Describe(Printer: TPrinter); override;
    property Line: integer read FLine;
    property Col: integer read FCol;
  end;

  { Лексема }
  TToken = class(TBaseToken)
  strict private
    FValue: string;
    FLine, FCol: integer;
  strict protected
    function TokenType: string; virtual; abstract;
  public
    constructor Create(const AValue: string; AChar: TPositionedChar); overload;
    constructor Create(AChar: TPositionedChar); overload;
    procedure Describe(Printer: TPrinter); override;
    property Value: string read FValue;
    property Line: integer read FLine;
    property Col: integer read FCol;
  end;

  { Неожиданная или неизвестная лексема - встретился символ, с которого не может начинаться лексема }
  TUnknownToken = class(TToken)
  strict protected
    function TokenType: string; override;
  end;

  { Незначащие символы }
  TWhitespace = class(TToken)
  strict protected
    function TokenType: string; override;
  end;

  { Символьная лексема }
  TCharToken = class(TToken)
  strict protected
    function TokenType: string; override;
  end;

  { Идентификатор }
  TIdent = class(TToken)
  strict protected
    function TokenType: string; override;
  end;

  { "Простой" (регистронезависимый) идентификатор }
  TSimpleIdent = class(TIdent);

  { "Квотированный" идентификатор }
  TQuotedIdent = class(TIdent);

  { Комментарий }
  TComment = class(TToken)
  strict protected
    function TokenType: string; override;
  end;

  { Число }
  TNumber = class(TToken)
  strict protected
    function TokenType: string; override;
  end;

  { Литерал }
  TLiteral = class(TToken)
  strict protected
    function TokenType: string; override;
  end;

implementation

{ TChar }

constructor TChar.Create(AValue: char);
begin
  FValue := AValue;
end;

procedure TChar.Describe(Printer: TPrinter);
begin
  Printer.WriteLn('Символ "%s", код %d', [FValue, Ord(FValue)]);
end;

{ TPositionedChar }

constructor TPositionedChar.Create(AValue: char; ALine, ACol: integer);
begin
  inherited Create(AValue);
  FLine := ALine;
  FCol  := ACol;
end;

procedure TPositionedChar.Describe(Printer: TPrinter);
begin
  Printer.WriteLn('Символ "%s", код %d, строка %d, позиция %d', [Value, Ord(Value), Line, Col]);
end;

{ TToken }

constructor TToken.Create(AChar: TPositionedChar);
begin
  FValue := AChar.Value;
  FLine  := AChar.Line;
  FCol   := AChar.Col;
end;

constructor TToken.Create(const AValue: string; AChar: TPositionedChar);
begin
  Create(AChar);
  FValue := AValue;
end;

procedure TToken.Describe(Printer: TPrinter);
begin
  Printer.WriteLn('%s "%s", строка %d, позиция %d', [TokenType, Value, Line, Col]);
end;

{ TUnknownToken }

function TUnknownToken.TokenType: string;
begin
  Result := '*** НЕОЖИДАННЫЙ СИМВОЛ ***';
end;

{ TWhitespace }

function TWhitespace.TokenType: string;
begin
  Result := 'Пробел';
end;

{ TIdent }

function TIdent.TokenType: string;
begin
  Result := 'Идентификатор';
end;

{ TComment }

function TComment.TokenType: string;
begin
  Result := 'Комментарий';
end;

{ TNumber }

function TNumber.TokenType: string;
begin
  Result := 'Число';
end;

{ TLiteral }

function TLiteral.TokenType: string;
begin
  Result := 'Литерал';
end;

{ TCharToken }

function TCharToken.TokenType: string;
begin
  Result := 'Символ';
end;

end.

