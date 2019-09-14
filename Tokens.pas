////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������  ����������                         //
//                                                                            //
//                           ������ �������� � ������                         //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Tokens;

interface

uses Printer;

type
  { ������� ����� ������. ��� ������� ������ ������� Describe }
  TBaseToken = class
  public
    procedure Describe(Printer: TPrinter); virtual; abstract;
  end;

  { ������ ����������� ������ }
  TChar = class(TBaseToken)
  strict private
    FValue: char;
  public
    constructor Create(AValue: char);
    property Value: char read FValue;
    procedure Describe(Printer: TPrinter); override;
  end;

  { ����������� ������, ����������� ���������� � ������ }
  TPositionedChar = class(TChar)
  strict private
    FLine, FCol: integer;
  public
    constructor Create(AValue: char; ALine, ACol: integer);
    procedure Describe(Printer: TPrinter); override;
    property Line: integer read FLine;
    property Col: integer read FCol;
  end;

  { ������� }
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

  { ����������� ��� ����������� ������� - ���������� ������, � �������� �� ����� ���������� ������� }
  TUnknownToken = class(TToken)
  strict protected
    function TokenType: string; override;
  end;

  { ���������� ������� }
  TWhitespace = class(TToken)
  strict protected
    function TokenType: string; override;
  end;

  { ���������� ������� }
  TCharToken = class(TToken)
  strict protected
    function TokenType: string; override;
  end;

  { ������������� }
  TIdent = class(TToken)
  strict protected
    function TokenType: string; override;
  end;

  { "�������" (�������������������) ������������� }
  TSimpleIdent = class(TIdent);

  { "�������������" ������������� }
  TQuotedIdent = class(TIdent);

  { ����������� }
  TComment = class(TToken)
  strict protected
    function TokenType: string; override;
  end;

  { ����� }
  TNumber = class(TToken)
  strict protected
    function TokenType: string; override;
  end;

  { ������� }
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
  Printer.WriteLn('������ "%s", ��� %d', [FValue, Ord(FValue)]);
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
  Printer.WriteLn('������ "%s", ��� %d, ������ %d, ������� %d', [Value, Ord(Value), Line, Col]);
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
  Printer.WriteLn('%s "%s", ������ %d, ������� %d', [TokenType, Value, Line, Col]);
end;

{ TUnknownToken }

function TUnknownToken.TokenType: string;
begin
  Result := '*** ����������� ������ ***';
end;

{ TWhitespace }

function TWhitespace.TokenType: string;
begin
  Result := '������';
end;

{ TIdent }

function TIdent.TokenType: string;
begin
  Result := '�������������';
end;

{ TComment }

function TComment.TokenType: string;
begin
  Result := '�����������';
end;

{ TNumber }

function TNumber.TokenType: string;
begin
  Result := '�����';
end;

{ TLiteral }

function TLiteral.TokenType: string;
begin
  Result := '�������';
end;

{ TCharToken }

function TCharToken.TokenType: string;
begin
  Result := '������';
end;

end.

