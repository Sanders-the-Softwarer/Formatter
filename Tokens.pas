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

uses System.SysUtils, System.Generics.Collections, Printer;

type

  { Предварительное объявление типов }
  TComment = class;

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
    FCommentsAbove, FCommentsBelow, FCommentsBefore, FCommentsAfter: TList<TComment>;
  public
    function TokenType: string; virtual; abstract;
  public
    constructor Create(const AValue: string; AChar: TPositionedChar); overload;
    constructor Create(AChar: TPositionedChar); overload;
    destructor Destroy; override;
    procedure Describe(Printer: TPrinter); override;
    procedure AddCommentAbove(AComment: TComment);
    procedure AddCommentBelow(AComment: TComment);
    procedure AddCommentBefore(AComment: TComment);
    procedure AddCommentAfter(AComment: TComment);
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
  TTerminal = class(TToken)
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

  { Ключевое слово }
  TKeyword = class(TToken)
  strict protected
    function TokenType: string; override;
  end;

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

  { Неожиданный конец входного файла }
  TUnexpectedEOF = class(TToken)
  strict protected
    function TokenType: string; override;
  end;

var
  UnexpectedEOF: TUnexpectedEOF;

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

destructor TToken.Destroy;
begin
  FreeAndNil(FCommentsAbove);
  FreeAndNil(FCommentsBelow);
  FreeAndNil(FCommentsBefore);
  FreeAndNil(FCommentsAfter);
  inherited;
end;

procedure TToken.Describe(Printer: TPrinter);
var Comma: char;
begin
  Printer.Write('%s "%s", строка %d, позиция %d', [TokenType, Value, Line, Col]);
  if Assigned(FCommentsAbove) or Assigned(FCommentsBelow) or Assigned(FCommentsBefore) or Assigned(FCommentsAfter) then
  begin
    Comma := ':';
    Printer.Write(' (комментарии');
    if Assigned(FCommentsAbove) then begin Printer.Write(Comma + ' выше'); Comma := ','; end;
    if Assigned(FCommentsBefore) then begin Printer.Write(Comma + ' слева'); Comma := ','; end;
    if Assigned(FCommentsAfter) then begin Printer.Write(Comma + ' справа'); Comma := ','; end;
    if Assigned(FCommentsBelow) then begin Printer.Write(Comma + ' ниже'); Comma := ','; end;
    Printer.Write(')');
  end;
  Printer.NextLine;
end;

procedure TToken.AddCommentAbove(AComment: TComment);
begin
  if not Assigned(FCommentsAbove) then FCommentsAbove := TList<TComment>.Create;
  FCommentsAbove.Add(AComment);
end;

procedure TToken.AddCommentBelow(AComment: TComment);
begin
  if not Assigned(FCommentsBelow) then FCommentsBelow := TList<TComment>.Create;
  FCommentsBelow.Add(AComment);
end;

procedure TToken.AddCommentBefore(AComment: TComment);
begin
  if not Assigned(FCommentsBefore) then FCommentsBefore := TList<TComment>.Create;
  FCommentsBefore.Add(AComment);
end;

procedure TToken.AddCommentAfter(AComment: TComment);
begin
  if not Assigned(FCommentsAfter) then FCommentsAfter := TList<TComment>.Create;
  FCommentsAfter.Add(AComment);
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

{ TTerminal }

function TTerminal.TokenType: string;
begin
  Result := 'Символ';
end;

{ TKeyword }

function TKeyword.TokenType: string;
begin
  Result := 'Ключевое слово';
end;

{ TUnexpectedEOF }

function TUnexpectedEOF.TokenType: string;
begin
  Result := 'Конец входного файла';
end;

initialization
  UnexpectedEOF := TUnexpectedEOF.Create;

finalization
  FreeAndNil(UnexpectedEOF);

end.

