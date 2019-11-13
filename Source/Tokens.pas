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

{ ----- Примечания -------------------------------------------------------------

  Понятие ключевого слова в Oracle крайне контекстно-зависимо. Например, слова
  loop и while являются ключевыми в PL/SQL, но отлично сойдут как имена таблиц
  или полей в SQL. Более того, внутри самого PL/SQL можно встретить конструкции
  типа my_variable char(1 char). На уровне лексического анализа невозможно
  определить, в каком смысле используется встреченное слово, поэтому классы
  TIdent и TKeyword в конце концов пришлось слить воедино, в общий класс TEpithet
  с признаком IsKeyword. Этот признак проставляется в ходе синтаксического
  анализа и служит больше для человека, которому удобнее мыслить в таких
  терминах.

------------------------------------------------------------------------------ }

interface

uses System.SysUtils, System.Generics.Collections, Attributes;

type

  { Предварительное объявление типов }
  TComment = class;

  { Базовый класс лексем }
  TBaseToken = class
  end;

  { Просто прочитанный символ }
  TChar = class(TBaseToken)
  strict private
    FValue: char;
  public
    constructor Create(AValue: char);
    property Value: char read FValue;
  end;

  { Прочитанный символ, дополненный положением в тексте }
  TPositionedChar = class(TChar)
  strict private
    FLine, FCol: integer;
  public
    constructor Create(AValue: char; ALine, ACol: integer);
    property Line: integer read FLine;
    property Col: integer read FCol;
  end;

  { Лексема }
  TToken = class(TBaseToken)
  strict private
    FValue: string;
    FLine, FCol: integer;
    FPrinted, FCanReplace: boolean;
    FCommentsAbove, FCommentsBelow, FCommentsBefore, FCommentsAfter: TList<TComment>;
  public
    function TokenType: string; virtual; abstract;
  public
    constructor Create(const AValue: string; AChar: TPositionedChar); overload;
    constructor Create(AChar: TPositionedChar); overload;
    constructor Create(const AValue: string; ALine, ACol: integer); overload;
    destructor Destroy; override;
    procedure AddCommentAbove(AComment: TComment);
    procedure AddCommentBelow(AComment: TComment);
    procedure AddCommentBefore(AComment: TComment);
    procedure AddCommentAfter(AComment: TComment);
    property Value: string read FValue;
    property Line: integer read FLine;
    property Col: integer read FCol;
    property Printed: boolean read FPrinted write FPrinted;
    property CanReplace: boolean read FCanReplace write FCanReplace;
  end;

  { Неожиданная или неизвестная лексема - встретился символ, с которого не может начинаться лексема }
  TUnknownToken = class(TToken)
  public
    function TokenType: string; override;
  end;

  { Незначащие символы }
  TWhitespace = class(TToken)
  public
    function TokenType: string; override;
  end;

  { Различие между унарными и бинарными операциями }
  TOpType = (otNone, otUnary, otBinary);

  { Символьная лексема }
  TTerminal = class(TToken)
  private
    FOpType: TOpType;
    FIntoNumber: boolean;
  public
    function TokenType: string; override;
    property OpType: TOpType read FOpType write FOpType; { позволяет отличить унарные плюсы-минусы от бинарных }
    property IntoNumber: boolean read FIntoNumber write FIntoNumber; { позволяет отличить запятую в number(5,2) }
  end;

  { Идентификатор либо ключевое слово }
  [LowerCase]
  TEpithet = class(TToken)
  private
    FIsKeyword, FIsIdent: boolean;
  public
    function TokenType: string; override;
    property IsIdent: boolean read FIsIdent write FIsIdent;
    property IsKeyword: boolean read FIsKeyword write FIsKeyword;
  end;

  { Комментарий }
  TComment = class(TToken)
  public
    function TokenType: string; override;
  end;

  { Число }
  TNumber = class(TToken)
  public
    function TokenType: string; override;
  end;

  { Литерал }
  TLiteral = class(TToken)
  public
    function TokenType: string; override;
  end;

  { Неожиданный конец входного файла }
  TUnexpectedEOF = class(TToken)
  public
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

{ TPositionedChar }

constructor TPositionedChar.Create(AValue: char; ALine, ACol: integer);
begin
  inherited Create(AValue);
  FLine := ALine;
  FCol  := ACol;
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

constructor TToken.Create(const AValue: string; ALine, ACol: integer);
begin
  FValue := AValue;
  FLine  := ALine;
  FCol   := ACol;
end;

destructor TToken.Destroy;
begin
  FreeAndNil(FCommentsAbove);
  FreeAndNil(FCommentsBelow);
  FreeAndNil(FCommentsBefore);
  FreeAndNil(FCommentsAfter);
  inherited;
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

{ TEpithet }

function TEpithet.TokenType: string;
begin
  if IsKeyword then
    Result := 'Ключевое слово'
  else if FIsIdent then
    Result := 'Идентификатор'
  else
    Result := 'Неидентифицированное слово';
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

