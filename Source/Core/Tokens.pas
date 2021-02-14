////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор  исходников                         //
//                                                                            //
//                           Классы символов и лексем                         //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
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

uses System.SysUtils, System.Generics.Collections, System.Character, TypInfo;

type

  { Возможные расположения комментария }
  TCommentPosition = (poSpecial,  { спецкомментарий }
                      poAfter,    { комментарий справа }
                      poFarAbove, { комментарий сверху через строку }
                      poAbove,    { комментарий сверху }
                      poBelow,    { комментарий снизу на уровне лексемы }
                      poBelowBOL, { комментарий снизу ближе к началу строки }
                      poFarBelow  { комментарий снизу через строку });
  TCommentPositions = set of TCommentPosition;

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
    FInitialValue, FValue: string;
    FLine, FCol: integer;
    FPrinted, FCanReplace: boolean;
    FIntoSupNextLine, FLastIntoSupNextLine: integer;
    FComments: array[TCommentPosition] of TComment;
    {$IFDEF DEBUG}
    FDebugInfo: string;
    FPrintCount: integer;
    {$ENDIF}
    function GetSkipChangeLineComment: boolean;
  strict protected
    function ModifyValue(const AValue: string): string; virtual;
  public
    function TokenType: string; virtual; abstract;
    function GetComment(Index: TCommentPosition): TComment;
    procedure SetComment(Index: TCommentPosition; AComment: TComment);
    {$IFDEF DEBUG}
    function TokenDebugInfo: string; virtual;
    function DebugInfo: string;
    procedure AddDebugInfo(const Msg: string; Params: array of const);
    property PrintCount: integer read FPrintCount write FPrintCount;
    {$ENDIF}
  public
    constructor Create(const AValue: string; AChar: TPositionedChar); overload;
    constructor Create(AChar: TPositionedChar); overload;
    constructor Create(const AValue: string; ALine, ACol: integer); overload;
    property InitialValue: string read FInitialValue;
    property Value: string read FValue write FValue;
    property Line: integer read FLine;
    property Col: integer read FCol;
    property Printed: boolean read FPrinted write FPrinted;
    property CanReplace: boolean read FCanReplace write FCanReplace;
    property SkipChangeLineComment: boolean read GetSkipChangeLineComment;
    property CommentFarAbove: TComment index poFarAbove read GetComment write SetComment;
    property CommentAbove: TComment    index poAbove    read GetComment write SetComment;
    property CommentAfter: TComment    index poAfter    read GetComment write SetComment;
    property CommentBelow: TComment    index poBelow    read GetComment write SetComment;
    property CommentBelowBOL: TComment index poBelowBOL read GetComment write SetComment;
    property CommentFarBelow: TComment index poFarBelow read GetComment write SetComment;
    property IntoSupNextLine: integer read FIntoSupNextLine write FIntoSupNextLine;
    property LastIntoSupNextLine: integer read FLastIntoSupNextLine write FLastIntoSupNextLine;
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
  strict private
    FOpType: TOpType;
    FWithoutSpace: boolean;
  strict protected
    function ModifyValue(const AValue: string): string; override;
  public
    function TokenType: string; override;
    property OpType: TOpType read FOpType write FOpType; { позволяет отличить унарные плюсы-минусы от бинарных }
    property WithoutSpace: boolean read FWithoutSpace write FWithoutSpace; { позволяет отличить запятую в number(5,2) и подобные ситуации }
  end;

  { Идентификатор либо ключевое слово }
  TEpithet = class(TToken)
  strict private
    FIsKeyword, FIsIdent: boolean;
  strict protected
    function ModifyValue(const AValue: string): string; override;
  public
    function TokenType: string; override;
    property IsIdent: boolean read FIsIdent write FIsIdent;
    property IsKeyword: boolean read FIsKeyword write FIsKeyword;
  end;

  { Метка }
  TLabel = class(TToken)
  strict protected
    function ModifyValue(const AValue: string): string; override;
  public
    function TokenType: string; override;
  end;

  { Комментарий }
  TComment = class(TToken)
  public
    Position: TCommentPosition;
    Lead: TToken;
    FixBug71: boolean;
    ChangeTypeToBrackets: boolean;
    function TokenType: string; override;
    function LineComment: boolean;
    function Height: integer;
    function LeadComment: TComment;
    procedure ShiftTo(ACol: integer);
    function CorrectSpaces: string;
    {$IFDEF DEBUG}
    function TokenDebugInfo: string; override;
    {$ENDIF}
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

  { Пустая лексема - технический объект, который иногда нужен }
  TEmptyToken = class(TToken)
  public
    function TokenType: string; override;
    constructor Create;
  end;

  { Тип лексемы для вывода специальных комментариев }
  TSpecialComment = class(TComment)
  public
    function TokenType: string; override;
    constructor Create(AText: string);
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
  FInitialValue := AChar.Value;
  FValue := ModifyValue(FInitialValue);
  FLine := AChar.Line;
  FCol  := AChar.Col;
end;

constructor TToken.Create(const AValue: string; AChar: TPositionedChar);
begin
  Create(AChar);
  FInitialValue := AValue;
  FValue := ModifyValue(FInitialValue);
end;

constructor TToken.Create(const AValue: string; ALine, ACol: integer);
begin
  FInitialValue := AValue;
  FValue := ModifyValue(FInitialValue);
  FLine := ALine;
  FCol  := ACol;
end;

{$IFDEF DEBUG}

function TToken.TokenDebugInfo: string;
begin
  Result := Format('Лексема: %s'#13#13'Значение: %s'#13#13'Исходная позиция: [%d, %d]'#13#13'Напечатана: %d раз', [TokenType, Value, Line, Col, PrintCount]);
end;

function TToken.DebugInfo: string;
begin
  Result := TokenDebugInfo;
  if (Result <> '') and (FDebugInfo <> '') then Result := Result + #13#13;
  Result := Result + FDebugInfo;
end;

procedure TToken.AddDebugInfo(const Msg: string; Params: array of const);
begin
  if FDebugInfo = ''
    then FDebugInfo := Format(Msg, Params)
    else FDebugInfo := FDebugInfo + #13#13 + Format(Msg, Params);
end;

{$ENDIF}

function TToken.ModifyValue(const AValue: string): string;
begin
  Result := AValue;
end;

function TToken.GetSkipChangeLineComment: boolean;
begin
  Result := (IntoSupNextLine = LastIntoSupNextLine);
end;

function TToken.GetComment(Index: TCommentPosition): TComment;
begin
  Result := FComments[Index];
end;

procedure TToken.SetComment(Index: TCommentPosition; AComment: TComment);
begin
  if Assigned(AComment) and Assigned(FComments[Index]) then
    if Index = poAfter
      then FComments[Index].CommentAfter := AComment
      else raise Exception.CreateFmt('Trying to link comment [%s] while comment [%s] is already linked', [AComment.Value, FComments[Index].Value])
  else
    FComments[Index] := AComment;
  if Assigned(AComment) then
  begin
    AComment.Position := Index;
    AComment.Lead     := Self;
  end;
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

function TEpithet.ModifyValue(const AValue: string): string;
begin
  if AValue.StartsWith('"')
    then Result := AValue
    else Result := LowerCase(AValue);
end;

{ TComment }

function TComment.TokenType: string;
begin
  Result := 'Комментарий';
end;

{ Признак строчного (до перевода строки) комментария }
function TComment.LineComment: boolean;
begin
  Result := Value.StartsWith('--');
end;

{ Высота комментария в строках }
function TComment.Height: integer;
var i: integer;
begin
  Result := 1;
  for i := Length(Value) downto 1 do if Value[i] = #13 then Inc(Result);
end;

{ Внесение в текст многострочного комментария поправки на разницу между тем,
  в какой позиции он будет выведен, и той, в которой был в оригинале }
procedure TComment.ShiftTo(ACol: integer);
var
  D, i, j: integer;
  C: char;
  S: string;
begin
  D := ACol - Col;
  if D = 0 then exit;
  i := 1;
  S := '';
  while i <= Length(Value) do
  begin
    C := Value[i];
    S := S + C;
    Inc(i);
    if C = #13 then
      if D > 0 then
        S := S + StringOfChar(' ', D)
      else if D < 0 then
        begin
          j := 0;
          while (j < -D) and (Value[i + j + 1] = ' ') do
          begin
            Inc(i);
            Inc(j);
          end;
        end;
      end;
  Value := S;
end;

function TComment.CorrectSpaces: string;

  var L: integer;

  function Correct(S, SE: string): string;
  begin
    Result := S;
    if (S <> '') and S[1].IsLetterOrDigit then
      Result := ' ' + Result;
    if (S <> '') and S[Length(S)].IsLetterOrDigit then
      Result := Result + SE;
  end;

begin
  L := Length(Value);
  if Value.StartsWith('--') then
    Result := '--' + Correct(Value.Substring(2), '')
  else if Value.StartsWith('/*') and Value.EndsWith('*/') then
    Result := '/*' + Correct(Value.Substring(2, L - 4), ' ') + '*/'
  else
    Exception.CreateFmt('Неверный комментарий: [%s]', [Value]);
end;

{$IFDEF DEBUG}

function TComment.TokenDebugInfo: string;
begin
  Result := inherited + #13#13 + Format('Расположение: %s', [GetEnumName(TypeInfo(TCommentPosition), Ord(Position))]);
  if SkipChangeLineComment then
    Result := Result + #13#13 + '<< Последняя лексема в однострочном блоке >>';
end;

{$ENDIF}

function TComment.LeadComment: TComment;
begin
  Result := Self;
  while Result.Lead is TComment do Result := TComment(Result.Lead);
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

function TTerminal.ModifyValue(const AValue: string): string;
begin
  Result := AValue.ToLower;
end;

{ TUnexpectedEOF }

function TUnexpectedEOF.TokenType: string;
begin
  Result := 'Конец входного файла';
end;

{ TLabel }

function TLabel.TokenType: string;
begin
  Result := 'Метка';
end;

function TLabel.ModifyValue(const AValue: string): string;
begin
  Result := AValue.ToLower;
end;

{ TEmptyToken }

constructor TEmptyToken.Create;
begin
  inherited Create('', -1, -1);
end;

function TEmptyToken.TokenType: string;
begin
  Result := 'Пустая лексема';
end;

{ TSpecialComment }

constructor TSpecialComment.Create(AText: string);
begin
  inherited Create(AText, -1, -1);
end;

function TSpecialComment.TokenType: string;
begin
  Result := 'Комментарий форматизатора';
end;

initialization
  UnexpectedEOF := TUnexpectedEOF.Create;

finalization
  FreeAndNil(UnexpectedEOF);

end.

