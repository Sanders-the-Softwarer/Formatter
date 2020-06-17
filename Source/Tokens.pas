////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������  ����������                         //
//                                                                            //
//                           ������ �������� � ������                         //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Tokens;

{ ----- ���������� -------------------------------------------------------------

  ������� ��������� ����� � Oracle ������ ����������-��������. ��������, �����
  loop � while �������� ��������� � PL/SQL, �� ������� ������ ��� ����� ������
  ��� ����� � SQL. ����� ����, ������ ������ PL/SQL ����� ��������� �����������
  ���� my_variable char(1 char). �� ������ ������������ ������� ����������
  ����������, � ����� ������ ������������ ����������� �����, ������� ������
  TIdent � TKeyword � ����� ������ �������� ����� �������, � ����� ����� TEpithet
  � ��������� IsKeyword. ���� ������� ������������� � ���� ���������������
  ������� � ������ ������ ��� ��������, �������� ������� ������� � �����
  ��������.

------------------------------------------------------------------------------ }

interface

uses System.SysUtils, System.Generics.Collections;

type

  { ��������� ������������ ����������� }
  TCommentPosition = (poSpecial, poAfter, poFarAbove, poAbove, poBelow, poFarBelow);
  TCommentPositions = set of TCommentPosition;

  { ��������������� ���������� ����� }
  TComment = class;

  { ������� ����� ������ }
  TBaseToken = class
  end;

  { ������ ����������� ������ }
  TChar = class(TBaseToken)
  strict private
    FValue: char;
  public
    constructor Create(AValue: char);
    property Value: char read FValue;
  end;

  { ����������� ������, ����������� ���������� � ������ }
  TPositionedChar = class(TChar)
  strict private
    FLine, FCol: integer;
  public
    constructor Create(AValue: char; ALine, ACol: integer);
    property Line: integer read FLine;
    property Col: integer read FCol;
  end;

  { ������� }
  TToken = class(TBaseToken)
  strict private
    FValue, FInitialValue: string;
    FLine, FCol: integer;
    FPrinted, FCanReplace: boolean;
    FComments: array[TCommentPosition] of TComment;
  strict protected
    function ModifyValue(const AValue: string): string; virtual;
    function GetComment(Index: TCommentPosition): TComment;
    procedure SetComment(Index: TCommentPosition; AComment: TComment);
  public
    function TokenType: string; virtual; abstract;
  public
    constructor Create(const AValue: string; AChar: TPositionedChar); overload;
    constructor Create(AChar: TPositionedChar); overload;
    constructor Create(const AValue: string; ALine, ACol: integer); overload;
    property Value: string read FValue;
    property Line: integer read FLine;
    property Col: integer read FCol;
    property InitialValue: string read FInitialValue;
    property Printed: boolean read FPrinted write FPrinted;
    property CanReplace: boolean read FCanReplace write FCanReplace;
    property CommentFarAbove: TComment index poFarAbove read GetComment write SetComment;
    property CommentAbove: TComment    index poAbove    read GetComment write SetComment;
    property CommentAfter: TComment    index poAfter    read GetComment write SetComment;
    property CommentBelow: TComment    index poBelow    read GetComment write SetComment;
    property CommentFarBelow: TComment index poFarBelow read GetComment write SetComment;
  end;

  { ����������� ��� ����������� ������� - ���������� ������, � �������� �� ����� ���������� ������� }
  TUnknownToken = class(TToken)
  public
    function TokenType: string; override;
  end;

  { ���������� ������� }
  TWhitespace = class(TToken)
  public
    function TokenType: string; override;
  end;

  { �������� ����� �������� � ��������� ���������� }
  TOpType = (otNone, otUnary, otBinary);

  { ���������� ������� }
  TTerminal = class(TToken)
  strict private
    FOpType: TOpType;
    FWithoutSpace: boolean;
  strict protected
    function ModifyValue(const AValue: string): string; override;
  public
    function TokenType: string; override;
    property OpType: TOpType read FOpType write FOpType; { ��������� �������� ������� �����-������ �� �������� }
    property WithoutSpace: boolean read FWithoutSpace write FWithoutSpace; { ��������� �������� ������� � number(5,2) � �������� �������� }
  end;

  { ������������� ���� �������� ����� }
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

  { ����� }
  TLabel = class(TToken)
  strict protected
    function ModifyValue(const AValue: string): string; override;
  public
    function TokenType: string; override;
  end;

  { ����������� }
  TComment = class(TToken)
  public
    Position: TCommentPosition;
    Lead: TToken;
    function TokenType: string; override;
    function LineComment: boolean;
    function Height: integer;
    function ShiftedValue(ACol: integer): string;
    function LeadComment: TComment;
  end;

  { ����� }
  TNumber = class(TToken)
  public
    function TokenType: string; override;
  end;

  { ������� }
  TLiteral = class(TToken)
  public
    function TokenType: string; override;
  end;

  { ����������� ����� �������� ����� }
  TUnexpectedEOF = class(TToken)
  public
    function TokenType: string; override;
  end;

  { ������ ������� - ����������� ������, ������� ������ ����� }
  TEmptyToken = class(TToken)
  public
    function TokenType: string; override;
    constructor Create;
  end;

  { ��� ������� ��� ������ ����������� ������������ }
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
  FLine  := AChar.Line;
  FCol   := AChar.Col;
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
  FLine  := ALine;
  FCol   := ACol;
end;

function TToken.ModifyValue(const AValue: string): string;
begin
  Result := AValue;
end;

function TToken.GetComment(Index: TCommentPosition): TComment;
begin
  Result := FComments[Index];
end;

procedure TToken.SetComment(Index: TCommentPosition; AComment: TComment);
begin
  if Assigned(AComment) and Assigned(FComments[Index])
    then raise Exception.CreateFmt('Trying to link comment [%s] while comment [%s] is already linked', [AComment.Value, FComments[Index].Value])
    else FComments[Index] := AComment;
  if Assigned(AComment) then
  begin
    AComment.Position := Index;
    AComment.Lead     := Self;
  end;
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

{ TEpithet }

function TEpithet.TokenType: string;
begin
  if IsKeyword then
    Result := '�������� �����'
  else if FIsIdent then
    Result := '�������������'
  else
    Result := '�������������������� �����';
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
  Result := '�����������';
end;

{ ������� ��������� (�� �������� ������) ����������� }
function TComment.LineComment: boolean;
begin
  Result := Value.StartsWith('--');
end;

{ ������ ����������� � ������� }
function TComment.Height: integer;
var i: integer;
begin
  Result := 1;
  for i := Length(Value) downto 1 do if Value[i] = #13 then Inc(Result);
end;

{ �������� � ����� �������������� ����������� �������� �� ������� ����� ���,
  � ����� ������� �� ����� �������, � ���, � ������� ��� � ��������� }
function TComment.ShiftedValue(ACol: integer): string;
var
  D, i, j: integer;
  C: char;
begin
  D := ACol - Col;
  i := 1;
  while i <= Length(Value) do
  begin
    C := Value[i];
    Result := Result + C;
    Inc(i);
    if C = #13 then
      if D > 0 then
        Result := Result + StringOfChar(' ', D)
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
end;

function TComment.LeadComment: TComment;
begin
  Result := Self;
  while Result.Lead is TComment do Result := TComment(Result.Lead);
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

{ TTerminal }

function TTerminal.TokenType: string;
begin
  Result := '������';
end;

function TTerminal.ModifyValue(const AValue: string): string;
begin
  Result := AValue.ToLower;
end;

{ TUnexpectedEOF }

function TUnexpectedEOF.TokenType: string;
begin
  Result := '����� �������� �����';
end;

{ TLabel }

function TLabel.TokenType: string;
begin
  Result := '�����';
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
  Result := '������ �������';
end;

{ TSpecialComment }

constructor TSpecialComment.Create(AText: string);
begin
  inherited Create(AText, -1, -1);
end;

function TSpecialComment.TokenType: string;
begin
  Result := '����������� �������������';
end;

initialization
  UnexpectedEOF := TUnexpectedEOF.Create;

finalization
  FreeAndNil(UnexpectedEOF);

end.

