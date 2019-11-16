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

uses System.SysUtils, System.Generics.Collections, Attributes;

type

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
  private
    FOpType: TOpType;
    FIntoNumber: boolean;
  public
    function TokenType: string; override;
    property OpType: TOpType read FOpType write FOpType; { ��������� �������� ������� �����-������ �� �������� }
    property IntoNumber: boolean read FIntoNumber write FIntoNumber; { ��������� �������� ������� � number(5,2) }
  end;

  { ������������� ���� �������� ����� }
  [LowerCase]
  TEpithet = class(TToken)
  private
    FIsKeyword, FIsIdent: boolean;
  public
    function TokenType: string; override;
    property IsIdent: boolean read FIsIdent write FIsIdent;
    property IsKeyword: boolean read FIsKeyword write FIsKeyword;
  end;

  { ����������� }
  TComment = class(TToken)
  public
    function TokenType: string; override;
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

{ TTerminal }

function TTerminal.TokenType: string;
begin
  Result := '������';
end;

{ TUnexpectedEOF }

function TUnexpectedEOF.TokenType: string;
begin
  Result := '����� �������� �����';
end;

initialization
  UnexpectedEOF := TUnexpectedEOF.Create;

finalization
  FreeAndNil(UnexpectedEOF);

end.
