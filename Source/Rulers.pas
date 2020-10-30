////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                       ���� ���������� � �������������                      //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Rulers;

{ ----- ���������� -------------------------------------------------------------

  ����� �� �������� ��� ������������� �������� ������������ - �� ���� �����
  ��������� ��������� ������ "�� ��������". ��������, ������ ����������
  ��������� �� ���:

      declare
        a integer; -- ���������� �
        bbb varchar2(30); -- ���������� �

  � ���:

      declare
        a   integer;      -- ���������� �
        bbb varchar2(30); -- ���������� �

  ��� ����, ����� ����� ��������, ����� TRulers �������� ���������� � ��������
  ��������� ��������� �������������� ����������� � ����� ������������ � ����
  ������ "������", ��� ������� ��� ������ ��������������.

  ���������� ������������ �������� ��������� �������: ������� ������� �����
  ����� � ������ fmGetRulers, ��� ���� � ������� ������ Take ������� ����������
  � �������������, � ����� ����� ��� � ������ fmSetRulers, ��� ���� � �������
  ������ Fix ���������� ����������� � ������ ������ ����� �������������
  ��������.

  ��� �� ���� �������� ������������ ��������� ��������� �������. ���������
  ����� �������������� ����������� ������������� �������. ������ ������
  ��������������� �������, ������� ���������� �� �������� ������������ �
  ������. � ������� ������ ������ ������� ������������� �� ������ ������������
  ������. ��� �������� ��������� "��������� �������" �������� � ��������
  �������� ��������. ��� ���� � ���, ��� �����, ������ �� �������� �������,
  ����� "�������" � ��������� ������, ������� ������ ����� ������ �� �����������
  ��� ����������� ������ �������, � ���������� ����� ������ ������� "�����"
  ��� � ����� ������ ��������� ������� (� ���������, ���� � ��� ���� �������
  ������...)

------------------------------------------------------------------------------ }

interface

uses Classes, SysUtils, System.Generics.Collections, Math, Utils, Printer;

type

  { ���������� � ������������� }
  TRulers = class
  private
    FOwner: TObject;
    { ������� ������� }
    CurrentRuler: string;
    { ������� � ��� �������, � ������� ��� ���� � �������������� ����������� }
    Names: TStringList;
    { ���������� � ������� }
    Cells: array of array of array of integer;
    { ������� �� ������ ������ Start }
    StartLine, StartCol: integer;
    { ������ ��������� ��������� ����� }
    MinLine, MaxLine, MaxColIndex: integer;
    { ������ �����������, ������������ �������� ��������� Take � Fix }
    FShift: integer;
    { ��������� ���������� ������ }
    FUseSpaces: boolean;
    { ���� ������������� ���������� }
    FFull: boolean;
    { �������� ������� ������ }
    Rulers: TDictionary<string, integer>;
  protected
    function GetCell(ALine, ACol, AIndex: integer): integer;
    procedure SetCell(ALine, ACol, AIndex, AValue: integer);
    procedure CalcRulers;
    property Width[ALine, ACol: integer]: integer index 0 read GetCell write SetCell;
    property Carry[ALine, ACol: integer]: integer index 1 read GetCell write SetCell;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    procedure AddRuler(const ARuler: string);
    procedure Start(const ARuler: string; ALine, ACol: integer);
    procedure Measure(ALine, ACol: integer; AFromStart: boolean = false);
    function Fix(const ARuler: string): integer;
    function Empty: boolean;
    property Shift: integer read FShift write FShift;
    property UseSpaces: boolean read FUseSpaces write FUseSpaces;
    property Full: boolean read FFull write FFull;
  public
    function DebugInfo: string;
    procedure Print(APrinter: TPrinter);
  end;

implementation

const
  LEFT_RULER = '$left$';

constructor TRulers.Create;
begin
  Names := TStringList.Create;
  Names.Add(LEFT_RULER);
  FOwner := AOwner;
end;

destructor TRulers.Destroy;
begin
  FreeAndNil(Rulers);
  FreeAndNil(Names);
  inherited;
end;

procedure TRulers.AddRuler(const ARuler: string);
begin
  if Names.IndexOf(ARuler) < 0 then Names.Add(ARuler);
end;

procedure TRulers.Start(const ARuler: string; ALine, ACol: integer);
begin
  if CurrentRuler = ARuler then exit;
  Measure(ALine, ACol, true);
  CurrentRuler := ARuler;
  StartLine := ALine;
  StartCol := ACol;
  Utils._Debug('[%p] Start, ruler = %s, line = %d, col = %d', [pointer(Self), ARuler, ALine, ACol]);
end;

procedure TRulers.Measure(ALine, ACol: integer; AFromStart: boolean = false);
var ColIndex: integer;
begin
  { ��� ����� ������ ���������� ����� ������ }
  if AFromStart and (ALine <> StartLine) then
  begin
    CurrentRuler := LEFT_RULER;
    StartLine := ALine;
    StartCol := 1;
  end;
  { ���� ������� ��� CurrentRuler - ��� ����������� ����� ����� �������� ������ }
  if CurrentRuler = '' then exit;
  Utils._Debug('[%p] Measure, ruler = %s, line = %d, col = %d', [pointer(Self), CurrentRuler, ALine, ACol]);
  { ������������� ������ � ��������� �����, � ������� ��� ������������ }
  if MinLine = 0 then MinLine := ALine;
  if MaxLine < ALine then MaxLine := ALine;
  { ������� ��������� ������� � ������, ���� � ��� ��� ��� }
  AddRuler(CurrentRuler);
  { ��� ����� ������ ������ ������ ������, ����� ������ �� ������� }
  if not AFromStart and (ALine <> StartLine) then exit;
  { ����� � ����� }
  ColIndex := Names.IndexOf(CurrentRuler);
  CurrentRuler := '';
  { �������� ������ ������� ������ }
  Width[ALine, ColIndex] := ACol - StartCol - Shift;
  Utils._Debug('[%p] Measure %s (%d, %d) => index = %d, width = %d', [pointer(Self), CurrentRuler, ALine, ACol, ColIndex, Width[ALine, ColIndex]]);
  MaxColIndex := Math.Max(MaxColIndex, ColIndex);
end;

function TRulers.Fix(const ARuler: string): integer;
begin
  if not Assigned(Rulers) then CalcRulers;
  if Rulers.ContainsKey(ARuler)
    then Utils._Debug('[%p] Fix %s, shift = %d, offset = %d', [pointer(Self), ARuler, Shift, Rulers[ARuler]])
    else Utils._Debug('[%p] Fix %s, shift = %d, - no ruler -', [pointer(Self), ARuler, Shift]);
  if Rulers.ContainsKey(ARuler)
    then Result := Shift + Rulers[ARuler]
    else Result := 0;
end;

function TRulers.Empty: boolean;
begin
  Result := (Names.Count = 0);
end;

function TRulers.DebugInfo: string;
var
  i, j: integer;
  N: string;
begin
  Result := '';
  if not Assigned(Rulers) then exit;
  for i := 0 to Names.Count - 1 do
  begin
    N := Names[i];
    Result := Result + N;
    if Rulers.ContainsKey(N) then Result := Result + Format('[ruler = %d]', [Rulers[N]]);
    Result := Result + #13;
  end;
  for i := 0 to Names.Count - 1 do
    for j := MinLine to MaxLine do
      Result := Result + Format('width[%d, %d] = %d, carry = %d (%s)', [j, i, Width[j, i], Carry[j, i], Names[i]]) + #13;
end;

procedure TRulers.Print(APrinter: TPrinter);
begin
  APrinter.NextLine;
  APrinter.PrintSpecialComment(#13 + DebugInfo);
  APrinter.NextLine;
end;

function TRulers.GetCell(ALine, ACol, AIndex: integer): integer;
begin
  Dec(ALine, MinLine);
  if (AIndex < Length(Cells)) and (ALine < Length(Cells[AIndex])) and (ACol < Length(Cells[AIndex][ALine]))
    then Result := Cells[AIndex][ALine][ACol]
    else Result := 0;
end;

procedure TRulers.SetCell(ALine, ACol, AIndex, AValue: integer);
begin
  Dec(ALine, MinLine);
  if Length(Cells) <= AIndex then SetLength(Cells, AIndex + 1);
  if Length(Cells[AIndex]) <= ALine then SetLength(Cells[AIndex], ALine + 1);
  if Length(Cells[AIndex][ALine]) <= ACol then SetLength(Cells[AIndex][ALine], ACol + 1);
  Cells[AIndex][ALine][ACol] := AValue;
end;

procedure TRulers.CalcRulers;
var Col, Line, Max, Ruler: integer;
begin
  Rulers := TDictionary<string, integer>.Create;
  Ruler := 1;
  { ������ �� �������� ����� ������� }
  for Col := 0 to Names.Count - 1 do
  begin
    { ���� ����� ���������� � � ������� ��� ������� ������, ���������� �
      ������ � �����������, ����� � ������������� }
    for Line := MinLine to MaxLine do
      if UseSpaces and (Width[Line, Col] > 0) and (Col < MaxColIndex) and (Width[Line, Col + 1] = 0) then
        begin Carry[Line, Col] := Width[Line, Col]; Width[Line, Col] := 0; end
      else if (Carry[Line, Col] > 0) and (Width[Line, Col + 1] > 0) then
        begin Width[Line, Col] := Carry[Line, Col]; Carry[Line, Col] := 0; end;
    { �������� ������������ ������ ������������� ����� }
    Max := 0;
    for Line := MinLine to MaxLine do
      Max := Math.Max(Max, Width[Line, Col]);
    { �������� ������� ��������� ������������ }
    Rulers.Add(Names[Col], Ruler);
    Inc(Ruler, Max);
    { �, �������, ���� ������, ������������ � ��������� ������ }
    for Line := MinLine to MaxLine do
      if Carry[Line, Col] > Max then
        Carry[Line, Col + 1] := Carry[Line, Col] - Max;
  end;
end;

end.

