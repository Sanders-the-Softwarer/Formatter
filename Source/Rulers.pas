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

uses Classes, SysUtils, System.Generics.Collections, Math, Utils;

type

  { ���������� � ������������� }
  TRulers = class
  private
    { ������� � ��� �������, � ������� ��� ���� � �������������� ����������� }
    Names: TStringList;
    { ���������� � ������� }
    Cells: array of array of array of integer;
    { �������, �� ������� �� ������������ }
    PrevLine, PrevCol, PrevColIndex, MinLine, MaxLine, MaxColIndex: integer;
    { ������ �����������, ������������ �������� ��������� Take � Fix }
    FShift: integer;
    { ��������� ���������� ������ }
    FUseSpaces: boolean;
    { ���� �������� ����� ��������� ����������� }
    SkipUntilNewLine: boolean;
    { �������� ������� ������ }
    Rulers: TDictionary<string, integer>;
  protected
    function GetCell(ALine, ACol, AIndex: integer): integer;
    procedure SetCell(ALine, ACol, AIndex, AValue: integer);
    procedure CalcRulers;
    property Width[ALine, ACol: integer]: integer index 0 read GetCell write SetCell;
    property Carry[ALine, ACol: integer]: integer index 1 read GetCell write SetCell;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddRuler(const ARuler: string);
    procedure Take(const ARuler: string; ALine, ACol: integer);
    function Fix(const ARuler: string): integer;
    function Empty: boolean;
    property Shift: integer read FShift write FShift;
    property UseSpaces: boolean read FUseSpaces write FUseSpaces;
  end;

implementation

constructor TRulers.Create;
begin
  Names := TStringList.Create;
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

procedure TRulers.Take(const ARuler: string; ALine, ACol: integer);
var ColIndex, FactIndex: integer;
begin
  { ������������� ������ � ��������� �����, � ������� ��� ������������ }
  if MinLine = 0 then MinLine := ALine;
  if MaxLine < ALine then MaxLine := ALine;
  { ������� ��������� ������� � ������, ���� � ��� ��� ��� }
  AddRuler(ARuler);
  { ����� � ����� }
  ColIndex := Names.IndexOf(ARuler);
  { ����� ����������� ��������� �� �� ������� �������, ���������� ����� ������ }
  if SkipUntilNewLine then
    if ColIndex > 0
      then exit
      else SkipUntilNewLine := false;
  { ��� ����� ������ ������� ���������� � ����������� � ������ ������� }
  if ALine <> PrevLine then
  begin
    PrevCol := 1;
    PrevColIndex := -1;
  end;
  { �������� ����������� ����� ������� ������� �������, ��� �������� �
    ������ ���������� � ������ ��������� ������� (��� �10). ����� �����
    ��������, ������� �������� ������ ������ � ������������� �����
    ������� }
  if ColIndex = PrevColIndex + 1 then
    FactIndex := ColIndex
  else
    begin
      FactIndex := PrevColIndex + 1;
      SkipUntilNewLine := true;
    end;
  { �������� ������ ������� ������ }
  Width[ALine, FactIndex] := ACol - PrevCol - Shift;
  { �������� ������� ���������, �� �������� ����� ����������� ��������� ������ }
  PrevLine := ALine;
  PrevCol := ACol;
  PrevColIndex := ColIndex;
  MaxColIndex := Math.Max(MaxColIndex, FactIndex);
end;

function TRulers.Fix(const ARuler: string): integer;
begin
  if not Assigned(Rulers) then CalcRulers;
  if Rulers.ContainsKey(ARuler)
    then Result := Shift + Rulers[ARuler]
    else Result := 0;
end;

function TRulers.Empty: boolean;
begin
  Result := (Names.Count = 0);
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
    Inc(Ruler, Max);
    Rulers.Add(Names[Col], Ruler);
    { �, �������, ���� ������, ������������ � ��������� ������ }
    for Line := MinLine to MaxLine do
      if Carry[Line, Col] > Max then
        Carry[Line, Col + 1] := Carry[Line, Col] - Max;
  end;
end;

end.

