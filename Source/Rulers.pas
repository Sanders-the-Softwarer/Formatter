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

------------------------------------------------------------------------------ }

interface

uses Classes, SysUtils, System.Generics.Collections, Math, Utils;

type
  { ���������� � ������������� }
  TRulers = class
  private
    { ������� � ��� �������, � ������� ��� ���� � �������������� ����������� }
    Names: TStringList;
    { ������������ ������ ����� }
    MaxWidth: TDictionary<String, integer>;
    { �������, �� ������� �� ������������ }
    PrevLine, PrevCol: integer;
    { ������ �����������, ������������ �������� ��������� Take � Fix }
    FShift: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddRuler(const ARuler: string);
    procedure Take(const ARuler: string; ALine, ACol: integer);
    function Fix(const ARuler: string): integer;
    function Empty: boolean;
    property Shift: integer read FShift write FShift;
  end;

implementation

constructor TRulers.Create;
begin
  Names := TStringList.Create;
  MaxWidth := TDictionary<String, integer>.Create;
end;

destructor TRulers.Destroy;
begin
  FreeAndNil(MaxWidth);
  FreeAndNil(Names);
  inherited;
end;

procedure TRulers.AddRuler(const ARuler: string);
begin
  if Names.IndexOf(ARuler) >= 0 then exit;
  Names.Add(ARuler);
  MaxWidth.Add(ARuler, 0);
end;

procedure TRulers.Take(const ARuler: string; ALine, ACol: integer);
begin
  AddRuler(ARuler);
  if ALine <> PrevLine then PrevCol := 1;
  PrevLine := ALine;
  MaxWidth[ARuler] := Math.Max(MaxWidth[ARuler], ACol - PrevCol - Shift);
  PrevCol := ACol;
end;

function TRulers.Fix(const ARuler: string): integer;
var i: integer;
begin
  Result := Shift;
  for i := 0 to Names.IndexOf(ARuler) do Inc(Result, MaxWidth[Names[i]]);
  Inc(Result);
end;

function TRulers.Empty: boolean;
begin
  Result := (Names.Count = 0);
end;

end.
