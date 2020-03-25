////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                   ��������� ������ ���������������� ������                 //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit TextBuilder;

{ ----- ���������� -------------------------------------------------------------

  TextBuilder - ����������, ��������� �� ������ ������������ StringBuilder.
  ��� ������������ �� �������� ������ ������, ������� ����� FormatterPrinter,
  ������������ ������� ������ � �������, ������������ ������ ������, ����������
  �� ���� ������� FormatterPrinter � ������� ��� ���. ����� ����, TextBuilder
  ����� �������� �� �������� ���������� ������ - ������ ������ �������. ���
  �������� ������������ � DraftPrinter ��� �����������.

------------------------------------------------------------------------------ }

interface

uses SysUtils;

type

  { ��������� ������ ���������������� ������ }
  TTextBuilder = class
  strict private
    { StringBuilder, � ������� ������������� ����� }
    Builder: TStringBuilder;
    { �������� ������� ������� }
    FLine, FCol, FMaxWidth, FLength: integer;
  public
    constructor Create(AMeasureOnly: boolean);
    destructor Destroy; override;
    procedure Clear;
  public
    { ���������� �������� ������ (Count ����)}
    procedure AppendLine(Count: integer);
    { ���������� ������� (Count ����)}
    procedure AppendSpace(Count: integer);
    { ���������� ������ }
    procedure AppendText(Text: string);
    { ������� ��������������� ������ }
    function Text: string;
    { �������������� �������� ��������� ������ }
    property Line: integer read FLine;
    property Col: integer read FCol;
    property Length: integer read FLength;
    function MaxWidth: integer;
  end;

implementation

{ TTextBuilder }

constructor TTextBuilder.Create(AMeasureOnly: boolean);
begin
  if not AMeasureOnly then Builder := TStringBuilder.Create;
  Clear;
end;

destructor TTextBuilder.Destroy;
begin
  FreeAndNil(Builder);
  inherited;
end;

procedure TTextBuilder.Clear;
begin
  if Assigned(Builder) then Builder.Clear;
  FLine := 1;
  FCol  := 1;
  FMaxWidth := 0;
  FLength := 0;
end;

function TTextBuilder.Text: string;
begin
  if Assigned(Builder)
    then Result := Builder.ToString
    else Result := '';
end;

{ ���������� �������� ������ (Count ����)}
procedure TTextBuilder.AppendLine(Count: integer);
var i: integer;
begin
  Assert(Count >= 0);
  if Count = 0 then exit;
  if Col > FMaxWidth then FMaxWidth := Col - 1;
  Inc(FLine, Count);
  FCol := 1;
  Inc(FLength, 2 * Count);
  if Assigned(Builder) then
    for i := 1 to Count do Builder.AppendLine;
end;

{ ���������� ������� (Count ����)}
procedure TTextBuilder.AppendSpace(Count: integer);
begin
  Assert(Count >= 0);
  if Count = 0 then exit;
  Inc(FCol, Count);
  Inc(FLength, Count);
  if Assigned(Builder) then Builder.Append(' ', Count);
end;

{ ���������� ������ }
procedure TTextBuilder.AppendText(Text: string);
var
  Lines: TArray<string>;
  i, Len: integer;
begin
  if Text = '' then exit;
  { �������� ����� �� ������ }
  Text := StringReplace(Text, #13#10, #13, [rfReplaceAll]);
  Lines := Text.Split([#13, #10]);
  { � ������� ������ �� ������� }
  for i := Low(Lines) to High(Lines) do
  begin
    Len := Lines[i].Length;
    Inc(FCol, Len);
    Inc(FLength, Len);
    if Assigned(Builder) then Builder.Append(Lines[i]);
    if i < High(Lines) then AppendLine(1);
  end;
end;

{ ������� ������������ ������ ������ }
function TTextBuilder.MaxWidth: integer;
begin
  { ���� ��� �������� ������, ����� ��������� ������ ����� � �� ������� � FMaxWidth }
  if FCol > FMaxWidth
    then Result := FCol - 1
    else Result := FMaxWidth;
end;

end.
