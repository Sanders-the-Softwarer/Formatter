////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//               ������� ������ ������ �������������� �����������             //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Streams;

{ ----- ���������� -------------------------------------------------------------

  ����������� ������� ��������� � ���� ������� ����������������� �������. ������
  ����� ���������� ���, ��� ������ �������� ������ � ����� ��� � ����
  ������������������ ��������. ������ ��������� ����� ����������� �����
  ����������� � ����� ������������������ ������������ ����������� ����������
  ������ - ��� ������� ������������ � �������, � ������� � ��������������
  �����������. �������� ������ ���� �� ������ � ����� ������ ������������������
  � ��������� ��������, �� ������������ ������������ ��������, �������� �������
  ��� ������������������ ��������� �� ��������� ������� �����.

------------------------------------------------------------------------------ }

interface

uses SysUtils, System.Generics.Collections, Tokens;

type

  { ������� ����� ������ ������������� ��������� �� ���� �������, Eof � Next }
  TBaseStream<T: class> = class
  public
    function Eof: boolean; virtual; abstract;
    function Next: T; virtual; abstract;
  end;

  { ����� ��������� ���������� ��������� ������� � ���������� �� ������ � ����� }
  TObjectStream<T: class> = class(TBaseStream<T>)
  strict protected
    Output: TObjectList<T>;
    function InternalNext: T; virtual; abstract;
    function OwnsOutput: boolean; virtual;
  public
    destructor Destroy; override;
    function Next: T; override;
  end;

  { ����� ����������� ��������� ����� � ��������� ������� ������������ }
  TBufferedStream<T: class> = class(TObjectStream<T>)
  public
    type TMark = integer;
  strict private
    RepeatMark, SavedMark: TMark;
  strict protected
    function InternalEof: boolean; virtual; abstract;
  public
    function Mark: TMark;
    procedure SaveMark;
    procedure Restore(AMark: TMark); overload;
    procedure Restore; overload;
  public
    function Eof: boolean; override; final;
    function Next: T; override; final;
  end;

  { ����� ������ ������ }
  TStringStream = class(TBufferedStream<TChar>)
  strict private
    FValue: string;
    FIndex: integer;
  strict protected
    function InternalNext: TChar; override;
    function InternalEof: boolean; override;
  public
    constructor Create(const AValue: string);
  end;

  { ������� ����� ������, ��������������� ����� ������� ������ }
  TNextStream<I: class; O: class> = class(TBufferedStream<O>)
  strict private
    FSource: TBufferedStream<I>;
  strict protected
    property Source: TBufferedStream<I> read FSource;
    function InternalEof: boolean; override;
  public
    constructor Create(ASource: TBufferedStream<I>);
    destructor Destroy; override;
  end;

  { �����, �������������� ��������� ������� �� ������� ������ }
  TPositionStream = class(TNextStream<TChar, TPositionedChar>)
  strict private
    Line, Col: integer;
    Initialized: boolean;
  strict protected
    function InternalNext: TPositionedChar; override;
  end;

implementation

/////////////////////////////////////////////////////////////////////////////////
//                                                                             //
// ����� ��������� ���������� ��������� ������� � ���������� �� ������ � ����� //
//                                                                             //
/////////////////////////////////////////////////////////////////////////////////

destructor TObjectStream<T>.Destroy;
begin
  FreeAndNil(Output);
  inherited;
end;

function TObjectStream<T>.Next: T;
begin
  if Eof then raise Exception.Create('End of the text had been reached');
  Result := InternalNext;
  if not Assigned(Output) then Output := TObjectList<T>.Create(OwnsOutput);
  Output.Add(Result);
end;

function TObjectStream<T>.OwnsOutput: boolean;
begin
  Result := true;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//     ����� ����������� ��������� ����� � ��������� ������� ������������     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TBufferedStream<T>.Eof: boolean;
begin
  Result := InternalEof and (not Assigned(Output) or (RepeatMark >= Output.Count));
end;

function TBufferedStream<T>.Next: T;
begin
  if Assigned(Output) and (RepeatMark < Output.Count)
    then Result := Output[RepeatMark]
    else Result := inherited Next;
  Inc(RepeatMark);
end;

function TBufferedStream<T>.Mark: TMark;
begin
  if Assigned(Output)
    then Result := RepeatMark
    else Result := 0;
end;

procedure TBufferedStream<T>.SaveMark;
begin
  SavedMark := Mark;
end;

procedure TBufferedStream<T>.Restore(AMark: TMark);
begin
  if AMark > Output.Count
    then raise Exception.Create('Invalid stream mark')
    else RepeatMark := AMark;
end;

procedure TBufferedStream<T>.Restore;
begin
  Restore(SavedMark);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                            ����� ������ ������                             //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TStringStream.Create(const AValue: string);
begin
  FValue := AValue;
end;

function TStringStream.InternalEof: boolean;
begin
  Result := FIndex >= Length(FValue);
end;

function TStringStream.InternalNext: TChar;
begin
  Inc(FIndex);
  Result := TChar.Create(FValue[FIndex]);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//         ������� ����� ������, ��������������� ����� ������� ������         //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TNextStream<I, O>.Create(ASource: TBufferedStream<I>);
begin
  FSource := ASource;
end;

destructor TNextStream<I, O>.Destroy;
begin
  FreeAndNil(FSource);
  inherited;
end;

function TNextStream<I, O>.InternalEof: boolean;
begin
  Result := FSource.Eof;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//         �����, �������������� ��������� ������� �� ������� ������          //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function LFtoCR(C: char): char;
begin
  if C = #10 then Result := #13 else Result := C;
end;

function TPositionStream.InternalNext: TPositionedChar;
var
  C: TChar;
begin
  { ��� ������ ������ ������� ������� � ������ ������ � ������� }
  if not Initialized then
  begin
    Line := 1;
    Col  := 1;
    Initialized := true;
  end;
  { ������ ������ � ��������� ��������� }
  C := Source.Next;
  Result := TPositionedChar.Create(LFtoCR(C.Value), Line, Col);
  { ���� ��� �� ������� ������ - �������� ������� ������ }
  if (C.Value <> #13) and (C.Value <> #10) then
    Inc(Col)
  { ���� �������, �� ������� �� ����� ������ }
  else
    begin
      Inc(Line);
      Col := 1;
      { � ���� ��� CR, ��������� LF ����� ���� }
      if (C.Value = #13) and not Source.Eof then
      begin
        Source.SaveMark;
        if Source.Next.Value <> #10 then Source.Restore;
      end;
    end;
end;

end.


