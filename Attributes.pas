////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                    �������� ��� �������� ������� �������                   //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Attributes;

interface

uses System.Rtti;

type
  { �������� �������� � ���, ��� ������ ������������ ������������ }
  AlignedAttribute = class(TCustomAttribute)
  public
    constructor Create;
  end;

  TCustomAttributeClass = class of TCustomAttribute;

{ �������� ������� �������� � ������ }
function HasAttribute(AClass: TClass; AttributeClass: TCustomAttributeClass): boolean;

implementation

var
  Context: TRttiContext;

{ �������� ������� �������� � ������ }
function HasAttribute(AClass: TClass; AttributeClass: TCustomAttributeClass): boolean;
var
  RttiType: TRttiType;
  Attrs: TArray<TCustomAttribute>;
Attr: TCustomAttribute;
begin
  Result := false;
  RttiType := Context.GetType(AClass);
  Attrs := RttiType.GetAttributes;
  for Attr in Attrs do
    Result := Result or Attr.InheritsFrom(AttributeClass);
end;

{ AlignedAttribute }

constructor AlignedAttribute.Create;
begin
end;

initialization
  Context := TRttiContext.Create;

end.

