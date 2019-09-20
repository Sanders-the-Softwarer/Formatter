////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                    Атрибуты для описания классов проекта                   //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Attributes;

interface

uses System.Rtti;

type
  { Сообщает принтеру о том, что объект поддерживает выравнивание }
  AlignedAttribute = class(TCustomAttribute)
  public
    constructor Create;
  end;

  TCustomAttributeClass = class of TCustomAttribute;

{ Проверка наличия атрибута в классе }
function HasAttribute(AClass: TClass; AttributeClass: TCustomAttributeClass): boolean;

implementation

var
  Context: TRttiContext;

{ Проверка наличия атрибута в классе }
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

