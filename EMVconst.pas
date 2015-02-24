unit EMVconst;

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.AnsiStrings,
  Generics.Collections;

type
  TEMVTag = packed record
    Tag: AnsiString;
    Name: string;

    procedure Clear;
  end;

function GetEMVTag(Tag: AnsiString): TEMVTag;

var
  EMVTags: TList<TEMVTag>;

implementation

function EMVTag(Tag: AnsiString; Name: string): TEMVTag;
begin
  Result.Tag := Tag;
  Result.Name := Name;
end;

function GetEMVTag(Tag: AnsiString): TEMVTag;
var
  i: Integer;
begin
  Result.Clear;
  for i := 0 to EMVTags.Count - 1 do
    if EMVTags[i].Tag = Tag then
    begin
      Result := EMVTags[i];
      break;
    end;
end;

{ TEMVTag }

procedure TEMVTag.Clear;
begin
  Tag := '';
  Name := '';
end;

initialization
begin
  EMVTags := TList<TEMVTag>.Create;
  EMVTags.Add(EMVTag(#$42,'Issuer Identification Number (IIN)'));
  EMVTags.Add(EMVTag(#$6F,'File Control Information (FCI) Template'));
  EMVTags.Add(EMVTag(#$A5,'File Control Information (FCI) Proprietary Template'));

end;

end.
