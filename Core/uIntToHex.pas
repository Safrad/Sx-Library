unit uIntToHex;

interface

{$if CompilerVersion < 32}
function IntToHex(Value: Int8): string; overload;
function IntToHex(Value: UInt8): string; overload;
function IntToHex(Value: Int16): string; overload;
function IntToHex(Value: UInt16): string; overload;
function IntToHex(Value: Int32): string; overload;
function IntToHex(Value: UInt32): string; overload;
function IntToHex(Value: Int64): string; overload;
function IntToHex(Value: UInt64): string; overload;
{$endif}

implementation

{$if CompilerVersion < 32}
function IntToHex(Value: Int8): string;
begin
  Result := IntToHex(UInt8(Value), SizeOf(Int8)*2);
end;

function IntToHex(Value: UInt8): string;
begin
  Result := IntToHex(Value, SizeOf(UInt8)*2);
end;

function IntToHex(Value: Int16): string;
begin
  Result := IntToHex(UInt16(Value), SizeOf(Int16)*2);
end;

function IntToHex(Value: UInt16): string;
begin
  Result := IntToHex(Value, SizeOf(UInt16)*2);
end;

function IntToHex(Value: Int32): string;
begin
  Result := IntToHex(UInt32(Value), SizeOf(Int32)*2);
end;

function IntToHex(Value: UInt32): string;
begin
  Result := IntToHex(Value, SizeOf(UInt32)*2);
end;

function IntToHex(Value: Int64): string;
begin
  Result := IntToHex(UInt64(Value), SizeOf(Int64)*2);
end;

function IntToHex(Value: UInt64): string;
begin
  Result := IntToHex(Value, SizeOf(UInt64)*2);
end;
{$endif}

end.
