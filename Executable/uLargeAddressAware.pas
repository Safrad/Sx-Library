// The 32 bit application can handle addresses larger than 2 GB.

unit uLargeAddressAware;

interface

implementation

{$ifdef WIN32}
  {$if CompilerVersion >= 18}
  uses
    Windows;
  {$else}
  const
    IMAGE_FILE_LARGE_ADDRESS_AWARE = $20;
  {$endif}

  {$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}
{$endif}

end.
