unit rpVersionInfo;  //version 1.0 3/8/98 written and tested with Delphi 3.
(*Written by Rick Peterson, this component is released to the public domain for
	any type of use, private or commercial.  Should you enhance the product
	please give me credit and send me a copy.  Also please report any bugs to me.
	Send all coorespondence to rickpet@airmail.net. *)

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	TypInfo;

type
{$M+}
(* Have you seen the $M+ before???This tells delphi to publish RTTI info for
	 enumerated types.  Basically allowing your enumerated types to act as
	 strings with GetEnumName *)
	TVersionType = (vtCompanyName, vtFileDescription, vtFileVersion, vtInternalName,
								vtLegalCopyright, vtLegalTradeMark, vtOriginalFileName,
								vtProductName, vtProductVersion, vtComments);
{$M-}
	TrpVersionInfo = class(TComponent)
(* This component will allow you to get Version Info from your program at
	 RunTime *)
	private
		FVersionInfo : array [0 .. ord(high(TVersionType))] of string;
	protected
		function GetCompanyName: string;
		function GetFileDescription: string;
		function GetFileVersion: string;
		function GetInternalName: string;
		function GetLegalCopyright: string;
		function GetLegalTradeMark: string;
		function GetOriginalFileName: string;
		function GetProductName: string;
		function GetProductVersion: string;
		function GetComments: string;
		function GetVersionInfo(VersionType: TVersionType): string; virtual;
		procedure SetVersionInfo; virtual;
	public
		constructor Create(AOwner: TComponent); override;
	published
(* Label1.Caption := VersionInfo1.FileVersion;  Simple as that.
	 NOTE:  ALL the properties are READONLY so you can not view them with the
	 Object Inspector *)
		property CompanyName: string read GetCompanyName;
		property FileDescription: string read GetFileDescription;
		property FileVersion: string read GetFileVersion;
		property InternalName: string read GetInternalName;
		property LegalCopyright: string read GetLegalCopyright;
		property LegalTradeMark: string read GetLegalTradeMark;
		property OriginalFileName: string read GetOriginalFileName;
		property ProductName: string read GetProductName;
		property ProductVersion: string read GetProductVersion;
		property Comments: string read GetComments;
	end;

implementation

constructor TrpVersionInfo.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	SetVersionInfo;
end;

function TrpVersionInfo.GetCompanyName: string;
begin
	Result := GeTVersionInfo(vtCompanyName);
end;

function TrpVersionInfo.GetFileDescription: string;
begin
	Result := GeTVersionInfo(vtFileDescription);
end;

function TrpVersionInfo.GetFileVersion: string;
begin
	Result := GeTVersionInfo(vtFileVersion);
end;

function TrpVersionInfo.GetInternalName: string;
begin
	Result := GeTVersionInfo(vtInternalName);
end;

function TrpVersionInfo.GetLegalCopyright: string;
begin
	Result := GeTVersionInfo(vtLegalCopyright);
end;

function TrpVersionInfo.GetLegalTradeMark: string;
begin
	Result := GeTVersionInfo(vtLegalTradeMark);
end;

function TrpVersionInfo.GetOriginalFileName: string;
begin
	Result := GeTVersionInfo(vtOriginalFileName);
end;

function TrpVersionInfo.GetProductName: string;
begin
	Result := GeTVersionInfo(vtProductName);
end;

function TrpVersionInfo.GetProductVersion: string;
begin
	Result := GeTVersionInfo(vtProductVersion);
end;

function TrpVersionInfo.GetComments: string;
begin
	Result := GeTVersionInfo(vtComments);
end;

function TrpVersionInfo.GeTVersionInfo(VersionType: TVersionType): string;
begin
	Result := FVersionInfo[ord(VersionType)];
end;

procedure TrpVersionInfo.SeTVersionInfo;
var
	sAppName, sVersionType : string;
	iAppSize, iLenOfValue: Cardinal;
	i: Integer;
	pcBuf: PWideChar;
	pcValue: PChar;
begin
	sAppName := Application.ExeName;
	iAppSize := GetFileVersionInfoSize(PChar(sAppName), iAppSize);
	if iAppSize > 0 then
	begin
		pcBuf := AllocMem(iAppSize);
		if GetFileVersionInfo(PChar(sAppName), 0, iAppSize, pcBuf) then
		for i := 0 to Ord(High(TVersionType)) do
		begin
			sVersionType := GetEnumName(TypeInfo(TVersionType), i);
			sVersionType := Copy(sVersionType, 3, Length(sVersionType));
			if VerQueryValue(pcBuf, PChar('StringFileInfo\040904E4\' + sVersionType),
				Pointer(pcValue), iLenOfValue) then
				FVersionInfo[i] := pcValue;
		end;
		FreeMem(pcBuf, iAppSize);
	end;
end;

end.
