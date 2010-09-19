unit uProjectInfo;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
	TypInfo;

type
	TProjectInfo = (
		piAuthor,
		piComments,
		piCompanyName,
		piFileDescription,
		piFileVersion,
		piFirstRelease,
		piInternalName,
		piLegalCopyright,
		piLegalTradeMark,
		piOriginalFileName,
		piProductName,
		piProductVersion,
		piRelease
	);

	TInfo = class(TComponent)
	private
		FProjectInfo : array [TProjectInfo] of string;
	public
		constructor Create(AOwner: TComponent); override;
		function GetProjectInfo(const ProjectInfo: TProjectInfo): string;
		procedure SetProjectInfo;
	end;

implementation

constructor TInfo.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	SetProjectInfo;
end;

function TInfo.GetProjectInfo(const ProjectInfo: TProjectInfo): string;
begin
	Result := FProjectInfo[ProjectInfo];
end;

procedure TInfo.SetProjectInfo;
var
	sAppName, sVersionType : string;
	iAppSize, iLenOfValue: Cardinal;
	i: TProjectInfo;
	pcBuf: PWideChar;
	pcValue: PChar;
begin
	sAppName := Application.ExeName;
	iAppSize := GetFileVersionInfoSize(PChar(sAppName), iAppSize);
	if iAppSize > 0 then
	begin
		pcBuf := AllocMem(iAppSize);
		if GetFileVersionInfo(PChar(sAppName), 0, iAppSize, pcBuf) then
		for i := Low(TProjectInfo) to High(TProjectInfo) do
		begin
			sVersionType := GetEnumName(TypeInfo(TProjectInfo), Ord(i));
			sVersionType := Copy(sVersionType, 3, Length(sVersionType));
			if VerQueryValue(pcBuf, PChar('StringFileInfo\040904E4\' + sVersionType),
				Pointer(pcValue), iLenOfValue) then
				FProjectInfo[i] := pcValue;
		end;
		FreeMem(pcBuf, iAppSize);
	end;
end;

end.
