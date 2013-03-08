program SelfTest;
{$apptype console}

uses
  SysUtils, Classes,
  DCPbase64,
  DCPblockciphers,
  DCPconst,
  DCPcrypt2,
  DCPblowfish,
  DCPcast128,
  DCPcast256,
  DCPdes,
  DCPgost,
  DCPice,
  DCPidea,
  DCPmars,
  DCPmisty1,
  DCPrc2,
  DCPrc4,
  DCPrc5,
  DCPrc6,
  DCPrijndael,
  DCPtea,
  DCPtwofish,
  DCPhaval,
  DCPmd4,
  DCPmd5,
  DCPripemd128,
  DCPripemd160,
  DCPsha1,
  DCPtiger,
  DCPreg,
  DCPserpent,
  DCPsha256,
  DCPsha512;

type
  TDCPHashClass = class of TDCP_hash;
  TDCPCipherClass = class of TDCP_cipher;

procedure TestHash(HashClass: TDCPHashClass);
begin
  if not HashClass.SelfTest then
    Writeln(Format('Self-test failed: %s', [HashClass.GetAlgorithm]));
end;

procedure TestCipher(CipherClass: TDCPCipherClass);
begin
  if not CipherClass.SelfTest then
    Writeln(Format('Self-test failed: %s', [CipherClass.GetAlgorithm]));
end;

begin
  TestHash(TDCP_haval);
  TestHash(TDCP_md4);
  TestHash(TDCP_md5);
  TestHash(TDCP_ripemd128);
  TestHash(TDCP_ripemd160);
  TestHash(TDCP_sha1);
  TestHash(TDCP_sha256);
  TestHash(TDCP_sha384);
  TestHash(TDCP_sha512);
  TestHash(TDCP_tiger);
  TestCipher(TDCP_blowfish);
  TestCipher(TDCP_cast128);
  TestCipher(TDCP_cast256);
  TestCipher(TDCP_des);
  TestCipher(TDCP_3des);
  TestCipher(TDCP_gost);
  TestCipher(TDCP_ice);
  TestCipher(TDCP_thinice);
  TestCipher(TDCP_ice2);
  TestCipher(TDCP_idea);
  TestCipher(TDCP_mars);
  TestCipher(TDCP_misty1);
  TestCipher(TDCP_rc2);
  TestCipher(TDCP_rc4);
  TestCipher(TDCP_rc5);
  TestCipher(TDCP_rc6);
  TestCipher(TDCP_rijndael);
  TestCipher(TDCP_serpent);
  TestCipher(TDCP_tea);
  TestCipher(TDCP_twofish);
  Writeln('Done.');
end.
