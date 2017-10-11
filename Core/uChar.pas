unit uChar;

interface

const
  // ASCII control characters
  CharNull = #$00; // Null character
  CharSOH = #$01; // Start of Header
  CharSTX = #$02; // Start of Text
  CharETX = #$03; // End of Text, hearts card suit
  CharEOT = #$04; // End of Transmission, diamonds card suit
  CharENQ = #$05; // Enquiry, clubs card suit
  CharACK = #$06; // Acknowledgement, spade card suit
  CharBEL = #$07; // Bell
  CharBS = #$08; // Backspace
  CharBackspace = CharBS;
  CharHT = #$09; // Horizontal Tab
  CharLF = #$0A; // Line feed (#10)
  CharVT = #$0B; // Vertical Tab, male symbol, symbol for Mars
  CharFF = #$0C; // Form feed, female symbol, symbol for Venus
  CharFormfeed = CharFF;
  CharCR = #$0D; // Carriage return (#13)
  CharSO = #$0E; // Shift Out
  CharSI = #$0F; // Shift In
  CharDLE = #$10; // Data link escape
  CharDC1 = #$11; // Device control 1
  CharDC2 = #$12; // Device control 2
  CharDC3 = #$13; // Device control 3
  CharDC4 = #$14; // Device control 4
  CharNAK = #$15; // Negatively acknowledge
  CharSYN = #$16; // Synchronous idle
  CharETB = #$17; // End of trans. block
  CharCAN = #$18; // Cancel
  CharEM = #$19; // End of medium
  CharSUB = #$1A; // Substitute
  CharESC = #$1B; // Escape
  CharFS = #$1C; // File separator
  CharGS = #$1D; // Group separator
  CharRS = #$1E; // Record separator
  CharUS = #$1F; // Unit separator
  CharDEL = #$7F; // Delete

  // ASCII printable characters
  CharSpace = #$20;

  // Extended ASCII characters
  CharUnbrokableSpace = #$A0;
  CharHyphen = #$96; // –
  CharLongHyphen = #$97; // —
  CharLeftPointingDoubleAngleQuotationMark = '«';
  CharRightPointingDoubleAngleQuotationMark = '»'; 
  CharHorizontalEllipsis = {$ifdef UNICODE}Char($2026){$else}#$85{$endif}; // ...
  CharMultiplicationSign = {$ifdef UNICODE}Char($00D7){$else}'x'{$endif}; // × (#$D7) is not supported in Windows-1251!

  // Unicode
  CharLeftArrow = {$ifdef UNICODE}Char($25C4){$else}'<-'{$endif};
  CharRightArrow = {$ifdef UNICODE}Char($25BA){$else}'->'{$endif};

  CharLeftawardsArrow = {$ifdef UNICODE}Char($2190){$else}'<-'{$endif};
  CharUpawardsArrow = {$ifdef UNICODE}Char($2191){$else}'^'{$endif};
  CharRightawardsArrow = {$ifdef UNICODE}Char($2192){$else}'->'{$endif};
  CharDownawardsArrow = {$ifdef UNICODE}Char($2193){$else}'\'{$endif};

  // Box Drawings
  CharLightHorizontal = Char($2500);
  CharLightVertical = Char($2502);
  CharLightDownAndRight = Char($250C);
  CharLightDownAndLeft = Char($2510);
  CharLightUpAndRight = Char($2514);
  CharLightUpAndLeft = Char($2518);
  CharLightVerticalAndRight = Char($251C);
  CharLightVerticalAndLeft = Char($2524);
  CharLightDownAndHorizontal = Char($252C);
  CharLightUpAndHorizontal = Char($2534);
  CharLightVerticalAndHorizontal = Char($253C);

  CharHeavyHorizontal = Char($2501);
  CharHeavyVertical = Char($2503);
  CharHeavyDownAndRight = Char($250F);
  CharHeavyDownAndLeft = Char($2513);
  CharHeavyUpAndRight = Char($2517);
  CharHeavyUpAndLeft = Char($251B);
  CharHeavyVerticalAndRight = Char($2523);
  CharHeavyVerticalAndLeft = Char($252B);
  CharHeavyDownAndHorizontal = Char($2533);
  CharHeavyUpAndHorizontal = Char($253B);
  CharHeavyVerticalAndHorizontal = Char($254B);

  CharDoubleHorizontal = Char($2550);
  CharDoubleVertical = Char($2551);
  CharDoubleDownAndRight = Char($2554);
  CharDoubleDownAndLeft = Char($2557);
  CharDoubleUpAndRight = Char($255A);
  CharDoubleUpAndLeft = Char($255D);
  CharDoubleVerticalAndRight = Char($2560);
  CharDoubleVerticalAndLeft = Char($2563);
  CharDoubleDownAndHorizontal = Char($2566);
  CharDoubleUpAndHorizontal = Char($2569);
  CharDoubleVerticalAndHorizontal = Char($256C);

implementation

end.
