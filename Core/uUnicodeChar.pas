unit uUnicodeChar;

interface

const
  CharCheckMark = Char($2713);
  CharCrossMark = Char($2717);


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

  AppleCommandKey = Char($2318);
  OptionKey = Char($2325);
  BackspaceKey = Char($232B);
  DelKey = Char($2421);
  ForwardDeleteKey = Char($2326);
  ControlKey = Char($2303);
  EscapeKey = Char($238B);
  AlternativeEscapeKey = Char($241B);
  EnterKey = Char($23CE);
  AlternativeEnterKey = Char($21A9);
  UpArrow = Char($2191);
  DownArrow = Char($2193);
  LeftArrow = Char($2190);
  RightArrow = Char($2192);
  TabKey = Char($21E5);
  ShiftKey = Char($21E7);
  CapsLockKey = Char($21EA);
  EjectKey = Char($21EA);
  PowerKey = Char($233D);
  SleepKey = Char($23FE); // Unicode 9.0
  SpaceKey = Char($2423);

  BeamedEightNotes = Char($266B);
  WindowsLogoKey = Char($229E);
  MenuKey = Char($2263);
//  MouseChar = Char($D83D) + Char($DDB0); // $1F5B0
  MouseChar = Char($D83D) + Char($DDB1); // $1F5B1
  EMailKey = Char($2709);
  SnowmanChar = Char($2603);

  SearchChar = Char($2315);
  StopChar = Char($23F9);
  FirstChar =Char($23EE);
  NextChar = Char($23E9);
  PrevChar = Char($23EA);
  LastChar = Char($23ED);
  PlayChar = Char($25B6);
  PlayBack = Char($25C4);

  RefreshChar = Char($21BA);
  FavoritesChar = Char($2606);

  LastQuarterMoonChar = Char($2639);

  // Control characters
  CharNull = #$2400; // Null character
  CharSOH = #$2401; // Start of Header
  CharSTX = #$2402; // Start of Text
  CharETX = #$2403; // End of Text, hearts card suit
  CharEOT = #$2404; // End of Transmission, diamonds card suit
  CharENQ = #$2405; // Enquiry, clubs card suit
  CharACK = #$2406; // Acknowledgement, spade card suit
  CharBEL = #$2407; // Bell
  CharBS = #$2408; // Backspace
  CharBackspace = CharBS;
  CharHT = #$2409; // Horizontal Tab
  CharLF = #$240A; // Line feed (#10)
  CharVT = #$240B; // Vertical Tab, male symbol, symbol for Mars
  CharFF = #$240C; // Form feed, female symbol, symbol for Venus
  CharFormfeed = CharFF;
  CharCR = #$240D; // Carriage return (#13)
  CharSO = #$240E; // Shift Out
  CharSI = #$240F; // Shift In
  CharDLE = #$2410; // Data link escape
  CharDC1 = #$2411; // Device control 1
  CharDC2 = #$2412; // Device control 2
  CharDC3 = #$2413; // Device control 3
  CharDC4 = #$2414; // Device control 4
  CharNAK = #$2415; // Negatively acknowledge
  CharSYN = #$2416; // Synchronous idle
  CharETB = #$2417; // End of trans. block
  CharCAN = #$2418; // Cancel
  CharEM = #$2419; // End of medium
  CharSUB = #$241A; // Substitute
  CharESC = #$241B; // Escape
  CharFS = #$241C; // File separator
  CharGS = #$241D; // Group separator
  CharRS = #$241E; // Record separator
  CharUS = #$241F; // Unit separator

implementation

end.
