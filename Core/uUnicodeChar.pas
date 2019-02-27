unit uUnicodeChar;

interface

type
  TUnicodeChar = class
  public
    const
      Permille = Char($2030);

      CheckMark = Char($2713);
      CrossMark = Char($2717);

      // Box Drawings
      LightHorizontal = Char($2500);
      LightVertical = Char($2502);
      LightDownAndRight = Char($250C);
      LightDownAndLeft = Char($2510);
      LightUpAndRight = Char($2514);
      LightUpAndLeft = Char($2518);
      LightVerticalAndRight = Char($251C);
      LightVerticalAndLeft = Char($2524);
      LightDownAndHorizontal = Char($252C);
      LightUpAndHorizontal = Char($2534);
      LightVerticalAndHorizontal = Char($253C);

      HeavyHorizontal = Char($2501);
      HeavyVertical = Char($2503);
      HeavyDownAndRight = Char($250F);
      HeavyDownAndLeft = Char($2513);
      HeavyUpAndRight = Char($2517);
      HeavyUpAndLeft = Char($251B);
      HeavyVerticalAndRight = Char($2523);
      HeavyVerticalAndLeft = Char($252B);
      HeavyDownAndHorizontal = Char($2533);
      HeavyUpAndHorizontal = Char($253B);
      HeavyVerticalAndHorizontal = Char($254B);

      DoubleHorizontal = Char($2550);
      DoubleVertical = Char($2551);
      DoubleDownAndRight = Char($2554);
      DoubleDownAndLeft = Char($2557);
      DoubleUpAndRight = Char($255A);
      DoubleUpAndLeft = Char($255D);
      DoubleVerticalAndRight = Char($2560);
      DoubleVerticalAndLeft = Char($2563);
      DoubleDownAndHorizontal = Char($2566);
      DoubleUpAndHorizontal = Char($2569);
      DoubleVerticalAndHorizontal = Char($256C);

      // Keyboard
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
    //  Mouse = Char($D83D) + Char($DDB0); // $1F5B0
      Mouse = Char($D83D) + Char($DDB1); // $1F5B1
      EMailKey = Char($2709);

      Search = Char($2315);
      Stop = Char($23F9);
      First =Char($23EE);
      Next = Char($23E9);
      Prev = Char($23EA);
      Last = Char($23ED);
      Play = Char($25B6);
      PlayBack = Char($25C4);

      Refresh = Char($21BA);
      Favorites = Char($2606);

      // Control
      Null = Char($2400); // Null character
      SOH = Char($2401); // Start of Header
      STX = Char($2402); // Start of Text
      ETX = Char($2403); // End of Text, hearts card suit
      EOT = Char($2404); // End of Transmission, diamonds card suit
      ENQ = Char($2405); // Enquiry, clubs card suit
      ACK = Char($2406); // Acknowledgement, spade card suit
      BEL = Char($2407); // Bell
      BS = Char($2408); // Backspace
      Backspace = BS;
      HT = Char($2409); // Horizontal Tab
      LF = Char($240A); // Line feed (#10)
      VT = Char($240B); // Vertical Tab, male symbol, symbol for Mars
      FF = Char($240C); // Form feed, female symbol, symbol for Venus
      Formfeed = FF;
      CR = Char($240D); // Carriage return (#13)
      SO = Char($240E); // Shift Out
      SI = Char($240F); // Shift In
      DLE = Char($2410); // Data link escape
      DC1 = Char($2411); // Device control 1
      DC2 = Char($2412); // Device control 2
      DC3 = Char($2413); // Device control 3
      DC4 = Char($2414); // Device control 4
      NAK = Char($2415); // Negatively acknowledge
      SYN = Char($2416); // Synchronous idle
      ETB = Char($2417); // End of trans. block
      CAN = Char($2418); // Cancel
      EM = Char($2419); // End of medium
      SUB = Char($241A); // Substitute
      ESC = Char($241B); // Escape
      FS = Char($241C); // File separator
      GS = Char($241D); // Group separator
      RS = Char($241E); // Record separator
      US = Char($241F); // Unit separator

      // World
      LastQuarterMoon = Char($2639);
      Snowman = Char($2603);

      // Chess
      WhiteChessPawn = Char($2659);
      WhiteChessKnight = Char($2658);
      WhiteChessBishop = Char($2657);
      WhiteChessRook = Char($2656);
      WhiteChessQueen = Char($2655);
      WhiteChessKing = Char($2654);
      BlackChessPawn = Char($265F);
      BlackChessKnight = Char($265E);
      BlackChessBishop = Char($265D);
      BlackChessRook = Char($265C);
      BlackChessQueen = Char($265B);
      BlackChessKing = Char($265A);
  end;

implementation

end.
