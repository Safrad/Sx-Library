unit uPerftParserTest;

interface

uses
  uTypes,
  SysUtils,
  TestFrameWork;

type
  TPerftParserTest = class(TTestCase)
  private
    procedure TestOne(const AText: string);
  published
    procedure Test;
  end;


implementation

uses
  uPerftParser,

  uStrings;

{ TPerftParserTest }

procedure TPerftParserTest.Test;
begin
  TestOne('Nodes searched: 197281'); // Safrad, Hozovy Sachy
  TestOne('perft: 197281 nodes' + LineSep + '0 secs' + LineSep + '1216402 nps'); // Piranha
  TestOne('Raw nodes for depth 4: 197281' + LineSep + 'Time : 0.00'); // Sjeng
  TestOne('197281 nodes in 3 ms, 49320 knps'); // Critter
  TestOne('197281 nodes' + LineSep + 'took 2 ms'); // Cheng
  TestOne('total moves=197281  time=0.01'); // Crafty
(*
// Stockfish
  TestOne(
    'a2a3: 8457' + LineSep +
    'b2b3: 9345' + LineSep +
    'c2c3: 9272' + LineSep +
    'd2d3: 11959' + LineSep +
    'e2e3: 13134' + LineSep +
    'f2f3: 8457' + LineSep +
    'g2g3: 9345' + LineSep +
    'h2h3: 8457' + LineSep +
    'a2a4: 9329' + LineSep +
    'b2b4: 9332' + LineSep +
    'c2c4: 9744' + LineSep +
    'd2d4: 12435' + LineSep +
    'e2e4: 13160' + LineSep +
    'f2f4: 8929' + LineSep +
    'g2g4: 9328' + LineSep +
    'h2h4: 9329' + LineSep +
    'b1a3: 8885' + LineSep +
    'b1c3: 9755' + LineSep +
    'g1f3: 9748' + LineSep +
    'g1h3: 8881' + LineSep +
    LineSep +
    'Nodes searched: 197281');

  // Firebird
  a2a4 9329/420/20
a2a3 8457/380/20
b2b4 9332/421/20
b2b3 9345/420/20
c2c4 9744/441/20
c2c3 9272/420/20
d2d4 12435/560/20
d2d3 11959/539/20
e2e4 13160/600/20
e2e3 13134/599/20
f2f4 8929/401/20
f2f3 8457/380/20
g2g4 9328/421/20
g2g3 9345/420/20
h2h4 9329/420/20
h2h3 8457/380/20
b1a3 8885/400/20
b1c3 9755/440/20
g1f3 9748/440/20
g1h3 8881/400/20
TOTAL 197281  moves 20  time: 15000 us
*)
end;

procedure TPerftParserTest.TestOne(const AText: string);
begin

end;

end.
