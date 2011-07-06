(*************************************************************************
Cephes Math Library Release 2.8:  June, 2000
Copyright by Stephen L. Moshier

Contributors:
    * Sergey Bochkanov (ALGLIB project). Translation from C to
      pseudocode.

See subroutines comments for additional copyrights.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

- Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer listed
  in this license in the documentation and/or other materials
  provided with the distribution.

- Neither the name of the copyright holders nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*************************************************************************)
unit GammaF;
interface
uses Math, Ap, Sysutils;

function Gamma(x : Extended):Extended;
function LnGamma(x : Extended; var SgnGam : Extended):Extended;

implementation

uses uMath;

function GammaStirF(X : Extended):Extended;forward;


(*************************************************************************
Gamma function

Input parameters:
    X   -   argument

Domain:
    0 < X < 171.6
    -170 < X < 0, X is not an integer.

Relative error:
 arithmetic   domain     # trials      peak         rms
    IEEE    -170,-33      20000       2.3e-15     3.3e-16
    IEEE     -33,  33     20000       9.4e-16     2.2e-16
    IEEE      33, 171.6   20000       2.3e-15     3.2e-16

Cephes Math Library Release 2.8:  June, 2000
Original copyright 1984, 1987, 1989, 1992, 2000 by Stephen L. Moshier
Translated to AlgoPascal by Bochkanov Sergey (2005, 2006, 2007).
*************************************************************************)
function Gamma(x : Extended):Extended;
var
    p : Extended;
    PP : Extended;
    q : Extended;
    QQ : Extended;
    z : Extended;
    i : Integer;
    SgnGam : Extended;
begin
    SgnGam := 1;
    q := AbsReal(x);
    if q>33.0 then
    begin
        if x<0.0 then
        begin
            p := floor(q);
						i := RoundSG(p);
            if i mod 2=0 then
            begin
                SgnGam := -1;
            end;
            z := q-p;
            if z>0.5 then
            begin
                p := p+1;
                z := q-p;
            end;
            z := q*Sin(Pi*z);
            z := AbsReal(z);
            z := Pi/(z*GammaStirF(q));
        end
        else
        begin
            z := GammaStirF(x);
        end;
        Result := SgnGam*z;
        Exit;
    end;
    z := 1;
    while x>=3 do
    begin
        x := x-1;
        z := z*x;
    end;
    while x<0 do
    begin
        if x>-0.000000001 then
        begin
            Result := z/((1+0.5772156649015329*x)*x);
            Exit;
        end;
        z := z/x;
        x := x+1;
    end;
    while x<2 do
    begin
        if x<0.000000001 then
        begin
            Result := z/((1+0.5772156649015329*x)*x);
            Exit;
        end;
        z := z/x;
        x := x+1.0;
    end;
    if x=2 then
    begin
        Result := z;
        Exit;
    end;
    x := x-2.0;
    PP := 1.60119522476751861407E-4;
    PP := 1.19135147006586384913E-3+X*PP;
    PP := 1.04213797561761569935E-2+X*PP;
    PP := 4.76367800457137231464E-2+X*PP;
    PP := 2.07448227648435975150E-1+X*PP;
    PP := 4.94214826801497100753E-1+X*PP;
    PP := 9.99999999999999996796E-1+X*PP;
    QQ := -2.31581873324120129819E-5;
    QQ := 5.39605580493303397842E-4+X*QQ;
    QQ := -4.45641913851797240494E-3+X*QQ;
    QQ := 1.18139785222060435552E-2+X*QQ;
    QQ := 3.58236398605498653373E-2+X*QQ;
    QQ := -2.34591795718243348568E-1+X*QQ;
    QQ := 7.14304917030273074085E-2+X*QQ;
    QQ := 1.00000000000000000320+X*QQ;
    Result := z*PP/QQ;
    Exit;
end;


(*************************************************************************
Natural logarithm of gamma function

Input parameters:
    X       -   argument

Result:
    logarithm of the absolute value of the Gamma(X).

Output parameters:
    SgnGam  -   sign(Gamma(X))

Domain:
    0 < X < 2.55e305
    -2.55e305 < X < 0, X is not an integer.

ACCURACY:
arithmetic      domain        # trials     peak         rms
   IEEE    0, 3                 28000     5.4e-16     1.1e-16
   IEEE    2.718, 2.556e305     40000     3.5e-16     8.3e-17
The error criterion was relative when the function magnitude
was greater than one but absolute when it was less than one.

The following test used the relative error criterion, though
at certain points the relative error could be much higher than
indicated.
   IEEE    -200, -4             10000     4.8e-16     1.3e-16

Cephes Math Library Release 2.8:  June, 2000
Copyright 1984, 1987, 1989, 1992, 2000 by Stephen L. Moshier
Translated to AlgoPascal by Bochkanov Sergey (2005, 2006, 2007).
*************************************************************************)
function LnGamma(x : Extended; var SgnGam : Extended):Extended;
var
    A : Extended;
    B : Extended;
    C : Extended;
    p : Extended;
    q : Extended;
    u : Extended;
    w : Extended;
    z : Extended;
    i : Integer;
    LogPi : Extended;
    LS2PI : Extended;
    Tmp : Extended;
begin
    SgnGam := 1;
    LogPi := 1.14472988584940017414;
    LS2PI := 0.91893853320467274178;
    if x<-34.0 then
    begin
        q := -x;
        w := LnGamma(q, Tmp);
        p := floor(q);
        i := RoundSG(p);
        if i mod 2=0 then
        begin
            SgnGam := -1;
        end
        else
        begin
            SgnGam := 1;
        end;
        z := q-p;
        if z>0.5 then
        begin
            p := p+1;
            z := p-q;
        end;
        z := q*Sin(Pi*z);
        Result := LogPi-Ln(z)-w;
        Exit;
    end;
    if x<13 then
    begin
        z := 1;
        p := 0;
        u := x;
        while u>=3 do
        begin
            p := p-1;
            u := x+p;
            z := z*u;
        end;
        while u<2 do
        begin
            z := z/u;
            p := p+1;
            u := x+p;
        end;
        if z<0 then
        begin
            sgngam := -1;
            z := -z;
        end
        else
        begin
            sgngam := 1;
        end;
        if u=2 then
        begin
            Result := Ln(z);
            Exit;
        end;
        p := p-2;
        x := x+p;
        B := -1378.25152569120859100;
        B := -38801.6315134637840924+X*B;
        B := -331612.992738871184744+X*B;
        B := -1162370.97492762307383+X*B;
        B := -1721737.00820839662146+X*B;
        B := -853555.664245765465627+X*B;
        C := 1;
        C := -351.815701436523470549+X*C;
        C := -17064.2106651881159223+X*C;
        C := -220528.590553854454839+X*C;
        C := -1139334.44367982507207+X*C;
        C := -2532523.07177582951285+X*C;
        C := -2018891.41433532773231+X*C;
        p := x*B/C;
        Result := Ln(z)+p;
        Exit;
    end;
    q := (x-0.5)*Ln(x)-x+LS2PI;
    if x>100000000 then
    begin
        Result := q;
        Exit;
    end;
    p := 1/(x*x);
    if x>=1000.0 then
    begin
        q := q+((7.9365079365079365079365*0.0001*p-2.7777777777777777777778*0.001)*p+0.0833333333333333333333)/x;
    end
    else
    begin
        A := 8.11614167470508450300*0.0001;
        A := -5.95061904284301438324*0.0001+p*A;
        A := 7.93650340457716943945*0.0001+p*A;
        A := -2.77777777730099687205*0.001+p*A;
        A := 8.33333333333331927722*0.01+p*A;
        q := q+A/x;
    end;
    Result := q;
end;


function GammaStirF(X : Extended):Extended;
var
    y : Extended;
    w : Extended;
    v : Extended;
    Stir : Extended;
begin
    w := 1/x;
    Stir := 7.87311395793093628397E-4;
    Stir := -2.29549961613378126380E-4+w*Stir;
    Stir := -2.68132617805781232825E-3+w*Stir;
    Stir := 3.47222221605458667310E-3+w*Stir;
    Stir := 8.33333333333482257126E-2+w*Stir;
    w := 1+w*Stir;
    y := Exp(x);
    if x>143.01608 then
    begin
        v := Power(x, 0.5*x-0.25);
        y := v*(v/y);
    end
    else
    begin
        y := Power(x, x-0.5)/y;
    end;
    Result := 2.50662827463100050242*y*w;
end;


end.