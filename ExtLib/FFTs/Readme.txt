Fast Fourier Transform for arbitrary length complex arrays
==========================================================

The code in this library contains an implementation for Fast
Fourier Transforms on arbitrary length complex data. It is based
on old FORTRAN code, found in a signal processing archive. I
dissected the FORTRAN code, since it worked with normal arrays
and I wanted everything to work with my own complex number 
arrays. Also, I substituted a lot of old written out code with
much more readable procedures.

It behaves like FFT for any series that can be factored in
factors 2, 3, 4, 5, 8, 10. When there are other prime factors in
the series, it will calculate the subseries with DFT.

So the extreme case would be a series composed of one prime
factor, for example a series length of 1021. In this case, the
algorithm works just like DFT (slow), and it would be advisary
to add zeroes to the series to make it a length of e.g. 1024.

Files:
FFTs.pas:     
  Forward FFT transform and inverse FFT transform on complex
  data series
Complexs.pas: 
  Complex number arithmic
Types.pas:    
  Definition of the floating point type (either single or
  double)

Copyright: Nils Haeck M.Sc. (email: n.haeck@simdesign.nl)
For more information visit http://www.simdesign.nl
Original date of publication: 10 Mar 2003

****************************************************************

The contents of these files are subject to the Mozilla Public
License Version 1.1 (the "License"); you may not use this file
except in compliance with the License. You may obtain a copy of
the License at:
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an
"AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
implied. See the License for the specific language governing
rights and limitations under the License.
