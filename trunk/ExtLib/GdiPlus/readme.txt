GdiPlus.pas Readme File
-----------------------

This package is contains the following:

GdiPlus.pas     - Partial new GDI+ pascal interface 
codebot.inc     - Delphi compiler definitions
readme.txt      - This file

Disclaimer notice:
------------------

This package does NOT represent a complete port of the GDI+ library. My intention in creating this port was to create a simple thread safe GDI+ alternative for working with different image formats based on interfaces rather than classes plus a few extra features. 

Currently I believe I have a full implementation of the IBitmap and IImage interfaces, and partial implementations of IGraphics and IImageAttributes. The implementation is complete enough to allow programmers to create, load, modify, draw, and save images. I have tested what has been implemented thus far and all function it works as expect.

How to get the latest version of this library:
----------------------------------------------

Goto http://codebot.org/gdiplus

Why am I working on another GDI+ port when one already exists? Here are a few reasons:
--------------------------------------------------------------------------------------

* GDI+ classes are now implemented as interfaces which reduces the work for programmers. You now longer need to explicitly free GDI+ objects and you no longer need guard your painting code with try finally blocks.

* These GDI+ Interfaces can reference the same GDI+ object or none via the IGdiPlusBase.Handle property.

* Interfaces participate in a handle reference counting scheme so that if A and B reference the same GDI+ object when releasing interface A, interface B will still reference a valid GDI+ object, but then if B is released the underlying GDI+ object will be destroyed.

* These interfaces now make use of properties for things like Pixels[X, Y], Smoothing, Interpolation etc. rather than only using getters/setters methods (subtle but helpful).

* The interface are implemented privately but can be instantiated through global NewXXX functions. i.e. NewBitmap(FileName), NewBitmap(IStream), NewGraphics(DC), NewCachedBitmap(IBitmap, IGraphics) etc.

* More thread safe code and error checking (interfaces and GDI+ are checked before being called, guarding against  and handling multi-threaded access, initialization only in the main program thread).

* The implementation is contained within a single unit referencing only the Windows, ActiveX pascal files.

* Finally, I follow a strict adherance to coding style and naming guidelines :)

Some Extra Features of Note:
----------------------------

Color matrices and transforms for adjusting image saturation, brightness, hue, and opacity. See the ColorMultiply function to combine these and other image pixel operations in any order. Also see the ColorTransform function to apply several operations into one matrix. Use IImageAttributes and its SetColorMatrix method to apply those transforms to a graphics draw command.

The GetEncoderClsid function is provided help find the appropriate class ids for the Save method of IImage. Use the BmpFormat, GifFormat, PngFormat, JpgFormat, TifFormat mime type constants to pick your encoder. 

If you like this package and want to leave a comment or send me changes:
------------------------------------------------------------------------

Send me an email at sysrpl@gmail.com with GdiPlus in the subject line.

