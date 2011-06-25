// * File:     Lib\GUI\efELoop.inc
// * Created:  2008-06-03
// * Modified: 2006-03-21
// * Version:  1.1.45.113
// * Author:   David Safranek (Safrad)
// * E-Mail:   safrad at email.cz
// * Web:      http://safrad.own.cz

		add esi, BPP
		add edi, BPP
		cmp edi, ecx
		jb @Loop

