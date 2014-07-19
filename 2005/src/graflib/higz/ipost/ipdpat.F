*
* $Id: ipdpat.F,v 1.3 2002/12/04 10:55:03 couet Exp $
*
* $Log: ipdpat.F,v $
* Revision 1.3  2002/12/04 10:55:03  couet
* - Line width must be 1 to draw PS patterns
*
* Revision 1.2  1999/07/21 15:48:24  couet
* - Improvements in PS Patterns drawing
*
* Revision 1.1  1999/07/20 14:09:04  couet
* - New version of the pattern polygon filling. It now uses the Pattern
*   functionality provided by PostScript level 2. This allows to have
*   colored pattern and a better drawing of the pattern.
*
*
#include "higz/pilot.h"
*-- Author :    O.Couet   16/07/99
      SUBROUTINE IPDPAT(IPAT,ICOL)
*.===========>
*.
*. Define the pattern IPAT in the current PS file. IPAT can varies from 1 to 25.
*. Together with the pattern, the color (ICOL) in which the pattern has to be   
*. drawn is also required.
*. A pattern is defined in the current PS file only the first time it is used.
*. Some level 2 Postscript functions are used, so on level 1 printers, patterns
*. will not work. This is not a big problem because, as we said, patterns are
*. define only if they are used, so if they are not used a PS level 1 file will
*. not be poluted by level 2 features, and in any case the old patterns (defined
*. by the routine IPPATT) used a lot of memory which made them almost unusable
*. on old level 1 printers. Finally we should say that level 1 devices are
*. becoming very rare. The official PostScript is now level 3 !  
*.
*..==========> (O.Couet)
#include "higz/hilut.inc"
#include "higz/hipost.inc"
      CHARACTER*28, CDEF
      CHARACTER*4, CPAT
*.______________________________________
*
      CPAT = ' P  '
      WRITE (CPAT(3:4),'(I2.2)') IPAT
*
* IPATD is used as a set of bits. If JBIT(IPATD,IPAT).NE.0 the
* pattern number IPAT as already be defined is this file and it
* is not necessary to redefine it. IPATD is set to zero in IPINIT.
* The bit number 26 allows to know if the macro "cs" has already 
* been defined in the current file (see label 200).
*
      IF (JBIT(IPATD,IPAT).NE.0) GOTO 200
*
* Define the Patterns. Line width must be 1
*
      CALL IPLWID(1)
      CALL IPPSTR(' << /PatternType 1 /PaintType 2 /TilingType 1')
*
      GOTO (  1,  2,  3,  4,  5,  6,  7,  8,  9, 10
     +,      11, 12, 13, 14, 15, 16, 17, 18, 19, 20
     +,      21, 22, 23, 24, 25), IPAT
*
  1   CALL IPPSTR(' /BBox [ 0 0 100 100 ]') 
      CALL IPPSTR(' /XStep 98 /YStep 4')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' [1] 0 sd 2 4 m 99 4 l s 1 3 m 98 3 l s')
      CALL IPPSTR(' 2 2 m 99 2 l s 1 1 m 98 1 l s')
      CALL IPPSTR(' gr end } >> [ 4.0 0 0 4.0 0 0 ]')
      GOTO 100
*
  2   CALL IPPSTR(' /BBox [ 0 0 100 100 ]')
      CALL IPPSTR(' /XStep 96 /YStep 4')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' [1 3] 0 sd 2 4 m 98 4 l s 0 3 m 96 3 l s')
      CALL IPPSTR(' 2 2 m 98 2 l s 0 1 m 96 1 l s')
      CALL IPPSTR(' gr end } >> [ 3.0 0 0 3.0 0 0 ]')
      GOTO 100
*
  3   CALL IPPSTR(' /BBox [ 0 0 100 100 ]')
      CALL IPPSTR(' /XStep 96 /YStep 16')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' [1 3] 0 sd 2 13 m 98 13 l s 0 9 m 96 9 l s')
      CALL IPPSTR(' 2 5 m 98 5 l s 0 1 m 96 1 l s')
      CALL IPPSTR(' gr end } >> [ 2.0 0 0 2.0 0 0 ]')
      GOTO 100
*
  4   CALL IPPSTR(' /BBox [ 0 0 100 100 ]')
      CALL IPPSTR(' /XStep 100 /YStep 100')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' 0 0 m 100 100 l s')
      CALL IPPSTR(' gr end } >> [ 0.24 0 0 0.24 0 0 ]')
      GOTO 100
*
  5   CALL IPPSTR(' /BBox [ 0 0 100 100 ]')
      CALL IPPSTR(' /XStep 100 /YStep 100')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' 0 100 m 100 0 l s')
      CALL IPPSTR(' gr end } >> [ 0.24 0 0 0.24 0 0 ]')
      GOTO 100
*
  6   CALL IPPSTR(' /BBox [ 0 0 100 100 ]')
      CALL IPPSTR(' /XStep 100 /YStep 100')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' 50 0 m 50 100 l s')
      CALL IPPSTR(' gr end } >> [ 0.12 0 0 0.12 0 0 ]')
      GOTO 100
*
  7   CALL IPPSTR(' /BBox [ 0 0 100 100 ]')
      CALL IPPSTR(' /XStep 100 /YStep 100')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' 0 50 m 100 50 l s')
      CALL IPPSTR(' gr end } >> [ 0.12 0 0 0.12 0 0 ]')
      GOTO 100
*
  8   CALL IPPSTR(' /BBox [ 0 0 101 101 ]')
      CALL IPPSTR(' /XStep 100 /YStep 100')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' 0 0 m 0 30 l 30 0 l f 0 70 m 0 100 l 30 100 l f')
      CALL IPPSTR(' 70 100 m 100 100 l 100 70 l f 70 0 m 100 0 l')
      CALL IPPSTR(' 100 30 l f 50 20 m 20 50 l 50 80 l 80 50 l f')
      CALL IPPSTR(' 50 80 m 30 100 l s 20 50 m 0 30 l s 50 20 m')
      CALL IPPSTR(' 70 0 l s 80 50 m 100 70 l s')
      CALL IPPSTR(' gr end } >> [ 0.24 0 0 0.24 0 0 ]')
      GOTO 100
*
  9   CALL IPPSTR(' /BBox [ 0 0 100 100 ]')
      CALL IPPSTR(' /XStep 100 /YStep 100')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' 0 50 m 50 50 50 180 360 arc')
      CALL IPPSTR(' 0 50 m 0 100 50 270 360 arc')
      CALL IPPSTR(' 50 100 m 100 100 50 180 270 arc s')
      CALL IPPSTR(' gr end } >> [ 0.24 0 0 0.24 0 0 ]')
      GOTO 100
*
 10   CALL IPPSTR(' /BBox [ 0 0 100 100 ]')
      CALL IPPSTR(' /XStep 100 /YStep 100')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' 0 50 m 100 50 l 1 1 m 100 1 l')
      CALL IPPSTR(' 0 0 m 0 50 l 100 0 m 100 50 l')
      CALL IPPSTR(' 50 50 m 50 100 l s')
      CALL IPPSTR(' gr end } >> [ 0.24 0 0 0.24 0 0 ]')
      GOTO 100
*
 11   CALL IPPSTR(' /BBox [ 0 0 100 100 ]')
      CALL IPPSTR(' /XStep 100 /YStep 100')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' 0 0 m 0 20 l 50 0 m 50 20 l')
      CALL IPPSTR(' 100 0 m 100 20 l 0 80 m 0 100 l')
      CALL IPPSTR(' 50 80 m 50 100 l 100 80 m 100 100 l')
      CALL IPPSTR(' 25 30 m 25 70 l 75 30 m 75 70 l')
      CALL IPPSTR(' 0 100 m 20 85 l 50 100 m 30 85 l')
      CALL IPPSTR(' 50 100 m 70 85 l 100 100 m 80 85 l')
      CALL IPPSTR(' 0 0 m 20 15 l 50 0 m 30 15 l')
      CALL IPPSTR(' 50 0 m 70 15 l 100 0 m 80 15 l')
      CALL IPPSTR(' 5 35 m 45 65 l 5 65 m 45 35 l')
      CALL IPPSTR(' 55 35 m 95 65 l 55 65 m 95 35 l s')
      CALL IPPSTR(' gr end } >> [ 0.5 0 0 0.5 0 0 ]')
      GOTO 100
*
 12   CALL IPPSTR(' /BBox [ 0 0 100 100 ]')
      CALL IPPSTR(' /XStep 100 /YStep 100')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' 0 80 m 0 100 20 270 360 arc')
      CALL IPPSTR(' 30 100 m 50 100 20 180 360 arc')
      CALL IPPSTR(' 80 100 m 100 100 20 180 270 arc')
      CALL IPPSTR(' 20 0 m 0 0 20 0 90 arc')
      CALL IPPSTR(' 70 0 m 50 0 20 0 180 arc')
      CALL IPPSTR(' 100 20 m 100 0 20 90 180 arc')
      CALL IPPSTR(' 45 50 m 25 50 20 0 360 arc')
      CALL IPPSTR(' 95 50 m 75 50 20 0 360 arc s')
      CALL IPPSTR(' gr end } >> [ 0.5 0 0 0.5 0 0 ]')
      GOTO 100
*
 13   CALL IPPSTR(' /BBox [ 0 0 100 100 ]')
      CALL IPPSTR(' /XStep 100 /YStep 100')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' 0 0 m 100 100 l 0 100 m 100 0 l s')
      CALL IPPSTR(' gr end } >> [ 0.24 0 0 0.24 0 0 ]')
      GOTO 100
*
 14   CALL IPPSTR(' /BBox [ 0 0 100 100 ]')
      CALL IPPSTR(' /XStep 80 /YStep 80')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' 0 20 m 100 20 l 20 0 m 20 100 l')
      CALL IPPSTR(' 0 80 m 100 80 l 80 0 m 80 100 l')
      CALL IPPSTR(' 20 40 m 60 40 l 60 20 m 60 60 l')
      CALL IPPSTR(' 40 40 m 40 80 l 40 60 m 80 60 l s')
      CALL IPPSTR(' gr end } >> [ 0.60 0 0 0.60 0 0 ]')
      GOTO 100
*
 15   CALL IPPSTR(' /BBox [ 0 0 60 60 ]')
      CALL IPPSTR(' /XStep 60 /YStep 60')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' 0 55 m 0 60 5 270 360 arc')
      CALL IPPSTR(' 25 60 m 30 60 5 180 360 arc')
      CALL IPPSTR(' 55 60 m 60 60 5 180 270 arc')
      CALL IPPSTR(' 20 30 m 15 30 5 0 360 arc')
      CALL IPPSTR(' 50 30 m 45 30 5 0 360')
      CALL IPPSTR(' arc 5 0 m 0 0 5 0 90 arc')
      CALL IPPSTR(' 35 0 m 30 0 5 0 180 arc')
      CALL IPPSTR(' 60 5 m 60 0 5 90 180 arc s')
      CALL IPPSTR(' gr end } >> [ 0.41 0 0 0.41 0 0 ]')
      GOTO 100
*
 16   CALL IPPSTR(' /BBox [ 0 0 100 100 ]')
      CALL IPPSTR(' /XStep 100 /YStep 100')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' 50 50 m 25 50 25 0 180 arc s')
      CALL IPPSTR(' 50 50 m 75 50 25 180 360 arc s')
      CALL IPPSTR(' gr end } >> [ 0.4 0 0 0.2 0 0 ]')
      GOTO 100
*
 17   CALL IPPSTR(' /BBox [ 0 0 100 100 ]')
      CALL IPPSTR(' /XStep 100 /YStep 100')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' [24] 0 setdash 0 0 m 100 100 l s')
      CALL IPPSTR(' gr end } >> [ 0.24 0 0 0.24 0 0 ]')
      GOTO 100
*
 18   CALL IPPSTR(' /BBox [ 0 0 100 100 ]')
      CALL IPPSTR(' /XStep 100 /YStep 100')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' [24] 0 setdash 0 100 m 100 0 l s')
      CALL IPPSTR(' gr end } >> [ 0.24 0 0 0.24 0 0 ]')
      GOTO 100
*
 19   CALL IPPSTR(' /BBox [ 0 0 100 100 ]')
      CALL IPPSTR(' /XStep 100 /YStep 100')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' 90 50 m 50 50 40 0 360 arc')
      CALL IPPSTR(' 0 50 m 0 100 50 270 360 arc')
      CALL IPPSTR(' 50 0 m 0 0 50 0 90 arc')
      CALL IPPSTR(' 100 50 m 100 0 50 90 180 arc')
      CALL IPPSTR(' 50 100 m 100 100 50 180 270 arc s')
      CALL IPPSTR(' gr end } >> [ 0.47 0 0 0.47 0 0 ]')
      GOTO 100
*
 20   CALL IPPSTR(' /BBox [ 0 0 100 100 ]')
      CALL IPPSTR(' /XStep 100 /YStep 100')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' 50 50 m 50 75 25 270 450 arc s')
      CALL IPPSTR(' 50 50 m 50 25 25 90  270 arc s')
      CALL IPPSTR(' gr end } >> [ 0.2 0 0 0.4 0 0 ]')
      GOTO 100
*
 21   CALL IPPSTR(' /BBox [ 0 0 101 101 ]')
      CALL IPPSTR(' /XStep 100 /YStep 100')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' 1 1 m 25 1 l 25 25 l 50 25 l 50 50 l')
      CALL IPPSTR(' 75 50 l 75 75 l 100 75 l 100 100 l')
      CALL IPPSTR(' 50 1 m 75 1 l 75 25 l 100 25 l 100 50 l')
      CALL IPPSTR(' 0 50 m 25 50 l 25 75 l 50 75 l 50 100 l s')
      CALL IPPSTR(' gr end } >> [ 0.5 0 0 0.5 0 0 ]')
      GOTO 100
*
 22   CALL IPPSTR(' /BBox [ 0 0 101 101 ]')
      CALL IPPSTR(' /XStep 100 /YStep 100')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' 1 100 m 25 100 l 25 75 l 50 75 l 50 50 l')
      CALL IPPSTR(' 75 50 l 75 25 l 100 25 l 100 1 l')
      CALL IPPSTR(' 50 100 m 75 100 l 75 75 l 100 75 l 100 50 l')
      CALL IPPSTR(' 0 50 m 25 50 l 25 25 l 50 25 l 50 1 l s')
      CALL IPPSTR(' gr end } >> [ 0.5 0 0 0.5 0 0 ]')
      GOTO 100
*
 23   CALL IPPSTR(' /BBox [ 0 0 100 100 ]')
      CALL IPPSTR(' /XStep 100 /YStep 100')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' [1 7] 0 sd 0 8 50 { dup dup m 2 mul 0 l s } for')
      CALL IPPSTR(' 0 8 50 { dup dup 2 mul 100 m 50 add exch 50')
      CALL IPPSTR(' add l s } for 100 0 m 100 100 l 50 50 l f')
      CALL IPPSTR(' gr end } >> [ 0.24 0 0 0.24 0 0 ]')
      GOTO 100
*
 24   CALL IPPSTR(' /BBox [ 0 0 100 100 ]') 
      CALL IPPSTR(' /XStep 100 /YStep 100')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' 100 100 m 100 36 l 88 36 l 88 88 l f')
      CALL IPPSTR(' 100 0 m 100 12 l 56 12 l 50 0 l f')
      CALL IPPSTR(' 0 0 m 48 0 l 48 48 l 50 48 l 56 60 l')
      CALL IPPSTR(' 36 60 l 36 12 l 0 12 l f [1 7] 0 sd')
      CALL IPPSTR(' 61 8 87 { dup dup dup 12 exch m 88 exch l s')
      CALL IPPSTR(' 16 exch 4 sub m 88 exch 4 sub l s } for')
      CALL IPPSTR(' 13 8 35 { dup dup dup 0 exch m 36 exch l s')
      CALL IPPSTR(' 4 exch 4 sub m 36 exch 4 sub l s } for')
      CALL IPPSTR(' 37 8 59 { dup dup dup 12 exch m 36 exch l s')
      CALL IPPSTR(' 16 exch 4 sub m 36 exch 4 sub l s } for')
      CALL IPPSTR(' 13 8 60 { dup dup dup 56 exch m 100 exch l s')
      CALL IPPSTR(' 60 exch 4 sub m 100 exch 4 sub l s } for')
      CALL IPPSTR(' gr end } >> [ 0.5 0 0 0.5 0 0 ]')
      GOTO 100
*
 25   CALL IPPSTR(' /BBox [ 0 0 101 101 ]')
      CALL IPPSTR(' /XStep 100 /YStep 100')
      CALL IPPSTR(' /PaintProc { begin gsave')
      CALL IPPSTR(' 0 0 m 30 30 l 70 30 l 70 70 l 100 100 l 100 0 l')
      CALL IPPSTR(' f 30 30 m 30 70 l 70 70 l f')
      CALL IPPSTR(' gr end } >> [ 0.5 0 0 0.5 0 0 ]')
*
 100  CDEF = ' makepattern /    exch def'
      CDEF(15:17) = CPAT(2:4)
      CALL IPPSTF(26,CDEF(1:26))
      CALL SBIT1(IPATD,IPAT)
*
* Activate the pattern.
*
 200  IF (JBIT(IPATD,26).EQ.0) THEN
         CALL IPPSTR(' /cs {[/Pattern /DeviceRGB] setcolorspace} def')
         CALL IPPSTR(' /FA {f [/DeviceRGB] setcolorspace} def')
         CALL SBIT1(IPATD,26)
      ENDIF
      CALL IPPSTF(3,' cs')
      I = ICOL+1
      IF (I.LT.1) I=1
      IF (I.GT.NBCLUT) I=NBCLUT
      CALL IPFOUT(REDLUT(I))
      CALL IPFOUT(GRNLUT(I))
      CALL IPFOUT(BLULUT(I))
      CALL IPPSTF(4,CPAT)
      CALL IPPSTF(9,' setcolor')
*
      END
