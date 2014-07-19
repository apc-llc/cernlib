*
* $Id: ipdef.F,v 1.7 2005/10/28 12:45:45 couet Exp $
*
* $Log: ipdef.F,v $
* Revision 1.7  2005/10/28 12:45:45  couet
* - Mods to make CUPS version 1.1.22 happy
*
* Revision 1.6  2005/10/28 12:38:13  couet
* - SVG driver, workstation type -779
*
* Revision 1.5  2003/03/20 09:14:10  couet
* - Enclose the PS code between %%EndProlog and the fist %%Page in %%BeginSetup
*   and %EndSetup. That way the CUPS printing package is happy !
*
* Revision 1.4  2003/03/18 13:37:01  couet
* - %%Page 1 moved just after %%EndProlog to be compliant with CUPS
*   printing software which seems to become the standard on Linux.
*
* Revision 1.3  2000/04/03 14:53:14  couet
* - "newpath" has been added at the beginning of the PS and EPS file to close
*   some graphics path which may have been left opened by the application in
*   which the HIGZ PS or EPS file is included.
*
* Revision 1.2  1999/07/20 14:09:03  couet
* - New version of the pattern polygon filling. It now uses the Pattern
*   functionality provided by PostScript level 2. This allows to have
*   colored pattern and a better drawing of the pattern.
*
* Revision 1.1.1.1  1996/02/14 13:11:06  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/02 07/06/95  17.36.12  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IPDEF
*.===========>
*.
*.  This routine initialise the following PostScript procedures:
*.
*. +------------+------------------+-----------------------------------+
*. | Macro Name | Input parameters |            Explanation            |
*. +------------+------------------+-----------------------------------+
*. |     l      | x y              | Draw a line to the x y position   |
*. +------------+------------------+-----------------------------------+
*. |     m      | x y              | Move to the position x y          |
*. +------------+------------------+-----------------------------------+
*. |     box    | dx dy x y        | Define a box                      |
*. +------------+------------------+-----------------------------------+
*. |     bl     | dx dy x y        | Draw a line box                   |
*. +------------+------------------+-----------------------------------+
*. |     bf     | dx dy x y        | Draw a filled box                 |
*. +------------+------------------+-----------------------------------+
*. |     sw     | text             | Return string width of text       |
*. +------------+------------------+-----------------------------------+
*. |     t      | x y              | Translate                         |
*. +------------+------------------+-----------------------------------+
*. |     r      | angle            | Rotate                            |
*. +------------+------------------+-----------------------------------+
*. |     rl     | i j              | Roll the stack                    |
*. +------------+------------------+-----------------------------------+
*. |     d      | x y              | Draw a relative line to x y       |
*. +------------+------------------+-----------------------------------+
*. |     X      | x                | Draw a relative line to x (y=0)   |
*. +------------+------------------+-----------------------------------+
*. |     Y      | y                | Draw a relative line to y (x=0)   |
*. +------------+------------------+-----------------------------------+
*. |     rm     | x y              | Move relatively to x y            |
*. +------------+------------------+-----------------------------------+
*. |     gr     |                  | Restore the graphic context       |
*. +------------+------------------+-----------------------------------+
*. |     lw     | lwidth           | Set line width to lwidth          |
*. +------------+------------------+-----------------------------------+
*. |     sd     | [] 0             | Set dash line define by []        |
*. +------------+------------------+-----------------------------------+
*. |     s      |                  | Stroke mode                       |
*. +------------+------------------+-----------------------------------+
*. |     c      | r g b            | Set rgb color to r g b            |
*. +------------+------------------+-----------------------------------+
*. |     cl     |                  | Close path                        |
*. +------------+------------------+-----------------------------------+
*. |     f      |                  | Fill the last describe path       |
*. +------------+------------------+-----------------------------------+
*. |     mXX    | x y              | Draw the marker type XX at (x,y)  |
*. +------------+------------------+-----------------------------------+
*. |     Zone   | ix iy            | Define the current zone           |
*. +------------+------------------+-----------------------------------+
*. |     black  |                  | The color is black                |
*. +------------+------------------+-----------------------------------+
*. |     C      | dx dy x y        | Clipping on                       |
*. +------------+------------------+-----------------------------------+
*. |     NC     |                  | Clipping off                      |
*. +------------+------------------+-----------------------------------+
*. |     R      |                  | repeat                            |
*. +------------+------------------+-----------------------------------+
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#include "higz/hipost.inc"
#include "higz/hiflag.inc"
      CHARACTER*80 CHPS1,CHPS2
#include "higz/hivers.inc"
*.______________________________________
*
#include "higz/icmtop.inc"
      NPAGES=1
*
* Mode is last digit of Postscript Workstation type
*     mode=1,2 for portrait/landscape black and white
*     mode=3 for Encapsulated PostScript File
*     mode=4 for portrait colour
*     mode=5 for lanscape colour
*
      MODE=MOD(ABS(IPSWTY),10)
      IF(MODE.LE.0.OR.MODE.GT.5)GOTO 20
*
* NX (NY) is the total number of windows in x (y)
*
      NX=MOD(ABS(IPSWTY),1000)/100
      NY=MOD(ABS(IPSWTY),100)/10
      IF(NX.LE.0.OR.NY.LE.0)GOTO 20
      IXZ=1
      IYZ=1
*
* IFMT = 0-99 is the European page format (A4,A3 ...)
* IFMT = 100 is the US format  8.5x11.0 inch
* IFMT = 200 is the US format  8.5x14.0 inch
* IFMT = 300 is the US format 11.0x17.0 inch
*
      IFMT=ABS(IPSWTY/1000)
      IF(IFMT.EQ.0)IFMT=4
      IF(IFMT.EQ.99)IFMT=0
*
      IF(TFLAG)GOTO 10
*
      IF(VFLAG)THEN
         INQUIRE(UNIT=LUNPS,NAME=CHPS2)
         ILEN=MIN(LENOCC(CHPS2),60)
         CALL IPPSTR('<title>@')
         CALL IPPSTR(CHPS2(1:ILEN))
         CALL IPPSTR('</title>@')
         CALL IPPSTR('<desc>@')
         CALL IPPSTF(21,'Creator: HIGZ Version')
         CALL IPPSTR(CHVERS)
         CALL IPPSTR('@')
         CALL IPPSTF(14,'Creation date:')
         CALL IGDATE(CHPS1)
         CHPS2=' '
         CHPS2(2:)=CHPS1(1:LENOCC(CHPS1))
         CALL IPPSTR(CHPS2)
         CALL IPPSTR('@')
         CALL IPPSTR('</desc>@')
         CALL IPMARK
         CALL IPPSTR('<defs>@')
         CALL IPPSTR('</defs>@')
         GOTO 10
      ENDIF
*
      CHPS1='%%Title: '
      INQUIRE(UNIT=LUNPS,NAME=CHPS2)
      CHPS1(10:)=CHPS2(1:LENOCC(CHPS2))
      ILEN=MIN(LENOCC(CHPS1),60)
      CALL IPPSTR(CHPS1(1:ILEN))
      IF(MODE.NE.3)THEN
         IF(MODE.EQ.1.OR.MODE.EQ.4)CALL IPPSTF(10,' (Portrait')
         IF(MODE.EQ.2.OR.MODE.EQ.5)CALL IPPSTF(11,' (Landscape')
         IF(IFMT.LE.99)THEN
            CALL IPPSTF(2,' A')
            CALL IPIOUT(IFMT)
            CALL IPPSTF(1,')')
         ELSE
            IF(IFMT.EQ.100)CALL IPPSTF(8,' Letter)')
            IF(IFMT.EQ.200)CALL IPPSTF(7,' Legal)')
            IF(IFMT.EQ.300)CALL IPPSTF(8,' Ledger)')
         ENDIF
         CALL IPPSTR('@')
         CALL IPPSTR('%%Pages: (atend)@')
      ELSE
         CALL IPPSTR('@')
      ENDIF
*
      CALL IPPSTF(23,'%%Creator: HIGZ Version')
      CALL IPPSTR(CHVERS)
      CALL IPPSTR('@')
*
      CALL IPPSTF(15,'%%CreationDate:')
      CALL IGDATE(CHPS1)
      CHPS2=' '
      CHPS2(2:)=CHPS1(1:LENOCC(CHPS1))
      CALL IPPSTR(CHPS2)
      CALL IPPSTR('@')
      CALL IPPSTR('%%EndComments@')
      CALL IPPSTR('%%BeginProlog@')
*
      IF(MODE.EQ.3)CALL IPPSTR('80 dict begin@')
*
*              Initialization of PostScript procedures
*
      CALL IPPSTR('/s {stroke} def /l {lineto} def /m {moveto} def /t {t
     +ranslate} def@')
      CALL IPPSTR('/sw {stringwidth} def /r {rotate} def /rl {roll}  def
     + /R {repeat} def@')
      CALL IPPSTR('/d {rlineto} def /rm {rmoveto} def /gr {grestore} def
     + /f {eofill} def@')
      CALL IPPSTR('/c {setrgbcolor} def /lw {setlinewidth} def /sd {setd
     +ash} def@')
      CALL IPPSTR('/cl {closepath} def /sf {scalefont setfont} def /blac
     +k {0 setgray} def@')
      CALL IPPSTR('/box {m dup 0 exch d exch 0 d 0 exch neg d cl} def@')
      CALL IPPSTR('/NC{systemdict begin initclip end}def/C{NC box clip n
     +ewpath}def@')
      CALL IPPSTR('/bl {box s} def /bf {box f} def /Y { 0 exch d} def /X
     + { 0 d} def @')
*
      CALL IPMARK
      CALL IPFON
      CALL IPSPEC
*
*     mode=1 for portrait black/white
*
      IF (MODE.EQ.1) THEN
         RPXMIN = 0.7
         RPYMIN = SQRT(2.)*RPXMIN
         IF (IFMT.EQ.100) THEN
            WIDTH  = (8.5*2.54)-2.*RPXMIN
            HEIGTH = (11.*2.54)-2.*RPYMIN
         ELSEIF (IFMT.EQ.200) THEN
            WIDTH  = (8.5*2.54)-2.*RPXMIN
            HEIGTH = (14.*2.54)-2.*RPYMIN
         ELSEIF (IFMT.EQ.300) THEN
            WIDTH  = (11.*2.54)-2.*RPXMIN
            HEIGTH = (17.*2.54)-2.*RPYMIN
         ELSE
            WIDTH  = 21.0-2.*RPXMIN
            HEIGTH = 29.7-2.*RPYMIN
         ENDIF
      ENDIF
*
*     mode=2 for landscape black/white
*
      IF (MODE.EQ.2) THEN
         RPYMIN = 0.7
         RPXMIN = SQRT(2.)*RPYMIN
         IF (IFMT.EQ.100) THEN
            WIDTH  = (11.*2.54)-2.*RPXMIN
            HEIGTH = (8.5*2.54)-2.*RPYMIN
         ELSEIF (IFMT.EQ.200) THEN
            WIDTH  = (14.*2.54)-2.*RPXMIN
            HEIGTH = (8.5*2.54)-2.*RPYMIN
         ELSEIF (IFMT.EQ.300) THEN
            WIDTH  = (17.*2.54)-2.*RPXMIN
            HEIGTH = (11.*2.54)-2.*RPYMIN
         ELSE
            WIDTH  = 29.7-2.*RPXMIN
            HEIGTH = 21-2.*RPYMIN
         ENDIF
      ENDIF
*
*     mode=3 encapsulated PostScript
*
   10 IF (MODE.EQ.3) THEN
         WIDTH  = X2W
         HEIGTH = Y2W
         IFMT   = 4
         NX     = 1
         NY     = 1
      ENDIF
*
*     mode=4 for portrait colour
*
      IF (MODE.EQ.4) THEN
         RPXMIN = 0.7
         RPYMIN = 3.4
         IF (IFMT.EQ.100) THEN
            WIDTH  = (8.5*2.54)-2.*RPXMIN
            HEIGTH = (11.*2.54)-2.*RPYMIN
         ELSEIF (IFMT.EQ.200) THEN
            WIDTH  = (8.5*2.54)-2.*RPXMIN
            HEIGTH = (14.*2.54)-2.*RPYMIN
         ELSEIF (IFMT.EQ.300) THEN
            WIDTH  = (11.*2.54)-2.*RPXMIN
            HEIGTH = (17.*2.54)-2.*RPYMIN
         ELSE
            WIDTH  = (21.0-2*RPXMIN)
            HEIGTH = (29.7-2.*RPYMIN)
         ENDIF
      ENDIF
*
*     mode=5 for lanscape colour
*
      IF (MODE.EQ.5) THEN
         RPXMIN = 3.4
         RPYMIN = 0.7
         IF (IFMT.EQ.100) THEN
            WIDTH  = (11.*2.54)-2.*RPXMIN
            HEIGTH = (8.5*2.54)-2.*RPYMIN
         ELSEIF (IFMT.EQ.200) THEN
            WIDTH  = (14.*2.54)-2.*RPXMIN
            HEIGTH = (8.5*2.54)-2.*RPYMIN
         ELSEIF (IFMT.EQ.300) THEN
            WIDTH  = (17.*2.54)-2.*RPXMIN
            HEIGTH = (11.*2.54)-2.*RPYMIN
         ELSE
            WIDTH  = (29.7-2*RPXMIN)
            HEIGTH = (21-2.*RPYMIN)
         ENDIF
      ENDIF
*
      IF (IFMT.LT.100) THEN
         VALUE = 21.*SQRT(2.)**(4-IFMT)
      ELSEIF (IFMT.EQ.100) THEN
         VALUE = 8.5*2.54
      ELSEIF (IFMT.EQ.200) THEN
         VALUE=8.5*2.54
      ELSEIF (IFMT.EQ.300) THEN
         VALUE = 11.*2.54
      ENDIF
*
      IF (IFMT.GE.100) IFMT = 4
*
* Compute size (in points) of the window for each picture = f(NX,NY)
*
      IPSI         = IGIWIN(IPSWID)
      XWKSIZ(IPSI) = WIDTH/FLOAT(NX)*SQRT(2.)**(4-IFMT)
      YWKSIZ(IPSI) = HEIGTH/FLOAT(NY)*SQRT(2.)**(4-IFMT)
      WKMAX(IPSI)  = FLOAT(MAX(ICMTOP(XWKSIZ(IPSI))
     +,                    ICMTOP(YWKSIZ(IPSI))))
      NPX          = 4*ICMTOP(XWKSIZ(IPSI))
      NPY          = 4*ICMTOP(YWKSIZ(IPSI))
*
      IF (TFLAG.OR.VFLAG) RETURN
*
* Procedure Zone
*
      IF (MODE.NE.3) THEN
         CALL IPPSTF(33,'/Zone {/iy exch def /ix exch def ')
         CALL IPPSTF(10,' ix 1 sub ')
         CALL IPIOUT(NPX)
         CALL IPPSTF(5,' mul ')
         CALL IPFOUT(FLOAT(NY))
         CALL IPPSTF(8,' iy sub ')
         CALL IPIOUT(NPY)
         CALL IPPSTR(' mul t} def@')
      ENDIF
*
      CALL IPPSTR('%%EndProlog@')
      IF (MODE.NE.3) CALL IPPSTR('%%Page: number 1@')
*
      CALL IPPSTR('%%BeginSetup@')
      CALL IPPSTR('%%EndSetup@')
      CALL IPPSTF(8,'newpath ')
      CALL IPSVRT(1)
      IF (MODE.EQ.1.OR.MODE.EQ.4) THEN
         CALL IPIOUT(ICMTOP(RPXMIN))
         CALL IPIOUT(ICMTOP(RPYMIN))
         CALL IPPSTF(2,' t')
      ENDIF
      IF (MODE.EQ.2.OR.MODE.EQ.5) THEN
         CALL IPPSTF(7,' 90 r 0')
         CALL IPIOUT(ICMTOP(-VALUE))
         CALL IPPSTF(3,' t ')
         CALL IPIOUT(ICMTOP(RPXMIN))
         CALL IPIOUT(ICMTOP(RPYMIN))
         CALL IPPSTF(2,' t')
      ENDIF
      CALL IPPSTF(15,' .25 .25 scale ')
      IF (MODE.NE.3) CALL IPSVRT(1)
*
      NBSAV0 = NBSAVE
*
      RETURN
*
   20 CALL IGERR('Invalid PostScript file type','IPDEF')
      PFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
*
      END
