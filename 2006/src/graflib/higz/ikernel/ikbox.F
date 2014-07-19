*
* $Id: ikbox.F,v 1.2 1996/09/25 14:58:46 couet Exp $
*
* $Log: ikbox.F,v $
* Revision 1.2  1996/09/25 14:58:46  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:55  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GL)||defined(CERNLIB_X11)||defined(CERNLIB_FALCO)||defined(CERNLIB_MACMPW)
*CMZ :  1.22/08 14/02/95  09.29.55  by  O.Couet
*-- Author :    O.Couet   17/05/90
      SUBROUTINE IKBOX(X1,X2,Y1,Y2)
*.===========>
*.
*.   This routine draws a boxe. The boxe is drawn according to the
*. current fill area attributes. Most of the time the basic graphics
*. packages are able to draw boxes in a very fast way. This routines
*. access this type of facilities.
*.
*. _Input parameters:
*.
*.  REAL X1,Y1 : Left down corner (in WC).
*.  REAL X2,Y2 : Right up corner (in WC).
*.
*..==========> (O.Couet)
#if defined(CERNLIB_KERNEL)
#include "higz/hikern.inc"
#endif
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
      DIMENSION X(4),Y(4)
*.______________________________________
*
#if defined(CERNLIB_KERNEL)
#include "higz/hiwcdc.inc"
#endif
*
*              Draw the inside of the box with the portable
*              HIGZ hatches.
*
      IF(IFAIS.EQ.3.AND.IFASI.GE.100)THEN
         X(1)=X1
         X(2)=X2
         X(3)=X2
         X(4)=X1
         Y(1)=Y1
         Y(2)=Y1
         Y(3)=Y2
         Y(4)=Y2
         CALL IGFA(4,X,Y)
         GOTO 20
      ENDIF
#if defined(CERNLIB_FALCO)
      IF(IWTYPE.EQ.7878.OR.IWTYPE.EQ.7879)GOTO 20
#endif
#if defined(CERNLIB_KERNEL)
*
*              Draw the inside of the box with color or
*              device dependant pattern.
*
      IF(IFAIS.NE.0)THEN
         IF(GFLAG)THEN
            DO 10 IWKNB=1,INOPWK
               IF(NODRFL(IWKNB))GOTO 10
               CALL IGSG(IWKNB)
#endif
#if defined(CERNLIB_GL)
               IX1=IXWCDC(X1)
               IX2=IXWCDC(X2)
               IY1=IYWCDC(Y1)
               IY2=IYWCDC(Y2)
               IF(IFAIS.NE.0)THEN
                  IF(ICURCI(IWKNB).NE.IFACI)THEN
                     CALL COLOR(IFACI+IOFCOL)
                     ICURCI(IWKNB)=IFACI
                  ENDIF
                  CALL SBOXFI(IX1,IY1,IX2,IY2)
               ENDIF
#endif
#if defined(CERNLIB_X11)
               IX1=IXWCDC(X1)
               IX2=IXWCDC(X2)
               IY1=IYWCDC(Y1)
               IY2=IYWCDC(Y2)
               MODE=1
               CALL IXBOX(IX1,IX2,IY1,IY2,MODE)
#endif
#if defined(CERNLIB_MACMPW)
               IX1=IXWCDC(X1)
               IX2=IXWCDC(X2)
               IY1=IYWCDC(Y1)
               IY2=IYWCDC(Y2)
               MODE=1
               CALL IMBOX(IX1,IX2,IY1,IY2,MODE)
#endif
#if defined(CERNLIB_KERNEL)
   10       CONTINUE
         ENDIF
      ENDIF
#endif
   20 CONTINUE
#if defined(CERNLIB_FALCO)
      IF(IWTYPE.EQ.7878.OR.IWTYPE.EQ.7879)THEN
         CALL IGSG(1)
         IF(GFLAG)THEN
            X(1)=X1
            X(2)=X2
            X(3)=X2
            X(4)=X1
            Y(1)=Y1
            Y(2)=Y1
            Y(3)=Y2
            Y(4)=Y2
            IF(IFAIS.EQ.1)CALL IGHATC(0.001,0.,4,X,Y)
            IF(IFAIS.EQ.0.OR.IBORD.EQ.1)THEN
               CALL IFPL(4,X,Y)
               X(2)=X(4)
               Y(2)=Y(4)
               CALL IFPL(2,X,Y)
            ENDIF
         ENDIF
         RETURN
      ENDIF
#endif
#if defined(CERNLIB_KERNEL)
*
*              Draw the border of the box
*
      IF(IFAIS.EQ.0.OR.IBORD.NE.0)THEN
         IF(GFLAG)THEN
            DO 30 IWKNB=1,INOPWK
               IF(NODRFL(IWKNB))GOTO 30
               CALL IGSG(IWKNB)
#endif
#if defined(CERNLIB_GL)
               IX1=IXWCDC(X1)
               IX2=IXWCDC(X2)
               IY1=IYWCDC(Y1)
               IY2=IYWCDC(Y2)
               IF(ICURCI(IWKNB).NE.IPLCI)THEN
                  CALL COLOR(IPLCI+IOFCOL)
                  ICURCI(IWKNB)=IPLCI
               ENDIF
               CALL SBOXI(IX1,IY1,IX2,IY2)
#endif
#if defined(CERNLIB_X11)
               IX1=IXWCDC(X1)
               IX2=IXWCDC(X2)
               IY1=IYWCDC(Y1)
               IY2=IYWCDC(Y2)
               MODE=0
               CALL IXBOX(IX1,IX2,IY1,IY2,MODE)
#endif
#if defined(CERNLIB_MACMPW)
               IX1=IXWCDC(X1)
               IX2=IXWCDC(X2)
               IY1=IYWCDC(Y1)
               IY2=IYWCDC(Y2)
               MODE=0
               CALL IMBOX(IX1,IX2,IY1,IY2,MODE)
#endif
#if defined(CERNLIB_KERNEL)
   30       CONTINUE
         ENDIF
      ENDIF
#endif
*
      END
#endif
