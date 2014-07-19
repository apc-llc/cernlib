*
* $Id: ica.F,v 1.1.1.1 1996/02/14 13:10:31 mclareni Exp $
*
* $Log: ica.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:31  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/08 04/06/93  15.09.57  by  O.Couet
*-- Author :    O.Couet   04/06/93
      SUBROUTINE ICA(X1,X2,Y1,Y2,NX,NY,IV,CHOPT)
*.===========>
*.
*.   Draws a CELL array
*.
*. _Input parameters:
*.
*.  REAL X1,Y1 : Left down corner (in WC).
*.  REAL X2,Y2 : Right up corner (in WC).
*.  INTEGER NX,NY : Array dimensions
*.  INTEGER IV(NX,NY) : Array to be drawn (array of colors)
*.  CHARACTER CHOPT : Option
*.                    'P' Pixel precision (IXCA is called).
*.
*..==========> (O.Couet)
#if defined(CERNLIB_X11)
#include "higz/hiatt.inc"
#endif
      CHARACTER*(*) CHOPT
      DIMENSION IV(NX,NY)
*.______________________________________
*
#if defined(CERNLIB_X11)
#include "higz/hiwcdc.inc"
#endif
*
      IF(INDEX(CHOPT,'P').NE.0)THEN
#if defined(CERNLIB_X11)
         DO 10 IWKNB=1,INOPWK
            IF(NODRFL(IWKNB))GOTO 10
            CALL IGSG(IWKNB)
            IX1 = IXWCDC(X1)
            IX2 = IXWCDC(X2)
            IY1 = IYWCDC(Y1)
            IY2 = IYWCDC(Y2)
            CALL IXCA(IX1,IX2,IY1,IY2,NX,NY,IV)
   10    CONTINUE
#endif
      ELSE
         CALL IGSET('FAIS',1.)
         CALL IGSET('BORD',0.)
         DX   = (X2-X1)/NX
         DY   = (Y2-Y1)/NY
         X1B  = X1
         X2B  = X1+DX
*
         DO 30 I = 1, NX
            Y1B  = Y1
            Y2B  = Y1+DY
            ICOL = IV(I,1)
            CALL ISFACI(ICOL)
            DO 20 J = 2, NY
               IF(IV(I,J).EQ.ICOL)THEN
                  Y2B = Y2B+DY
               ELSE
                  CALL IGBOX(X1B,X2B,Y1B,Y2B)
                  ICOL = IV(I,J)
                  CALL ISFACI(ICOL)
                  Y1B  = Y2B
                  Y2B  = Y2B+DY
               ENDIF
   20       CONTINUE
            CALL IGBOX(X1B,X2B,Y1B,Y2B)
            X1B = X2B
            X2B = X2B+DX
   30    CONTINUE
      ENDIF
*
      END
