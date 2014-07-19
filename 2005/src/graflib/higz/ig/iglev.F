*
* $Id: iglev.F,v 1.2 2002/03/18 14:18:08 couet Exp $
*
* $Log: iglev.F,v $
* Revision 1.2  2002/03/18 14:18:08  couet
* - The number of divisions when IHCSPE is called, must be positive.
*
* Revision 1.1.1.1  1996/02/14 13:10:36  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/09 16/01/92  15.50.38  by  O.Couet
*-- Author :    O.Couet   16/01/92
      SUBROUTINE IGLEV(N,WMINI,WMAXI,ICOLOR,CHOPT)
*.===========>
*.
*.   This routines allows to initialize the level for the
*. IH routines set. It can initialize the levels along the
*. Z axis (CHOPT='A') or with a palette of colors (CHOPT='C')
*.
*. _Input parameters:
*.
*.  INTEGER N : Number of levels
*.  REAL WMINI : minimum value on Z axis
*.  REAL WMAXI : maximum value on Z axis
*.  INTEGER ICOLOR : Array containing the palette
*.  CHARACTER : Option ('A' or 'C'). The two options
*.              are exclusive.
*.
*..==========> (O.Couet)
#include "higz/higraf.inc"
      CHARACTER*(*) CHOPT
      DIMENSION ICOLOR(*)
*.______________________________________
*
      NDIVZ=N
      WMIN=WMINI
      WMAX=WMAXI
*
*              Initialize the levels on the Z axis
*
      IF(INDEX(CHOPT,'A').NE.0)THEN
         IF(NDIVZ.LT.0)THEN
            CALL IGAXIS(-1.,1.,-1.,1.,WMIN,WMAX,-NDIVZ,'XN')
         ELSE
            CALL IGAXIS(-1.,1.,-1.,1.,WMIN,WMAX,NDIVZ,'X')
         ENDIF
         CALL IHCSPE(ABS(NDIVZ),WMIN,WMAX,1,0,IREP)
      ENDIF
*
*              Initialize the colour levels
*
      IF(INDEX(CHOPT,'C').NE.0)THEN
         IF(NDIVZ.GE.NPMAX)THEN
            CALL IGERR('Too many levels','IGLEV')
            RETURN
         ENDIF
         DZ = (WMAX-WMIN)/FLOAT(NDIVZ)
         DO 10 I=1,NDIVZ-1
            YGRAF(I)=WMIN+DZ*FLOAT(I)
   10    CONTINUE
         CALL IHCLEV(NDIVZ-1,YGRAF,ICOLOR,IREP)
      ENDIF
*
      END
