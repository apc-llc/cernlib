*
* $Id: igmess.F,v 1.1.1.1 1996/02/14 13:10:36 mclareni Exp $
*
* $Log: igmess.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:36  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.16/07 13/11/92  09.17.20  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGMESS(N,CHMESS,CHTIT,CHOPT)
#if defined(CERNLIB_X11)
#include "higz/hikern.inc"
#endif
      CHARACTER*(*) CHMESS(N),CHTIT,CHOPT
      CHARACTER*12 CODED
*.______________________________________
*
#if !defined(CERNLIB_X11)
      WRITE (CODED,'(''('',I2,''(X,A,/))'')') N
      IF(INDEX(CHOPT,'P').NE.0)WRITE (6,CODED)
     +              (CHMESS(I)(1:LENOCC(CHMESS(I))+1),I=1,N)
#endif
#if defined(CERNLIB_X11)
*
      IF(INDEX(CHOPT,'T').NE.0)THEN
         WRITE (CODED,'(''('',I2,''(X,A,/))'')') N
         IF(INDEX(CHOPT,'P').NE.0)WRITE (6,CODED)
     +                (CHMESS(I)(1:LENOCC(CHMESS(I))+1),I=1,N)
         RETURN
      ENDIF
*
      LENMAX=LENOCC(CHMESS(1))
      DO 10 I=2,N
         IF(LENOCC(CHMESS(I)).GT.LENMAX)LENMAX=LENOCC(CHMESS(I))
   10 CONTINUE
      CALL IGXMES(0,0,LENMAX,N,CHMESS,CHTIT,CHOPT)
*
#endif
*
      END
