*
* $Id: iegrid.F,v 1.1.1.1 1996/02/14 13:10:22 mclareni Exp $
*
* $Log: iegrid.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:22  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.07/01 19/07/89  10.26.11  by  O.Couet
*-- Author :    O.Couet   19/07/89
      SUBROUTINE IEGRID
*.===========>
*.
*.   Draws the grid if needed.
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
#include "higz/higed.inc"
*.______________________________________
*
      IF((IXSTEP*IXFACT.EQ.0).OR.(IYSTEP*IYFACT.EQ.0))RETURN
      ZFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      CALL ISMK(1)
      CALL ISPMCI(1)
      XSTDX=(RWXMAX-RWXMIN)/FLOAT(IXSTEP*IXFACT)
      YSTDY=(RWYMAX-RWYMIN)/FLOAT(IYSTEP*IYFACT)
      NBPM=1
      DO 20 I=0,(IXSTEP*IXFACT)
         XPMVAL=RWXMIN+I*XSTDX
         DO 10 J=0,(IYSTEP*IYFACT)
            X(NBPM)=XPMVAL
            Y(NBPM)=RWYMIN+J*YSTDY
            IF(NBPM.EQ.100)THEN
               CALL IPM(NBPM,X,Y)
               NBPM=0
            ENDIF
            NBPM=NBPM+1
   10    CONTINUE
   20 CONTINUE
      CALL IPM(NBPM,X,Y)
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
*
      END
