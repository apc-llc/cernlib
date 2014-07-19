*
* $Id: izml.F,v 1.1.1.1 1996/02/14 13:11:11 mclareni Exp $
*
* $Log: izml.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:11  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
*CMZ :  1.10/02 14/05/90  17.31.11  by  O.Couet
*-- Author :
      SUBROUTINE IZML(N,X,Y)
*.===========>
*.
*.   This routine stores in the current picture the necessary data to
*. to draw a multiline .
*.
*. _Input parameters:
*.
*.  INTEGER N : Number of points .
*.  REAL X(N) : X coordinates .
*.  REAL Y(N) : Y coordinates .
*.
*..==========> (O.Couet)
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hipaw.inc"
#endif
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
      DIMENSION X(*),Y(*)
*.______________________________________
*
#if defined(CERNLIB_MAIL)
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(I3,I5)') IMLCO,N
         CALL IMWRIT(1)
         CALL IMFOUT(N,X)
         CALL IMFOUT(N,Y)
         CALL IMWRIT(5)
      ENDIF
#endif
#if defined(CERNLIB_ZEBRA)
*
      IF(ZFLAG)THEN
         IF(LPICT.LT.0)RETURN
         IF(IZPUSH(2,2*N,0,'IML').NE.0)RETURN
         CALL IZCFA(ILNCO,1)
         CALL IZCFA(ILWSCO,1)
         CALL IZCFA(IPLCCO,1)
         CALL IZSTCC(IMLCO,INTPTR)
         IQ(LHI+INTPTR)=IFLPTR
         IQ(LHI+INTPTR+1)=N
         CALL IZINCI(2)
         DO 10 I=1,N
            Q(LHF+IFLPTR+I-1)=X(I)
   10    CONTINUE
         DO 20 I=1,N
            Q(LHF+IFLPTR+I+N-1)=Y(I)
   20    CONTINUE
         CALL IZINCF(2*N)
      ENDIF
#endif
*
      END
#endif
