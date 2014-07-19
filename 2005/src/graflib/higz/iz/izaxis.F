*
* $Id: izaxis.F,v 1.1.1.1 1996/02/14 13:11:08 mclareni Exp $
*
* $Log: izaxis.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:08  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
*CMZ :  1.11/00 03/10/90  11.54.57  by  O.Couet
*-- Author :
      SUBROUTINE IZAXIS(X0,X1,Y0,Y1,WMIN,WMAX,NDIV,IOPT)
*.===========>
*.
*.   This routine store in the current picture the necessary data to
*. draw an axis .
*.
*. _Input parameters:
*.
*.  REAL X0 : X origin axis coordinate in WC space .
*.  REAL X1 : X end axis coordinate in WC space .
*.  REAL Y0 : Y origin axis coordinate in WC space .
*.  REAL Y1 : Y end axis coordinate in WC space .
*.  REAL WMIN : Lowest value for the tick marks labels written on axis .
*.  REAL WMAX : Highest value for the tick marks labels written on axis .
*.  INTEGER NDIV : Number of division .
*.  INTEGER IOPT : Option . (See IGAXIS comments)
*.
*..==========> (O.Couet)
#include "higz/hicode.inc"
#include "higz/hiflag.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hipaw.inc"
#endif
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
      DIMENSION IOPT(*)
*.______________________________________
*
#if defined(CERNLIB_ZEBRA)
      IF(ZFLAG)THEN
         IF(LPICT.LT.0)RETURN
         IF(IZPUSH(3,6,0,'IGAXIS').NE.0)RETURN
         CALL IZCFA(IPLCCO,1)
         CALL IZCFA(ILNCO,1)
         CALL IZCFA(ILWSCO,1)
         CALL IZCFA(ITMSCO,1)
         CALL IZCFA(IALHCO,1)
         CALL IZCFA(IALDCO,1)
         CALL IZCFA(ITXCCO,1)
         CALL IZCFA(ITXFCO,1)
         CALL IZCFA(IGTSCO,1)
         CALL IZCFA(IGTWCO,1)
         CALL IZCFA(IAWLCO,1)
*
         CALL IZSTCC(IAXICO,INTPTR)
         IQ(LHI+INTPTR)=IFLPTR
         IQ(LHI+INTPTR+1)=NDIV
         DO 10 I=1,25
            IF(IOPT(I).NE.0)CALL SBIT1(IQ(LHI+INTPTR+2),I)
   10    CONTINUE
         CALL IZINCI(3)
         Q(LHF+IFLPTR)=X0
         Q(LHF+IFLPTR+1)=X1
         Q(LHF+IFLPTR+2)=Y0
         Q(LHF+IFLPTR+3)=Y1
         Q(LHF+IFLPTR+4)=WMIN
         Q(LHF+IFLPTR+5)=WMAX
         CALL IZINCF(6)
      ENDIF
#endif
#if defined(CERNLIB_MAIL)
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(I3,4E16.7)') IAXICO,X0,X1,Y0,Y1
         CALL IMWRIT(1)
         WRITE (CHMAIL,'(2E16.7,I10,A10)') WMIN,WMAX,NDIV,CHOPTM
         CALL IMWRIT(3)
      ENDIF
#endif
*
      END
#endif
