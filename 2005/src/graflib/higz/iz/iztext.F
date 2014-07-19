*
* $Id: iztext.F,v 1.1.1.1 1996/02/14 13:11:14 mclareni Exp $
*
* $Log: iztext.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:14  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
*CMZ :  1.11/00 24/08/90  16.34.17  by  O.Couet
*-- Author :
      SUBROUTINE IZTEXT(X,Y,KTEXT,SSIZE,ANGLE,IOPT)
*.===========>
*.
*.   This routine stores the IGTEXT data in the current picture .
*.
*. _Input parameters:
*.
*.  REAL X,Y : Coordinates for beginning of text .
*.  CHARACTER KTEXT : Text .
*.  INTEGER SSIZE : Text size .
*.  REAL ANGLE : The rotation angle in degrees .
*.  INTEGER IOPT : Option .
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
      CHARACTER*(*) KTEXT
      DIMENSION IOPT(*)
*.______________________________________
*
      NCHP=LENOCC(KTEXT)
      IF(NCHP.LE.0)RETURN
#if defined(CERNLIB_ZEBRA)
      IF(ZFLAG)THEN
         IF(LPICT.LT.0)RETURN
         IF(IZPUSH(4,4,(NCHP+3)/4,'IGTEXT').NE.0)RETURN
         CALL IZCFA(ITXCCO,1)
         CALL IZCFA(IGTSCO,1)
         CALL IZCFA(IGTWCO,1)
*
         CALL IZSTCC(IGTXCO,INTPTR)
         IQ(LHI+INTPTR)=IFLPTR
         IQ(LHI+INTPTR+1)=ISTPTR
         IQ(LHI+INTPTR+2)=NCHP
         DO 10 I=1,3
            IF(IOPT(I).NE.0)CALL SBIT1(IQ(LHI+INTPTR+3),I)
   10    CONTINUE
         CALL IZINCI(4)
         Q(LHF+IFLPTR)=X
         Q(LHF+IFLPTR+1)=Y
         Q(LHF+IFLPTR+2)=SSIZE
         Q(LHF+IFLPTR+3)=ANGLE
         CALL IZINCF(4)
         CALL UCTOH(KTEXT,IQ(LHC+ISTPTR),4,NCHP)
         CALL IZINCS((NCHP+3)/4)
      ENDIF
#endif
#if defined(CERNLIB_MAIL)
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(I3,4E16.7,A4)') IGTXCO,X,Y,SSIZE,ANGLE,CHOPTM
         CALL IMWRIT(1)
         CHMAIL=' '
         CHMAIL=KTEXT
         CALL IMWRIT(3)
      ENDIF
#endif
*
      END
#endif
