*
* $Id: izlbl.F,v 1.1.1.1 1996/02/14 13:11:11 mclareni Exp $
*
* $Log: izlbl.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:11  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
*CMZ :  1.16/09 07/01/93  15.34.11  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IZLBL
*.===========>
*.
*.   This routine stores the IGLBL data in the current picture .
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
#include "higz/hilabs.inc"
*.______________________________________
*
#if defined(CERNLIB_ZEBRA)
      IF(ZFLAG)THEN
         IF(LPICT.LE.0)RETURN
         IF(IZPUSH(NHILAB+2,0,0,'IGLBL').NE.0)RETURN
         IPUSH=0
         DO 10 I=1,NHILAB
            ILEN  = LENOCC(HILABS(I))
            IPUSH = IPUSH+((ILEN+3)/4)
   10    CONTINUE
         IF(IZPUSH(0,0,IPUSH,'IGLBL').NE.0)RETURN
*
         CALL IZSTCC(ILBLCO,INTPTR)
         IQ(LHI+INTPTR)   = NHILAB
         IQ(LHI+INTPTR+1) = ISTPTR
         DO 20 I=1,NHILAB
            ILEN               = LENOCC(HILABS(I))
            IQ(LHI+INTPTR+I+1) = ILEN
            CALL UCTOH(HILABS(I),IQ(LHC+ISTPTR),4,ILEN)
            CALL IZINCS((ILEN+3)/4)
   20    CONTINUE
         CALL IZINCI(NHILAB+2)
      ENDIF
#endif
#if defined(CERNLIB_MAIL)
*
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(I3,I5)') ILBLCO,NHILAB
         CALL IMWRIT(1)
         DO 30 I=1,NHILAB
            CHMAIL=HILABS(I)
            CALL IMWRIT(2)
   30    CONTINUE
         CALL IMWRIT(5)
      ENDIF
#endif
*
      END
#endif
