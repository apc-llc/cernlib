*
* $Id: izscor.F,v 1.1.1.1 1996/02/14 13:11:13 mclareni Exp $
*
* $Log: izscor.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:13  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
*CMZ :  1.10/02 14/05/90  17.31.11  by  O.Couet
*-- Author :
      SUBROUTINE IZSCOR(IWKID,IC,CR,CG,CB)
*.===========>
*.
*.   This routine stores the color representation in the current picture.
*.
*. _Input parameters:
*.
*.  INTEGER WKID : Workstation identifier .
*.  INTEGER CI : Color index .
*.  REAL CR : Red .
*.  REAL CG : Green .
*.  REAL CB : Blue .
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
*.______________________________________
*
#if defined(CERNLIB_MAIL)
*
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(2I3,I5,3E16.7)')ICRCO,IWKID,IC,CR,CG,CB
         CALL IMWRIT(4)
      ENDIF
#endif
#if defined(CERNLIB_ZEBRA)
*
      IF(ZFLAG)THEN
         IF(LPICT.LT.0)RETURN
         IF(IZPUSH(2,3,0,'ISCR').NE.0)RETURN
         CALL IZSTCC(ICRCO,INTPTR)
         IQ(LHI+INTPTR)=IFLPTR
         IQ(LHI+INTPTR+1)=IC
         CALL IZINCI(2)
         Q(LHF+IFLPTR)=CR
         Q(LHF+IFLPTR+1)=CG
         Q(LHF+IFLPTR+2)=CB
         CALL IZINCF(3)
      ENDIF
#endif
*
      END
#endif
