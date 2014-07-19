*
* $Id: izarc.F,v 1.2 1996/11/20 15:21:44 couet Exp $
*
* $Log: izarc.F,v $
* Revision 1.2  1996/11/20 15:21:44  couet
* *** empty log message ***
*
* Revision 1.1.1.1  1996/02/14 13:11:08  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      SUBROUTINE IZARC(X,Y,R1,R2,PHI1,PHI2)
*.===========>
*.
*.   This routine stores in the current picture the necessary data to
*. draw an arc of circle .
*.
*. _Input parameters:
*.
*.  REAL X,Y : Center of the arc .
*.  REAL R1 : First radius .
*.  REAL R2 : Second radius .
*.  REAL PHI1 : First angle .
*.  REAL PHI2 : Second angle .
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
*.______________________________________
*
#if defined(CERNLIB_ZEBRA)
      IF(ZFLAG)THEN
         IF(LPICT.LT.0)RETURN
         IF(IZPUSH(0,6,0,'IGARC').NE.0)RETURN
         CALL IZCFA(IFAICO,1)
         CALL IZCFA(IFASCO,1)
         CALL IZCFA(ILNCO,1)
         CALL IZCFA(ILWSCO,1)
         CALL IZCFA(IPLCCO,1)
         CALL IZCFA(IFACCO,1)
         CALL IZCFA(IBORCO,1)
*
         CALL IZSTCC(IARCCO,IFLPTR)
         Q(LHF+IFLPTR)=X
         Q(LHF+IFLPTR+1)=Y
         Q(LHF+IFLPTR+2)=R1
         Q(LHF+IFLPTR+3)=R2
         Q(LHF+IFLPTR+4)=PHI1
         Q(LHF+IFLPTR+5)=PHI2
         CALL IZINCF(6)
      ENDIF
#endif
#if defined(CERNLIB_MAIL)
*
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(I3,4E16.7)') IARCCO,X,Y,R1,R2
         CALL IMWRIT(1)
         WRITE (CHMAIL,'(2E16.7)') PHI1,PHI2
         CALL IMWRIT(3)
      ENDIF
#endif
*
      END
#endif
