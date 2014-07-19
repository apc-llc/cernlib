*
* $Id: izfbox.F,v 1.1.1.1 1996/02/14 13:11:10 mclareni Exp $
*
* $Log: izfbox.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:10  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
*CMZ :  1.07/00 23/05/89  15.36.30  by  O.Couet
*-- Author :
      SUBROUTINE IZFBOX(X1,X2,Y1,Y2,X3,X4,Y3,Y4)
*.===========>
*.
*.   This routine stores in the current picture the necessary data to
*. draw a frame boxe .
*.
*. _Input parameters:
*.
*.  INTEGER X1,Y1 : Left down corner of the inside boxe .
*.  INTEGER X2,Y2 : Right up corner of the inside boxe .
*.  INTEGER X3,Y3 : Left down corner of the outside boxe .
*.  INTEGER X4,Y4 : Right up corner of the outside boxe .
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
         IF(IZPUSH(0,8,0,'IGFBOX').NE.0)RETURN
         CALL IZCFA(IFAICO,1)
         CALL IZCFA(IFASCO,1)
         CALL IZCFA(IFACCO,1)
         CALL IZCFA(IBORCO,1)
*
         CALL IZSTCC(IFBXCO,IFLPTR)
         Q(LHF+IFLPTR)=X1
         Q(LHF+IFLPTR+1)=X2
         Q(LHF+IFLPTR+2)=Y1
         Q(LHF+IFLPTR+3)=Y2
         Q(LHF+IFLPTR+4)=X3
         Q(LHF+IFLPTR+5)=X4
         Q(LHF+IFLPTR+6)=Y3
         Q(LHF+IFLPTR+7)=Y4
         CALL IZINCF(8)
      ENDIF
#endif
#if defined(CERNLIB_MAIL)
*
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(I3,4E16.7)') IFBXCO,X1,X2,Y1,Y2
         CALL IMWRIT(1)
         WRITE (CHMAIL,'(4E16.7)') X3,X4,Y3,Y4
         CALL IMWRIT(3)
      ENDIF
#endif
*
      END
#endif
