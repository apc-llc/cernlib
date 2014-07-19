*
* $Id: izpave.F,v 1.1.1.1 1996/02/14 13:11:12 mclareni Exp $
*
* $Log: izpave.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:12  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/08 20/07/94  16.40.03  by  O.Couet
*-- Author :    O.Couet   20/07/94
      SUBROUTINE IZPAVE(X1I,X2I,Y1I,Y2I,DZ,ISBOX,ISFRAM,IOPT)
*.===========>
*.
*.   This routine store in the current picture the necessary data to
*. draw a pave.
*.
*..==========> (O.Couet)
#include "higz/hicode.inc"
#include "higz/hiflag.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hipaw.inc"
#endif
      DIMENSION IOPT(*)
*.______________________________________
*
#if defined(CERNLIB_ZEBRA)
      IF(ZFLAG)THEN
         IF(LPICT.LT.0)RETURN
         IF(IZPUSH(4,5,0,'IGPAVE').NE.0)RETURN
         CALL IZCFA(IPLCCO,1)
         CALL IZCFA(ILNCO,1)
*
         CALL IZSTCC(IPAVCO,INTPTR)
         IQ(LHI+INTPTR)   = IFLPTR
         IQ(LHI+INTPTR+1) = ISBOX
         IQ(LHI+INTPTR+2) = ISFRAM
         DO 10 I=1,9
            IF(IOPT(I).NE.0)CALL SBIT1(IQ(LHI+INTPTR+3),I)
   10    CONTINUE
         CALL IZINCI(4)
         Q(LHF+IFLPTR)   = X1I
         Q(LHF+IFLPTR+1) = X2I
         Q(LHF+IFLPTR+2) = Y1I
         Q(LHF+IFLPTR+3) = Y2I
         Q(LHF+IFLPTR+4) = DZ
         CALL IZINCF(5)
      ENDIF
#endif
*
      END
