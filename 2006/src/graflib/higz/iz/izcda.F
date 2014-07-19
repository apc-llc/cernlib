*
* $Id: izcda.F,v 1.2 1996/11/20 15:21:45 couet Exp $
*
* $Log: izcda.F,v $
* Revision 1.2  1996/11/20 15:21:45  couet
* *** empty log message ***
*
* Revision 1.1.1.1  1996/02/14 13:11:08  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
      SUBROUTINE IZCDA(IATTCO)
*.===========>
*.
*.   This routine Changes the current normalisation transformation
*. Default Attributes .
*.
*. _Input parameters:
*.
*.  INTEGER IATTCO : Code of the attribute function .
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
#include "higz/hiatnb.inc"
#include "higz/hiatt.inc"
      DIMENSION RATVAL(NBRATT)
      EQUIVALENCE (RATVAL(1),RMKSC)
      DIMENSION IATVAL(NBIATT)
      EQUIVALENCE (IATVAL(1),IPLCI)
*.______________________________________
*
      IF(LPICT.LE.0)RETURN
      INBIAT=IZGCOD(LHNT,8)
      INBRAT=IZGCOD(LHNT,9)
*
      IF(IATTCO.EQ.0)THEN
         DO 10 I=1,INBRAT
            Q(LHF+IRAPTR+I-1)=RATVAL(I)
 10      CONTINUE
         DO 20 I=1,INBIAT
            IQ(LHI+IIAPTR+I-1)=IATVAL(I)
 20      CONTINUE
      ELSE
         IF(IATTCO.LT.80)THEN
            INDEX=IATTCO-49
            IF(INDEX.GT.INBRAT)RETURN
            Q(LHF+IRAPTR+INDEX-1)=RATVAL(INDEX)
         ELSE
            INDEX=IATTCO-79
            IF(INDEX.GT.INBIAT)RETURN
            IQ(LHI+IIAPTR+INDEX-1)=IATVAL(INDEX)
         ENDIF
      ENDIF
*
      END
#endif
