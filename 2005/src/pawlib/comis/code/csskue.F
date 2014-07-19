*
* $Id: csskue.F,v 1.1.1.1 1996/02/26 17:16:17 mclareni Exp $
*
* $Log: csskue.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:17  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.35  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSSKUE
***--------------------------
*     skips routine definition until END
*     now skips rest of the file (04.05.90)
***--------------------------
#include "comis/cslun.inc"
C+SEQ,CSPAR.
C+SEQ,CSREC.
C+SEQ,CSPNTS.
       IF(ISTFIL.EQ.0)RETURN
C 1     I=INDEX(REC,'#')
C       IF(I.NE.0)RETURN
C       I=INDEX(REC,'END')
C       IF(I.NE.0)THEN
C         N=MNBLAN(JSR,80)
C         IF(N.EQ.I+2)RETURN
C       ENDIF
C       READ(LUNFIL,77,END=99)REC
C  77   FORMAT(A)
C       GO TO 1
 99    ISTFIL=0
       CLOSE(LUNFIL)
       END
