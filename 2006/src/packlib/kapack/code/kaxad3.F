*
* $Id: kaxad3.F,v 1.1.1.1 1996/03/08 11:40:52 mclareni Exp $
*
* $Log: kaxad3.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:52  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXAD3(KEY,IDATA,LDATA,LINK)
*
*.....ADD A RECORD TO A TREE: METHOD 3, ACQUIRE ADDITIONAL BLOCK(S)
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax0a0.inc"
#include "kapack/kax0b0.inc"
*
      INTEGER IDATA(*), KEY(*)
*
*-----------------------------------------------------------------------
*
*.....INITIALIZE
      CALL KAXGBK(NBLOCK)
      LINK  = NBLOCK
      NPREV = IA(2)
      ITYPE = 0
      LDONE = 0
      LDO   = MIN ( LBLK-NBCW-NRCW-KEY(1), LDATA )
*
*.....LOOP FOR EACH SEGMENT
 1    IF ( LDATA .GT. LDONE+LDO )  THEN
         CALL KAXGBK(NXTBLK)
         IF ( ITYPE .EQ. 0 )  ITYPE = 1
      ELSE
         NXTBLK = IA(1)
         IF ( ITYPE .NE. 0 )  ITYPE = 2
      ENDIF
      IB( 1) = NBLOCK
      IB( 2) = NPREV
      IB( 3) = NXTBLK
      IB( 4) = IA( 4)
      IB( 5) = IA( 5)
      IB( 6) = NBCW + 1
      IB( 7) = IB( 6)
      IB( 8) = ITYPE
      IB( 9) = 0
      IB(10) = 0
      IF ( ITYPE .LE. 1 )  THEN
         IB(IB(7)) = LREC
         CALL UCOPY(KEY,IB(IB(7)+1),KEY(1))
         IB(7) = IB(7) + NRCW + KEY(1)
      ENDIF
      CALL UCOPY(IDATA(LDONE+1),IB(IB(7)),LDO)
      IB(7) = IB(7) + LDO
      CALL KAXWRT(IB(1),IB,IB(7)-1)
      IF ( ITYPE .LE. 1 )  CALL KAXGXA(IB)
      LDONE = LDONE + LDO
      IF ( LDONE .LT. LDATA )  THEN
         NPREV  = NBLOCK
         NBLOCK = NXTBLK
         ITYPE  = 3
         LDO    = MIN ( LBLK-NBCW, LDATA-LDONE )
         GO TO 1
      ENDIF
*
      IA(2) = IB(1)
*
      END
