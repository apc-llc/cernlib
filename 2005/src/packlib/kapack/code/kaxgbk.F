*
* $Id: kaxgbk.F,v 1.1.1.1 1996/03/08 11:40:52 mclareni Exp $
*
* $Log: kaxgbk.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:52  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXGBK(IBLOCK)
*
*.....GET A BLOCK FROM THE FREE BLOCK CHAIN
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax030.inc"
#include "kapack/kax050.inc"
#include "kapack/kax0a0.inc"
#include "kapack/kax0b0.inc"
*
      INTEGER IFREE(1:4)
*
*-----------------------------------------------------------------------
*
*.....GET A LOCK ON BLOCK 1
      CALL KAXOLK(1)
*
*.....UNCHAIN THE FIRST FREE BLOCK IF ONE EXISTS
      CALL KAXRD(1,IB,LBLK)
      IF ( IB(2) .LE. 0 )  GO TO 91
      CALL KAXRD(IB(3),IFREE,4)
      IF ( IFREE(1).NE.IB(3) .OR. IFREE(2).NE.0 .OR.
     +     IFREE(3).LT.0 .OR. IFREE(4).NE.0 )  GO TO 92
      IBLOCK = IFREE(1)
      IB(3)  = IFREE(3)
      IB(2)  = IB(2) - 1
      CALL KAXWRT(1,IB,IB(7)-1)
      NFBK(LUNKAF) = IB(2)
      NFREE        = IB(2)
*
*.....UPDATE BLOCK 1 IN STORAGE IF IT EXISTS THERE
      IF ( IA(1) .EQ. 1 )  THEN
         IA(2) = IB(2)
         IA(3) = IB(3)
      ENDIF
*
*.....CALCULATE THE NUMBER AND PERCENTAGE OF FREE BLOCKS
      PCFREE = REAL(IB(2)) / REAL(IB(14)) * 100.0
      IF ( PCFREE .LT. 10.0 )  THEN
         WRITE(MSG,101) LUNKAF, IB(2), PCFREE
         CALL KAXMSG(LUNLOG,MSG)
      ELSE IF ( PCFREE .LT. 20.0 )  THEN
         WRITE(MSG,102) LUNKAF, IB(2), PCFREE
         CALL KAXMSG(LUNLOG,MSG)
      ENDIF
*
*.....SUCCESSFUL COMPLETION - RELEASE THE LOCK AND RETURN
      CALL KAXRLK(1)
      RETURN
*
*.....ERROR PROCESSING
 91   WRITE(MSG,191) LUNKAF
      GO TO 99
*
 92   WRITE(MSG,192) LUNKAF, IB(3), IFREE
      GO TO 99
*
 99   CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 101  FORMAT('CRNKA191 KAXGBK: **** SERIOUS WARNING ****, THE KA-FILE
     +        ON UNIT ',I3,' CONTAINS ONLY ',I11,' (',F5.1,'%) ',
     +       ' FREE BLOCKS')
 102  FORMAT('CRNKA192 KAXGBK: *** WARNING ***, THE KA-FILE
     +        ON UNIT ',I3,' CONTAINS ONLY ',I11,' (',F5.1,'%) ',
     +       ' FREE BLOCKS')
 191  FORMAT('CRNKA193 KAXGBK: NO FREE BLOCKS AVAILABLE ON UNIT ',I3)
 192  FORMAT('CRNKA194 KAXGBK: KAPACK SYSTEM ERROR OR CORRUPT FILE, ',
     +       ' INVALID FREE BLOCK ON UNIT ',I3,', BLOCK NUMBER = ',I11,
     +       ', CONTENTS = ',4I11)
*
      END
