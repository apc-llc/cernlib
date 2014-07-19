*
* $Id: kaxfbk.F,v 1.1.1.1 1996/03/08 11:40:52 mclareni Exp $
*
* $Log: kaxfbk.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:52  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXFBK(IBLOCK)
*
*.....INSERT A BLOCK IN THE FREE BLOCK CHAIN
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
*.....CHECK THE NUMBER OF THE BLOCK TO BE FREED
      IF ( IBLOCK.LT.2 .OR. IBLOCK.GT.NBLKS )  GO TO 91
*
      IFREE(1) = IBLOCK
      IFREE(2) = 0
      IFREE(4) = 0
*.....GET A LOCK ON BLOCK 1
      CALL KAXOLK(1)
*
*.....ADD IT TO THE FREE BLOCK CHAIN
      CALL KAXRD(1,IB,LBLK)
      IFREE(3) = IB(3)
      CALL KAXWRT(IFREE(1),IFREE,4)
      IB(2) = IB(2) + 1
      IB(3) = IFREE(1)
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
 91   WRITE(MSG,191) IBLOCK, LUNKAF
      CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 101  FORMAT('CRNKA181 KAXFBK: **** SERIOUS WARNING ****, THE KA-FILE
     +        ON UNIT ',I3,' CONTAINS ONLY ',I11,' (',F5.1,'%) ',
     +       ' FREE BLOCKS')
 102  FORMAT('CRNKA182 KAXFBK: *** WARNING ***, THE KA-FILE
     +        ON UNIT ',I3,' CONTAINS ONLY ',I11,' (',F5.1,'%) ',
     +       ' FREE BLOCKS')
 191  FORMAT('CRNKA183 KAXFBK: KAPACK SYSTEM ERROR, ATTEMPT TO FREE
     +        BLOCK ',I11,' ON UNIT ',I3)
*
      END
