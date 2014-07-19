*
* $Id: dzdlft.F,v 1.1.1.1 1996/03/04 16:13:18 mclareni Exp $
*
* $Log: dzdlft.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:18  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDLFT(LDESC,LP,L,JB)
#include "dzdoc/bkwrp.inc"
      DIMENSION IDSBNK(5)
 
      IDSBNK(1) = IQ(KQSP+LDESC+7)
      NSA=IQ(KQSP+LDESC+2)
      IF(NSA .GE. 0)THEN
            IDSBNK(3) = NSA
      ELSE
            IDSBNK(3) = 1
      ENDIF
      NLDOC = IQ(KQSP+LDESC+5)
      IF(NLDOC .GT. IDSBNK(3)) THEN
          IF(NSA.LT.0)THEN
              IDSBNK(3)=NLDOC
              IQ(KQSP+LDESC+2)=-1
              IQ(KQSP+LDESC+1)=-1
*          ELSE
*              IDSBNK(3)=MAX(NSA,NLDOC)
          ENDIF
      ENDIF
      IF(IQ(KQSP+LDESC+1) .GE. 0)THEN
            IDSBNK(2) = IQ(KQSP+LDESC+1)
      ELSE
            IDSBNK(2) = 1
      ENDIF
      IF(IDSBNK(2) .LT. IDSBNK(3))IDSBNK(2)=IDSBNK(3)
      IF(JB.EQ.1)THEN
            IDSBNK(4)=4
      ELSE
            IDSBNK(4) = 3
      ENDIF
      IDSBNK(5) = 2
      CALL MZLIFT(0,L,LP,JB,IDSBNK,0)
      IQ(KQSP+L+1) = IQ(KQSP+LDESC+3)
      IQ(KQSP+L+2) = IQ(KQSP+LDESC+2)
      IQ(KQSP+L+3) = IQ(KQSP+LDESC+1)
      IF(JB.EQ.1)THEN
         IQ(KQSP+L+4) = IQ(KQSP+LDESC+9)
      ENDIF
      END
***********************************************************************
