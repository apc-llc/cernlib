*
* $Id: dzdgnz.F,v 1.1.1.1 1996/03/04 16:13:09 mclareni Exp $
*
* $Log: dzdgnz.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:09  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDGNZ(LB,IB,LD,JB1,NLDON1,NBLEV, NULEV, NBLEVD)
      INTEGER LB(999),IB(999),LD,NLDON1,NBLEV, NULEV, NBLEVD, K
*     get # of live down banks and highest live bank (2 levels)
      NBLEV=0
      NULEV=0
      NLDON1=0
      NBLEVD=0
      IF(IB(LD-3).GT.0 .AND. JB1.LE.IB(LD-3))THEN
         DO 30 K=JB1,IB(LD-3)
            LDD = LB(LD-K)
            IF(LDD .NE. 0)THEN
               IF(K.EQ.1)NLDON1 = 1
               NBLEV = NBLEV + 1
               NULEV=K
*              next level
               IF(IB(LDD-3).GT.0)THEN
                  DO 25 J=1,IB(LDD-3)
25                IF(LB(LDD-J).NE.0)NBLEVD=NBLEVD+1
               ENDIF
            ENDIF
   30    CONTINUE
      ENDIF
      END
******************************************************************
