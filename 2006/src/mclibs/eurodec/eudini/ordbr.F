*
* $Id: ordbr.F,v 1.1.1.1 1996/03/08 16:58:50 mclareni Exp $
*
* $Log: ordbr.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:50  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE ORDBR(IFIRST,ILAST)
C.----------------------------------------------------------------------
C.
C.    ROUTINE ORDERS DECAY TABLE IN DECREASING BRANCHING RATIO FOR EACH
C.    PARTICLE DECAY. THE CUM. DISTRIBUTION IS CALCULATED.
C.    LAST UPDATE: 10/04/88
C.
C.----------------------------------------------------------------------
#include "eurodec/dtable.inc"
      IF (IFIRST.EQ.ILAST) RETURN
C--
C--   ORDER THE BRANCHING FRACTIONS IN A DECREASING SERIES
      DO 30 I=IFIRST,ILAST-1
        DO 20 J=I+1,ILAST
          IF (DBR(J).LE.DBR(I)) GOTO 20
          NDBUF=NDP(J)
          NDP(J)=NDP(I)
          NDP(I)=NDBUF
          DO 10 K=1,6
            IBUFCO=IDC(K,J)
            IDC(K,J)=IDC(K,I)
   10     IDC(K,I)=IBUFCO
          BUFBR=DBR(J)
          DBR(J)=DBR(I)
          DBR(I)=BUFBR
          IBUFME=ME(J)
          ME(J)=ME(I)
          ME(I)=IBUFME
   20   CONTINUE
   30 CONTINUE
C--
C--   CALCULATE CUM. DISTRIBUTION
      DO 40 I=IFIRST+1,ILAST
   40 DBR(I)=DBR(I-1)+DBR(I)
      RETURN
      END
