*
* $Id: d209rd.F,v 1.1.1.1 1996/02/15 17:48:40 mclareni Exp $
*
* $Log: d209rd.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:40  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
#if defined(CERNLIB_NEVER)
      SUBROUTINE D209RD(DX,DY,DF)
      DOUBLE PRECISION DX,DY(4),DF(4),DU(4),DAJ
      DOUBLE PRECISION DA,DQ
      COMMON /D209CM/N,ITRANS,A(4),Q(4,4),DA(4),DQ(4,4)
C
C  SUBROUTINE WHICH SETS RIGHT-HAND SIDES (DERIVATIVES) FOR DRKSTP.
C  CALLS ... CERN PACKAGES F002 AND F003.
C
C  START.
      IF(ITRANS.EQ.1) CALL DVCPY(N,DY,DY(2),DU,DU(2))
      IF(ITRANS.EQ.2) CALL DMMPY(N,N,DQ,DQ(1,2),DQ(2,1),DY,DY(2),
     *                                                    DU,DU(2))
      DO 1 J=1,N
         DAJ=DA(J)
         IF(J.EQ.1)  DU(J)=DAJ*DEXP(DAJ*DX)*DSQRT( DABS(1D0-DU(J)**2) )
         IF(J.EQ.2)  DU(J)=DAJ/DCOS(DAJ*DX)**2
         IF(J.EQ.3)  DU(J)=2D0*DAJ*DSQRT(DU(J))/(1D0+DAJ*DX)
         IF(J.EQ.4)  DU(J)=DAJ*DU(J)*DCOS(DAJ*DX)
    1 CONTINUE
      IF(ITRANS.EQ.1) CALL DVCPY(N,DU,DU(2),DF,DF(2))
      IF(ITRANS.EQ.2) CALL DMMPY(N,N,DQ,DQ(1,2),DQ(2,1),DU,DU(2),
     *                                                    DF,DF(2))
      RETURN
      END
#endif
