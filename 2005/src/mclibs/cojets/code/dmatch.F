*
* $Id: dmatch.F,v 1.1.1.1 1996/01/11 14:14:34 mclareni Exp $
*
* $Log: dmatch.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:34  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE DMATCH(NR,MDEC,IFR)
C     ******************************
C-- MATCHES FORCED DECAYS WITH TABLE -- CALLED BY READIN
C-- CREATED: 88/05/08
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/data2.inc"
#include "cojets/decpar.inc"
#include "cojets/fstate.inc"
#include "cojets/itapes.inc"
      DIMENSION MDEC(6)
C
C-- REORDER AND CONVERT TO INTERNAL PARTICLE CODE
      MDEC(6)=KFORCE(1,NR)
      DO 115 L=1,5
115   MDEC(L)=KFORCE(L+1,NR)
      IFLAG=1
      CALL REORSL(MDEC,IFLAG)
      DO 116 L=1,6
      IF(MDEC(L).EQ.0) GO TO 116
      INTC=INTID(MDEC(L))
      IF(INTC.GT.0) THEN
        MDEC(L)=INTC
      ELSEIF(ABS(MDEC(L)).LE.6) THEN
        MDEC(L)=MDEC(L)*1000
      ELSEIF(MOD(MDEC(L),100).EQ.0) THEN
        MDEC(L)=MDEC(L)*10
      ELSE
        IFR=2
        WRITE(ITLIS,200) (KFORCE(LK,NR),LK=1,6)
200     FORMAT(/' ***BAD INPUT FOR FORCE DECAY'//4X,6I10
     *  //' ***JOB WILL BE ABORTED')
        RETURN
      ENDIF
116   CONTINUE
C
C-- FIND MATCH
      ICHI=IDB(MDEC(6))
      IF(ICHI.EQ.0) GO TO 10
      ICHR=ICHI-1
1     ICHR=ICHR+1
      DO 2 L=1,5
      IF(MDEC(L).EQ.KDP(ICHR,L)) GO TO 2
      IF(CBR(ICHR).NE.1.) GO TO 1
      GO TO 10
2     CONTINUE
      CBR(ICHR)=-CBR(ICHR)
      RETURN
C-- NO MATCH FOUND -- SET FLAG FOR ABORT
10    IFR=1
      WRITE(ITLIS,11) (KFORCE(L,NR),L=1,6)
11    FORMAT(/' ***NO MATCH FOR FORCE DECAY'//4X,6I10
     *//' ***JOB WILL BE ABORTED')
      RETURN
      END
