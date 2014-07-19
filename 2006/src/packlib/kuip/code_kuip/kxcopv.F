*
* $Id: kxcopv.F,v 1.1.1.1 1996/03/08 15:32:54 mclareni Exp $
*
* $Log: kxcopv.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:54  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  1.64/07 28/09/90  20.10.59  by
** Supporting Complex-type vectors now available in SIGMA
*-- Author :
      SUBROUTINE KXCOPV
*
********************************************************************************
*
* Execution routine for command '/VECTOR/COPY'
*
********************************************************************************
*
#include "kuip/kcgen.inc"
#include "kuip/kcques.inc"
#include "kuip/kcvect.inc"
      CHARACTER*32 VNAM1,VNAM2
      SAVE VNAM1,VNAM2
      CHARACTER*1  VECTYP
      DIMENSION LENGTH(3),LENFR(3),LENTO(3),NDIM(3),ISTEPS(3)
*
      CALL KUGETV(VNAM1,LLOW1,LHIGH1)
      IF (LLOW1.EQ.0) GO TO 999
      ICOPY=IQUEST(20)
*
* Temporary vector used ?
*
      IF (ICOPY.EQ.1) THEN
        LLLBEG=LVECN
        DO 10 I=1,3
          LENFR(I)=IQUEST(20+I)
          LENTO(I)=IQUEST(30+I)
          NDIM(I)=IQ(LVECN+10+I)
          IF (LENFR(I).LE.LENTO(I)) THEN
            ISTEPS(I)=1
          ELSE
            ISTEPS(I)=-1
          ENDIF
10      CONTINUE
      ENDIF
*
      LENTOT=MIN(LHIGH1-LLOW1+1,IQUEST(11))
      DO 20 I=1,3
        LENGTH(I)=IQ(LVECN+10+I)
20    CONTINUE
      ITYPE1=IQUEST(14)
*
* KUGETV replaced by KUGETC + KUVECT to avoid warning if vector not existing
*
      CALL KUGETC(VNAM2,NCH)
      TVECFL=.FALSE.
      CALL KUVECT(VNAM2,LLOW2,LHIGH2)
      TVECFL=.TRUE.
      IF (IQUEST(1).NE.0) GO TO 999
      IF (LLOW2.EQ.0) THEN
        IF (ITYPE1.EQ.1) THEN
          VECTYP='R'
        ELSE IF (ITYPE1.EQ.2) THEN
          VECTYP='I'
        ELSE IF (ITYPE1.EQ.3) THEN
          VECTYP='C'
        ENDIF
*
        IF (INDEX(VNAM1,'(').GT.0) THEN
          LENGTH(1)=LENTOT
          LENGTH(2)=1
          LENGTH(3)=1
        ENDIF
        CALL KUVCRE(VNAM2,VECTYP,LENGTH,LLOW2,LHIGH2)
        TVECFL=.FALSE.
        CALL KUVECT(VNAM1,LLOW1,LHIGH1)
        IF (ICOPY.EQ.1) LLLBEG=LVECN
        CALL KUVECT(VNAM2,LLOW2,LHIGH2)
        TVECFL=.TRUE.
        IF (IQUEST(1).NE.0) THEN
          CALL KUALFA
          PRINT *,'*** KXCOPV: Error in creating destination vector'
          GO TO 999
        ENDIF
*
      ENDIF
      ITYPE2=IQUEST(14)
*
      IF (ICOPY.EQ.0) THEN
*
* Protect against overwriting when destination < source
*
        IF((LHIGH1-LLOW1).GT.(LHIGH2-LLOW2)) LHIGH1=LLOW1+(LHIGH2-LLOW2)
*
        J=0
*
        DO 30 I=LLOW1,LHIGH1
          IF (ITYPE1.EQ.1.AND.ITYPE2.EQ.2) THEN
            IQ(LLOW2+J)=Q(LLOW1+J)
          ELSE IF (ITYPE1.EQ.2.AND.ITYPE2.EQ.1) THEN
            Q(LLOW2+J)=IQ(LLOW1+J)
          ELSE
            IQ(LLOW2+J)=IQ(LLOW1+J)
          ENDIF
          J=J+1
30      CONTINUE
*
      ELSE
*
        III=0
*
        DO 40 K=LENFR(3),LENTO(3),ISTEPS(3)
          DO 40 J=LENFR(2),LENTO(2),ISTEPS(2)
            DO 40 I=LENFR(1),LENTO(1),ISTEPS(1)
              JJJ=I+NDIM(1)*(J-1)+NDIM(1)*NDIM(2)*(K-1)
              LLL=LLLBEG+14+JJJ
              IF (ITYPE1.EQ.1.AND.ITYPE2.EQ.2) THEN
                IQ(LLOW2+III)=Q(LLL)
              ELSE IF (ITYPE1.EQ.2.AND.ITYPE2.EQ.1) THEN
                Q(LLOW2+III)=IQ(LLL)
              ELSE
                IQ(LLOW2+III)=IQ(LLL)
              ENDIF
              III=III+1
*
* Protect against overwriting when destination < source
*
              IF (III.GT.(LHIGH2-LLOW2)) GO TO 999
*
40      CONTINUE
*
      ENDIF
*
999   END



