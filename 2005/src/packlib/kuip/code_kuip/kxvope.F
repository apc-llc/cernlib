*
* $Id: kxvope.F,v 1.1.1.1 1996/03/08 15:32:54 mclareni Exp $
*
* $Log: kxvope.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:54  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  1.64/07 28/09/90  20.16.48  by
** Supporting Complex-type vectors now available in SIGMA
*-- Author :
      SUBROUTINE KXVOPE
*
********************************************************************************
*
* Execution routine for commands : '/VECTOR/OPERATIONS/VBIAS'
*                                  '/VECTOR/OPERATIONS/VSCALE'
*                                  '/VECTOR/OPERATIONS/VADD'
*                                  '/VECTOR/OPERATIONS/VMULTIPLY'
*                                  '/VECTOR/OPERATIONS/VSUBTRACT'
*                                  '/VECTOR/OPERATIONS/VDIVIDE'
*
********************************************************************************
*
#include "kuip/kcgen.inc"
#include "kuip/kcques.inc"
      CHARACTER*32 VNAM1,VNAM2,VNAM3,CHPATH
      SAVE VNAM1,VNAM2,VNAM3,RVAL
      CHARACTER*1  CTYPE
*
      CALL KUPATL(CHPATH,NPAR)
*
* Search for the (first) source vector
*
      CALL KUGETV(VNAM1,LLOW1,LHIGH1)
      IF (LLOW1.EQ.0) GO TO 999
*
* Get the type and the length of the source vector(s)
*
      IF (IQUEST(14).EQ.1) THEN
        CTYPE='R'
      ELSE IF (IQUEST(14).EQ.2) THEN
        CTYPE='I'
      ELSE IF (IQUEST(14).EQ.3) THEN
        CTYPE='C'
      ENDIF
      LENTOT=IQUEST(11)
*
      IF (CHPATH.EQ.'VBIAS'.OR.CHPATH.EQ.'VSCALE') THEN
        CALL KUGETR(RVAL)
      ELSE
*
* Search for the (second) source vector
*
        CALL KUGETV(VNAM2,LLOW2,LHIGH2)
        IF (LLOW2.EQ.0) GO TO 999
      ENDIF
*
* Search for the destination vector and if not found create it
* (KUGETV replaced by KUGETC + KUVECT to avoid warning if vector not existing)
*
      CALL KUGETC(VNAM3,NCH)
      CALL KUVECT(VNAM3,LLOW3,LHIGH3)
*
      IF (LLOW3.EQ.0) THEN
        CALL KUVEC(VNAM3,DUMMY,LENTOT,'C')
        IF (IQUEST(1).NE.0) THEN
          CALL KUALFA
          PRINT *,'*** KXVOPE: Error in creating destination vector'
          GO TO 999
        ENDIF
        LLOW3=IQUEST(12)
        LHIGH3=IQUEST(13)
      ENDIF
*
* Protect against overwriting when destination < source
*
      IF ((LHIGH1-LLOW1).GT.(LHIGH3-LLOW3)) LHIGH1=LLOW1+(LHIGH3-LLOW3)
*
      IF (CHPATH.EQ.'VBIAS') THEN
        J=0
        DO 10 I=LLOW1,LHIGH1
          IF (IQUEST(14).EQ.1) THEN
            Q(LLOW3+J)=RVAL+Q(I)
          ELSE IF (IQUEST(14).EQ.2) THEN
            IQ(LLOW3+J)=RVAL+IQ(I)
          ENDIF
          J=J+1
10      CONTINUE
      ELSE IF (CHPATH.EQ.'VSCALE') THEN
        J=0
        DO 20 I=LLOW1,LHIGH1
          IF (IQUEST(14).EQ.1) THEN
            Q(LLOW3+J)=RVAL*Q(I)
          ELSE IF (IQUEST(14).EQ.2) THEN
            IQ(LLOW3+J)=RVAL*IQ(I)
          ENDIF
          J=J+1
20      CONTINUE
      ELSE IF (CHPATH.EQ.'VADD') THEN
        J=0
        DO 30 I=LLOW1,LHIGH1
          IF (IQUEST(14).EQ.1) THEN
            Q(LLOW3+J)=Q(I)+Q(LLOW2+J)
          ELSE IF (IQUEST(14).EQ.2) THEN
            IQ(LLOW3+J)=IQ(I)+IQ(LLOW2+J)
          ENDIF
          J=J+1
30      CONTINUE
      ELSE IF (CHPATH.EQ.'VMULTIPLY') THEN
        J=0
        DO 40 I=LLOW1,LHIGH1
          IF (IQUEST(14).EQ.1) THEN
            Q(LLOW3+J)=Q(I)*Q(LLOW2+J)
          ELSE IF (IQUEST(14).EQ.2) THEN
            IQ(LLOW3+J)=IQ(I)*IQ(LLOW2+J)
          ENDIF
          J=J+1
40      CONTINUE
      ELSE IF (CHPATH.EQ.'VSUBTRACT') THEN
        J=0
        DO 50 I=LLOW1,LHIGH1
          IF (IQUEST(14).EQ.1) THEN
            Q(LLOW3+J)=Q(I)-Q(LLOW2+J)
          ELSE IF (IQUEST(14).EQ.2) THEN
            IQ(LLOW3+J)=IQ(I)-IQ(LLOW2+J)
          ENDIF
          J=J+1
50      CONTINUE
      ELSE IF (CHPATH.EQ.'VDIVIDE') THEN
        J=0
        DO 60 I=LLOW1,LHIGH1
          IF (IQUEST(14).EQ.1) THEN
            IF (Q(LLOW2+J).EQ.0.) THEN
              Q(LLOW3+J)=0.
            ELSE
              Q(LLOW3+J)=Q(I)/Q(LLOW2+J)
            ENDIF
          ELSE IF (IQUEST(14).EQ.2) THEN
            IF (IQ(LLOW2+J).EQ.0) THEN
              IQ(LLOW3+J)=0
            ELSE
              IQ(LLOW3+J)=IQ(I)/IQ(LLOW2+J)
            ENDIF
          ENDIF
          J=J+1
60      CONTINUE
      ENDIF

999   END



