*
* $Id: kivind.F,v 1.1.1.1 1996/03/08 15:32:53 mclareni Exp $
*
* $Log: kivind.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:53  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  1.61/01 22/06/89  17.49.56  by
*-- Author :
      SUBROUTINE KIVIND(VSUB,NCHSUB,LENFR,LENTO)
*
********************************************************************************
*
* Decode the subscript of a vector into the two arrays LENFR end LENTO
*
*     Input :
*
      CHARACTER*(*) VSUB
      INTEGER NCHSUB
*
*     Output :
*
      INTEGER LENFR(3)
      INTEGER LENTO(3)
*
********************************************************************************
*
#include "kuip/kcques.inc"
      DIMENSION NL(3),NR(3)
*
      IQUEST(1)=0

      M1=INDEX(VSUB,',')
      IF (M1.EQ.0) THEN
        M2=0
      ELSE
        IF (M1.EQ.1) GO TO 910
        IF (M1.EQ.NCHSUB) GO TO 910
        M2=INDEX(VSUB(M1+1:),',')
        IF (M2.GT.0) THEN
          M2=M2+M1
          IF (M2.EQ.M1+1) GO TO 910
          IF (M2.EQ.NCHSUB) GO TO 910
          M3=INDEX(VSUB(M2+1:),',')
          IF (M3.GT.0) GO TO 920
        ENDIF
      ENDIF

      NL(1)=1
      IF (M1.EQ.0) THEN
        NR(1)=NCHSUB
      ELSE
        NR(1)=M1-1
        NL(2)=M1+1
        IF (M2.EQ.0) THEN
          NR(2)=NCHSUB
        ELSE
          NR(2)=M2-1
          NL(3)=M2+1
          NR(3)=NCHSUB
        ENDIF
      ENDIF

      DO 20 I=1,3
        LENFR(I)=1
        IF((I.EQ.1).OR.(I.EQ.2.AND.M1.GT.0).OR.(I.EQ.3.AND.M2.GT.0))THEN
          ICOLON=INDEX(VSUB(NL(I):NR(I)),':')
          IF (ICOLON.EQ.0) THEN
            CALL KICTOI(VSUB(NL(I):NR(I)),LENFR(I))
            IF (IQUEST(1).NE.0) GO TO 910
            LENTO(I)=LENFR(I)
          ENDIF
          IF (ICOLON.GT.1) THEN
            CALL KICTOI(VSUB(NL(I):NL(I)+ICOLON-2),LENFR(I))
            IF (IQUEST(1).NE.0) GO TO 910
          ENDIF
          IF (ICOLON.GT.0.AND.ICOLON.LT.NR(I)-NL(I)+1) THEN
            CALL KICTOI(VSUB(NL(I)+ICOLON:NR(I)),LENTO(I))
            IF (IQUEST(1).NE.0) GO TO 910
          ENDIF
        ENDIF
20    CONTINUE

      GO TO 999

910   IQUEST(1)=1
      CALL KUALFA
      PRINT *,'*** Illegal syntax in vector name'
      GO TO 999

920   IQUEST(1)=1
      CALL KUALFA
      PRINT *,'*** Max 3 dimensions are supported'
      GO TO 999

999   END


