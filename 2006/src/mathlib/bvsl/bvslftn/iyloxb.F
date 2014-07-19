*
* $Id: iyloxb.F,v 1.1.1.1 1996/03/21 17:19:56 mclareni Exp $
*
* $Log: iyloxb.F,v $
* Revision 1.1.1.1  1996/03/21 17:19:56  mclareni
* Bvsl
*
*
#include "pilot.h"
*CMZ :          06/11/90  14.36.24  by  Michel Roethlisberger/IBM
*-- Author :
      SUBROUTINE IYLOXB (N,IY,IX,LV,NB,LO)
C
C  ************************************************************
C  *                                                          *
C  * Author: M.Roethlisberger/IBM                             *
C  *                                                          *
C  * Date:  6 Nov 1990,  V1.01,  Creation of the module       *
C  *                                                          *
C  ************************************************************
C
      LOGICAL    LV  (N)
      DIMENSION  IY  (N)
      DIMENSION  IX  (N)
      CHARACTER*2    LO
 
      IF (LO.EQ.'EQ') THEN
        NB = 0
        DO 110 I=1,N
          LV(I) = IY(I).EQ.IX(I)
          IF (LV(I)) NB = NB + 1
 110    CONTINUE
        GOTO 999
      ENDIF
 
      IF (LO.EQ.'LT') THEN
        NB = 0
        DO 120 I=1,N
          LV(I) = IY(I).LT.IX(I)
          IF (LV(I)) NB = NB + 1
 120    CONTINUE
        GOTO 999
      ENDIF
 
      IF (LO.EQ.'LE') THEN
        NB = 0
        DO 130 I=1,N
          LV(I) = IY(I).LE.IX(I)
          IF (LV(I)) NB = NB + 1
 130    CONTINUE
        GOTO 999
      ENDIF
 
      IF (LO.EQ.'GT') THEN
        NB = 0
        DO 140 I=1,N
          LV(I) = IY(I).GT.IX(I)
          IF (LV(I)) NB = NB + 1
 140    CONTINUE
        GOTO 999
      ENDIF
 
      IF (LO.EQ.'GE') THEN
        NB = 0
        DO 150 I=1,N
          LV(I) = IY(I).GE.IX(I)
          IF (LV(I)) NB = NB + 1
 150    CONTINUE
        GOTO 999
      ENDIF
 
      IF (LO.EQ.'NE') THEN
        NB = 0
        DO 160 I=1,N
          LV(I) = IY(I).NE.IX(I)
          IF (LV(I)) NB = NB + 1
 160    CONTINUE
      ENDIF
 
 999  RETURN
      END
