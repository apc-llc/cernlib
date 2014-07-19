C***********************************************************************
C $Id: arclus.F,v 1.2 1996/04/10 12:33:05 mclareni Exp $

      SUBROUTINE ARCLUS(NJET)

C...ARiadne subroutine jet CLUStering

C...Clusters particle in the /LUJETS/ common block into jets according
C...the dipole clustering algorithm.

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "ardat1.f"
#include "ludat1.f"


C...Reset error flag.
      MSTA(13)=0

C...Copy all particle to be considered as jet-initiators to the end
C...of the event record.
      CALL ARCOPJ

C...The total number of jetinitiators = current number of jets.
      I1=0
      I3=0

      IF (IDIPS.LT.MAX(MSTU(47),2)) THEN
        CALL ARORDJ(NJET)
        NJET=-1
        RETURN
      ENDIF

C...Loop over all possible three-jets to find the three jets with
C...smallest invariant p_t^2

C...Exit if minimum number of jets reached
 100  IF (IDIPS.EQ.2) THEN
        CALL ARORDJ(NJET)
        RETURN
      ENDIF

      J1=0
      J2=0
      J3=0
      PT2MIN=PARA(31)

      DO 110 I2=1,IPART
        IF (QDONE(I2)) GOTO 110
        CALL ARUPDJ(I2,I1,I3)
        IF (PT2IN(I2).LT.PT2MIN) THEN
          J1=IDI(I2)
          J2=I2
          J3=IDO(I2)
          PT2MIN=PT2IN(I2)
        ENDIF
 110  CONTINUE

      PARU(63)=SQRT(PT2MIN)

C...Exit if smallest p_t^2 is above cutoff 
      IF (J1.EQ.0.OR.IDIPS.EQ.MAX(MSTU(47),2)) THEN
        CALL ARORDJ(NJET)
        RETURN
      ENDIF

C...Else join the three jets into two and redo the procedure.
      CALL ARJOIN(J1,J2,J3)
      I1=J1
      I3=J3

      GOTO 100

C**** END OF ARCLUS ****************************************************
      END
