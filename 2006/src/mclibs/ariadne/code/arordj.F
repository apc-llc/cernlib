C***********************************************************************
C $Id: arordj.F,v 1.2 1996/04/10 12:33:25 mclareni Exp $

      SUBROUTINE ARORDJ(NJET)

C...ARiadne subroutine ORDer Jets

C...Orders jets in falling energy

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "lujets.f"
#include "ludat1.f"


C...Error if no space left in /ARPART/
      IF (IPART+N.GT.MSTU(4)) THEN
        CALL LUERRM(11,'(ARCLUS:) no more memory left in LUJETS')
        NJET=-1
        MSTU(3)=0
        RETURN
      ENDIF

C...Tag all jets to be copied
      NJET=0
      DO 100 I=1,IPART
        QQ(I)=QDONE(I)
        IF (.NOT.QDONE(I)) NJET=NJET+1
 100  CONTINUE

C...Loop twice to get jets in falling order in energy
      IJ=N
      DO 200 II=1,NJET
        EMAX=-1.0
        IEMAX=0
        DO 210 I=1,IPART
          IF (QQ(I)) GOTO 210
          IF (BP(I,4).LE.EMAX) GOTO 210
          EMAX=BP(I,4)
          IEMAX=I
 210    CONTINUE

C...Copy jet into /LUJETS/

        IJ=IJ+1
        DO 220 J=1,5
          P(IJ,J)=BP(IEMAX,J)
          K(IJ,J)=0
 220    CONTINUE
        K(IJ,1)=31
        K(IJ,2)=97
        QQ(IEMAX)=.TRUE.

 200  CONTINUE

      MSTU(3)=NJET

      RETURN

C**** END OF ARORDJ ****************************************************
      END
