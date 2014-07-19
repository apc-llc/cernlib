C***********************************************************************
C $Id: arrobo.F,v 1.2 1996/04/10 12:33:37 mclareni Exp $

      SUBROUTINE AROBO1(THE,PHI,DBEX,DBEY,DBEZ,I1)

C...Ariadne subroutine ROtate BOost 1 parton

C...Rotates and boosts 1 parton in /ARPART/

#include "arimpl.f"
      DIMENSION I(1)


      I(1)=I1
      CALL ARROBO(THE,PHI,DBEX,DBEY,DBEZ,1,I)

      RETURN

C**** END OF AROBO1 ****************************************************
      END
C***********************************************************************
C $Id: arrobo.F,v 1.2 1996/04/10 12:33:37 mclareni Exp $

      SUBROUTINE AROBO2(THE,PHI,DBEX,DBEY,DBEZ,I1,I2)

C...Ariadne subroutine ROtate BOost 2 partons

C...Rotates and boosts 2 partons in /ARPART/

#include "arimpl.f"
      DIMENSION I(2)


      I(1)=I1
      I(2)=I2
      CALL ARROBO(THE,PHI,DBEX,DBEY,DBEZ,2,I)

      RETURN

C**** END OF AROBO2 ****************************************************
      END
C***********************************************************************
C $Id: arrobo.F,v 1.2 1996/04/10 12:33:37 mclareni Exp $

      SUBROUTINE AROBO3(THE,PHI,DBEX,DBEY,DBEZ,I1,I2,I3)

C...Ariadne subroutine ROtate BOost 3 partons

C...Rotates and boosts 3 partons in /ARPART/

#include "arimpl.f"
      DIMENSION I(3)


      I(1)=I1
      I(2)=I2
      I(3)=I3
      CALL ARROBO(THE,PHI,DBEX,DBEY,DBEZ,3,I)

      RETURN

C**** END OF AROBO3 ****************************************************
      END
C***********************************************************************
C $Id: arrobo.F,v 1.2 1996/04/10 12:33:37 mclareni Exp $

      SUBROUTINE AROBO4(THE,PHI,DBEX,DBEY,DBEZ,I1,I2,I3,I4)

C...Ariadne subroutine ROtate BOost 4 partons

C...Rotates and boosts 4 partons in /ARPART/

#include "arimpl.f"
      DIMENSION I(4)


      I(1)=I1
      I(2)=I2
      I(3)=I3
      I(4)=I4
      CALL ARROBO(THE,PHI,DBEX,DBEY,DBEZ,4,I)

      RETURN

C**** END OF AROBO4 ****************************************************
      END
C***********************************************************************
C $Id: arrobo.F,v 1.2 1996/04/10 12:33:37 mclareni Exp $

      SUBROUTINE AROBO5(THE,PHI,DBEX,DBEY,DBEZ,I1,I2,I3,I4,I5)

C...Ariadne subroutine ROtate BOost 5 partons

C...Rotates and boosts 5 partons in /ARPART/

#include "arimpl.f"
      DIMENSION I(5)


      I(1)=I1
      I(2)=I2
      I(3)=I3
      I(4)=I4
      I(5)=I5
      CALL ARROBO(THE,PHI,DBEX,DBEY,DBEZ,5,I)

      RETURN

C**** END OF AROBO5 ****************************************************
      END
C***********************************************************************
C $Id: arrobo.F,v 1.2 1996/04/10 12:33:37 mclareni Exp $

      SUBROUTINE ARROBO(THE,PHI,DBEX,DBEY,DBEZ,NI,I)

C...ARiadne subroutine ROtate and BOost

C...Rotates and boost NI particles in /ARPART/

#include "arimpl.f"

      DIMENSION I(NI)


      CALL ARDBRB(DBLE(THE),DBLE(PHI),DBEX,DBEY,DBEZ,NI,I)

      RETURN

C**** END OF ARROBO ****************************************************
      END
C***********************************************************************
C $Id: arrobo.F,v 1.2 1996/04/10 12:33:37 mclareni Exp $

      SUBROUTINE ARDBRB(DTHE,DPHI,DBEX,DBEY,DBEZ,NI,I)

C...ARiadne subroutine DouBle precision ROtate and BOost

C...Rotates and boost NI particles in /ARPART/ using double precision
C...angles.

#include "arimpl.f"
#include "arpart.f"

      DIMENSION I(NI),BR(3,3),BV(3),DP(4)


      IF (DTHE**2+DPHI**2.GT.1.0D-20) THEN

C...Rotate (typically from z axis to direction theta,phi)

        DSP=SIN(DPHI)
        DCP=COS(DPHI)
        DST=SIN(DTHE)
        DCT=COS(DTHE)

        BR(1,1)=DCT*DCP
        BR(1,2)=-DSP
        BR(1,3)=DST*DCP
        BR(2,1)=DCT*DSP
        BR(2,2)=DCP
        BR(2,3)=DST*DSP
        BR(3,1)=-DST
        BR(3,2)=0.0
        BR(3,3)=DCT

        DO 100 IJ=1,NI
          DO 110 J=1,3
            BV(J)=BP(I(IJ),J)
 110      CONTINUE
          DO 120 J=1,3
            BP(I(IJ),J)=BR(J,1)*BV(1)+BR(J,2)*BV(2)+BR(J,3)*BV(3)
 120      CONTINUE
 100    CONTINUE

      ENDIF

      DBTOT2=DBEX**2+DBEY**2+DBEZ**2
      IF (DBTOT2.GT.1.0D-20) THEN
        IF (DBTOT2.GE.1.0D0) CALL ARERRM('ARROBO',14,0)
        DGA=1.0D0/DSQRT(1.0D0-DBTOT2)

        DO 200 IJ=1,NI
          DO 210 J=1,4
            DP(J)=BP(I(IJ),J)
 210      CONTINUE
          DBEP=DBEX*DP(1)+DBEY*DP(2)+DBEZ*DP(3)
          DGABEP=DGA*(DGA*DBEP/(1.0D0+DGA)+DP(4))

          BP(I(IJ),1)=DP(1)+DGABEP*DBEX
          BP(I(IJ),2)=DP(2)+DGABEP*DBEY
          BP(I(IJ),3)=DP(3)+DGABEP*DBEZ
          BP(I(IJ),4)=DGA*(DP(4)+DBEP)

 200    CONTINUE

      ENDIF

      RETURN

C**** END OF ARDBRB ****************************************************
      END
