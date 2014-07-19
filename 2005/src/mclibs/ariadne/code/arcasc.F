C***********************************************************************
C $Id: arcasc.F,v 1.2 1996/04/10 12:33:02 mclareni Exp $

      SUBROUTINE ARCASC

C...ARiadne subroutine perform dipole CASCade

C...Performs a colour dipole cascade on string put in the ariadne
C...event record.

#include "arimpl.f"
#include "arstrs.f"
#include "ardat1.f"
#include "arlist.f"


C...Calculate total momentum of strings for debugging
      IF (MSTA(9).GT.0) CALL ARCHEM(1)

C...Reset counter
      IO=0
      NPTOT=0

C...Perform the evolution
      CALL AREVOL(SQRT(PT2LST),0.0)

C...Check momentum and dump to /LUJETS/
      IF (.NOT.QDUMP) CALL ARDUMP
      IF (MSTA(9).GT.0) CALL ARCHEM(0)

      RETURN

C**** END OF ARCASC ****************************************************
      END
C***********************************************************************
C $Id: arcasc.F,v 1.2 1996/04/10 12:33:02 mclareni Exp $

      SUBROUTINE ARCONT

C...ARiadne subroutine CONTinue dipole cascade

C...Continues a dipole cascade peviously started with ARCASC

#include "arimpl.f"
#include "arstrs.f"
#include "ardat1.f"


C...Perform the evolution
      CALL AREVOL(SQRT(PT2LST),0.0)

C...Check momentum and dump to /LUJETS/
      IF (.NOT.QDUMP) CALL ARDUMP
      IF (MSTA(9).GT.0) CALL ARCHEM(0)

      RETURN

C**** END OF ARCONT ****************************************************
      END
C***********************************************************************
C $Id: arcasc.F,v 1.2 1996/04/10 12:33:02 mclareni Exp $

      SUBROUTINE AREVOL(PTMAX,PTMIN)

C...ARiadne subroutine EVOLute with dipole emissions

C...Evolves a string in the ariadne event record using dipole
C...successive emissions between the scale PTMAX and PTMIN

#include "arimpl.f"
#include "ardips.f"
#include "arstrs.f"
#include "ardat1.f"
#include "arhide.f"


C...Set limits
      PT2LST=PTMAX**2
      PSAV3=PARA(3)
      PSAV5=PARA(5)
      PARA(3)=MAX(PARA(3),PTMIN)
      PARA(5)=MAX(PARA(5),PTMIN)
      QABOVE=(MSTA(35).GT.1.AND.PT2LST.GT.ABS(PHAR(112)).AND.
     $     PHAR(112).NE.0)

C...Loop over all dipole to find largest possible p_t^2
 100  ISEL=0
      IF (MSTA(35).GT.0.AND.(IO.GT.0.OR.MHAR(108).GT.0)) CALL AREARR
      PT2MAX=0.0
      DO 110 I=1,IDIPS
        PT2I=ARGPT2(I)
        IF (PT2I.GT.PT2MAX) THEN
          PT2MAX=PT2I
          ISEL=I
        ENDIF
 110  CONTINUE

C...Check that largest p_t^2 is above cuts.
      IF (ISEL.GT.0) THEN
        IF ((QEM(ISEL).AND.PT2MAX.LE.PARA(5)**2).OR.
     $     ((.NOT.QEM(ISEL)).AND.PT2MAX.LE.PARA(3)**2)) ISEL=0
      ENDIF

      IF (MSTA(6).GE.0.AND.IO.GE.MSTA(6)) ISEL=0

C...Check if reconnection between strings are possible.
      IF (QABOVE.AND.(PT2MAX.LT.ABS(PHAR(112)).OR.ISEL.EQ.0)) THEN
        QABOVE=.FALSE.
        DO 200 ID=1,IDIPS
          IF (QEM(ID)) GOTO 200
          ICOLI(ID)=MOD(ICOLI(ID),1000)
 200    CONTINUE
      ENDIF

C...Exit if below cuts or limit of number of emissions is reached
      IF (ISEL.EQ.0) THEN
        IF (MHAR(107).EQ.-1) THEN
          DO 210 ID=1,IDIPS
            IF (QEM(ID)) GOTO 210
            ICOLI(ID)=MOD(ICOLI(ID),1000)
 210      CONTINUE
        ENDIF
        IF (MSTA(35).NE.0) CALL AREARR
        PARA(3)=PSAV3
        PARA(5)=PSAV5
        RETURN
      ENDIF

C...Perform the emission
      IO=IO+1
      PT2LST=PT2MAX
      CALL AREMIT(ISEL)
      QDUMP=.FALSE.

C...Check total momentum and dump according to debug mode
      IF (MSTA(9).GT.2) CALL ARDUMP
      IF (MSTA(9).GT.1) CALL ARCHEM(0)
      GOTO 100

C**** END OF ARCASC ****************************************************
      END
