*
* $Id: tiucom.F,v 1.1.1.1 1996/02/15 17:54:57 mclareni Exp $
*
* $Log: tiucom.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:57  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TIUCOM

#include "kerngent/mkcde.inc"

      DIMENSION    ICHEK1(200)
      EQUIVALENCE (ICHEK1(1),B(1))
      INTEGER TINF(4)
      DATA   TINF /1000, 4HIUCP, 20, 4HDOWN/

      CALL NEWGUY ('IUCOMP-IUFIND-IUHUNT-LOCBYT.','TIUCOM  ')

      CALL UCOPY (IBCD,A,47)
      DO 17 J=1,47
      ICHEK1(J)= IUCOMP(IBCD(J),A,47)
      ICHEK1(J+50)=IUFIND (IBCD(J),A,J,47)
   17 ICHEK1(J+100)=IUHUNT(IBCD(J),A,47,J-1)
      CALL MVERII (1,ICHEK1,INTG,47)
      CALL MVERII (2,ICHEK1(51),INTG,47)
      CALL MVERII (3,ICHEK1(101),INTG,47)

      DO 27 J=1,46
      ICHEK1(J)= IUCOMP(IBCD(J+1),A,J)
      ICHEK1(J+50)=IUFIND (IBCD(J+1),A,1,J)
   27 ICHEK1(J+100)=IUHUNT(IBCD(J+1),A,J,1)
      ICHEK1(47)=IUCOMP (IBCD(47),A,0)
      ICHEK1(147)=IUHUNT(IBCD(47),A,0,1)
      CALL UZERO (A,1,50)
      CALL MVERII (4,ICHEK1,A,47)
      CALL MVERII (5,ICHEK1(51),INTG(2),46)
      CALL MVERII (6,ICHEK1(101),A,47)

C----              TEST LOCBYT

      IPRE = 0
   41 CALL VFILL (A,100,IPRE)
      DO 44  J=1,20
      CALL SBYT     (J,  IA(J),     1, 7)
      CALL SBYT     (J,  IA(J),    11, 7)
      CALL SBYT     (J,  IA(J+20),  1, 7)
      CALL SBYT     (J,  IA(J+20), 11, 7)
      CALL SBYT     (J,  IA(J+40),  1, 7)
      CALL SBYT     (J,  IA(J+40), 11, 7)
      CALL SBYT     (J,  IA(J+60),  1, 7)
      CALL SBYT     (J,  IA(J+60), 11, 7)
   44 CONTINUE

      DO 46 J=1,20
      IB(J)    = LOCBYT (J,IA(1),20,1, 1,7)
      IB(J+20) = LOCBYT (J,IA(1),20,1,11,7) + 20
   46 CONTINUE
      CALL MVERII (11-IPRE,IB,INTG,40)

      DO 49 J=1,20
      IB(J) = LOCBYT (J,IA(1),80,8,11,7)

      JP = J
   48 IB(J+100) = JP
      IF (MOD(JP-1,8).EQ.0)  GO TO 49
      JP = JP + 20
      IF (JP.LT.81)          GO TO 48
      IB(J+100) = 0
   49 CONTINUE

      IB(21)  = LOCBYT (13,IA,73,9,1,7)
      IB(121) = 73
      CALL MVERII (13-IPRE,IB,IB(101),21)
      IPRE = IPRE - 1
      IF (IPRE.EQ.-1)        GO TO 41

C--      TIMING
      IF (ITIMES.EQ.0) RETURN
      NTIMES = ITIMES*TINF(1)
      TINF(1) = NTIMES
      CALL TIMED (TIMERD)

      JS = 1
      DO 80 J=1,NTIMES
      IA(JS) = IUCOMP(IBCD(20),IBCD,30)
      JS = JS + 1
      IF (JS.GT.1000)  JS=1
   80 CONTINUE

      CALL TIMING (TINF)
      RETURN
      END
