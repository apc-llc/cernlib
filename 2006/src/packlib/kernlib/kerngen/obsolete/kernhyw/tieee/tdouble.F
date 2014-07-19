*
* $Id: tdouble.F,v 1.1.1.1 1996/02/15 17:52:58 mclareni Exp $
*
* $Log: tdouble.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:58  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      PROGRAM TIEEED
 
      PARAMETER    (NWDP=12)
      PARAMETER    (NVEC=2*NWDP)
      PARAMETER    (NWUSE=NVEC)
      DOUBLE PRECISION  DPOR(NWDP), DPEE(NWDP), DPRE(NWDP)
      DIMENSION    VOR(NVEC), VEE(NVEC), VRE(NVEC)
      DIMENSION    KOR(NVEC), KEE(NVEC), KRE(NVEC)
      DIMENSION    MORLH(60), MEELH(60), MRELH(60)
      DIMENSION    MORRH(60), MEERH(60), MRERH(60)
      EQUIVALENCE  (DPOR,VOR,KOR), (DPEE,VEE,KEE), (DPRE,VRE,KRE)
 
      EQUIVALENCE  (IWORLH,WORLH), (IWEELH,WEELH), (IWRELH,WRELH)
      EQUIVALENCE  (IWORRH,WORRH), (IWEERH,WEERH), (IWRERH,WRERH)
 
      CALL VZERO (VOR,NVEC)
 
      DPOR(1)  = 1.
      DPOR(2)  = -1.D0
      DPOR(3)  = 2.
      DPOR(4)  = 4.
      DPOR(5)  = 8.
      DPOR(6)  = 16.
      DPOR(7)  = 32.
      DPOR(8)  = DPOR(1) + DPOR(1) * 2.**(-45)
      DPOR(9)  = DPOR(1) + DPOR(2) * 2.**(-45)
      DPOR(10) = DPOR(3) + DPOR(1) * 2.**(-50)
      DPOR(11) = DPOR(4) + DPOR(2) * 2.**(-50)
      DPOR(12) = DPOR(1) - DPOR(6)
 
C-                 test exponent at upper limit
 
C-                 test exponent at lower limit
 
C-                 test special cases
 
      CALL IE3FOD (VOR,VEE,NWUSE/2,JBAD)
      PRINT 9001, JBAD
 9001 FORMAT ('1PROGRAM TIEEEF EXECUTING.'/1X
     +/' DONE TO IEEE, JBAD=',I6)
 
      CALL IE3TOD (VEE,VRE,NWUSE/2,JBAD)
      PRINT 9002, JBAD
 9002 FORMAT (1X/' DONE FR IEEE, JBAD=',I6)
 
      DO 79  JL=1,NWUSE,2
      WORLH = VOR(JL)
      WEELH = VEE(JL)
      WRELH = VRE(JL)
 
      WORRH = VOR(JL+1)
      WEERH = VEE(JL+1)
      WRERH = VRE(JL+1)
 
      IFLLH = 0
      IF (IWORLH.NE.IWRELH)  IFLLH=7777
 
      IFLRH = 0
      IF (IWORRH.NE.IWRERH)  IFLRH=77777
 
      PRINT 9021, JL,WORLH,WRELH,WORLH,WRELH,IFLLH,WORRH,WRERH,IFLRH
 9021 FORMAT (1X/1X,20(1H-)/' WORD',I3,2E18.10/2(8X,2O18,2X,I3/),1X)
 
      CALL UPKBYT (WORLH,1,MORLH,36,0)
      CALL UPKBYT (WEELH,1,MEELH,32,0)
      CALL UPKBYT (WRELH,1,MRELH,36,0)
 
      CALL UPKBYT (WORRH,1,MORRH,36,0)
      CALL UPKBYT (WEERH,1,MEERH,32,0)
      CALL UPKBYT (WRERH,1,MRERH,36,0)
 
      JEXO = JBYT (WOR,28,9)
      IF (JEXO.GE.128)  JEXO=JEXO-256
 
      JEXR = JBYT (WRE,28,9)
      IF (JEXR.GE.128)  JEXR=JEXR-256
 
      JEXE = JBYT (WEELH,21,11) - 1023
 
      PRINT 9041, JEXE,(MEELH(J),J=32,1,-1),(MEERH(J),J=32,1,-1)
 9041 FORMAT (' E EXP=',I5,5X, 9I1, 3X,20I1, 1X,32I1,1X,5I1)
 
      PRINT 9043, JEXO,(MORLH(J),J=36,1,-1), (MORRH(J),J=36, 1,-1)
 9043 FORMAT (' O EXP=',I5,6X, 8I1, 1X,I1, 1X,27I1, 1X,25I1, 1X,14I1)
 
      PRINT 9044, JEXR,(MRELH(J),J=36,1,-1), (MRERH(J),J=36, 1,-1)
 9044 FORMAT (' R EXP=',I5,6X, 8I1, 1X,I1, 1X,27I1, 1X,25I1, 1X,14I1)
 
   79 CONTINUE
      END
