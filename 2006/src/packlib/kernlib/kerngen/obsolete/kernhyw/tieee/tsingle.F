*
* $Id: tsingle.F,v 1.1.1.1 1996/02/15 17:52:58 mclareni Exp $
*
* $Log: tsingle.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:58  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      PROGRAM TIEEES
 
      PARAMETER    (NVEC=44)
 
C-                  original   IEEE       read-back
      DIMENSION    VOR(NVEC), VEE(NVEC), VRE(NVEC)
      DIMENSION    KOR(NVEC), KEE(NVEC), KRE(NVEC)
      DIMENSION    MOR(60),   MEE(60),   MRE(60)
      EQUIVALENCE (KOR,VOR), (KEE,VEE), (KRE,VRE)
      EQUIVALENCE (WOR,IWOR),           (WRE,IWRE)
 
      DIMENSION    GRA(10)
      EQUIVALENCE  (VOR(1),GRA(1))
      DIMENSION    GROVF(10)
      EQUIVALENCE  (VOR(11),GROVF(1))
      DIMENSION    GRDEN(20)
      EQUIVALENCE  (VOR(21),GRDEN(1))
      DIMENSION    GRSPE(4)
      EQUIVALENCE  (VOR(41),GRSPE(1))
 
      DATA  GRA / 1.,-1., 2., 4., 8., 16.,-16., 0., 12345.,-12345. /
 
C-                 test special cases
      DATA  GRSPE(1)       / O'376777777776' /
      DATA  GRSPE(2)       / O'376777777777' /
      DATA  GRSPE(3)       / O'377000000000' /
      DATA  GRSPE(4)       / O'400000000000' /
 
C-                 test exponent at upper limit
 
      GROVF(1) = 7. * 2.**24
      GROVF(1) = GROVF(1) * 2.**100
      GROVF(2) = -GROVF(1)
 
      DO 16  J=3,9,2
      GROVF(J)   =  GROVF(J-2) / 2.
   16 GROVF(J+1) = -GROVF(J)
 
C-                 test exponent at lower limit
 
      GRDEN(1) = 7. * 2.**(-124)
      GRDEN(2) = - GRDEN(1)
 
      DO 17  J=3,19,2
      GRDEN(J)   =  GRDEN(J-2) / 2.
   17 GRDEN(J+1) = -GRDEN(J)
 
      CALL IE3FOS (VOR,VEE,NVEC,JBAD)
      PRINT 9001, JBAD
 9001 FORMAT ('1PROGRAM TIEEEF EXECUTING.'/1X
     +/' DONE TO IEEE, JBAD=',I6)
 
      CALL IE3TOS (VEE,VRE,NVEC,JBAD)
      PRINT 9002, JBAD
 9002 FORMAT (1X/' DONE FR IEEE, JBAD=',I6)
 
      DO 79  JL=1,NVEC
      WOR = VOR(JL)
      WEE = VEE(JL)
      WRE = VRE(JL)
 
      JFL = 0
      IF (IWOR.NE.IWRE)  JFL=77777
 
      PRINT 9021, JL,WOR,WRE,WOR,WRE,JFL
 9021 FORMAT (1X/1X,20(1H-)/' WORD',I3,2E18.10/8X,2O18,3X,I3/1X)
 
      CALL UPKBYT (WOR,1,MOR,36,0)
      CALL UPKBYT (WEE,1,MEE,32,0)
      CALL UPKBYT (WRE,1,MRE,36,0)
 
      JEXO = JBYT (WOR,28,9)
      IF (JEXO.GE.128)  JEXO=JEXO-256
 
      JEXR = JBYT (WRE,28,9)
      IF (JEXR.GE.128)  JEXR=JEXR-256
 
      JEXE = JBYT (WEE,24,8) - 127
      JNUL = 0
 
      PRINT 9041, JEXE, (MEE(J),J=32,1,-1)
 9041 FORMAT (' E EXP=',I5,5X, 9I1, 3X,23I1)
 
      PRINT 9043, JEXO,(MOR(J),J=36,1,-1)
 9043 FORMAT (' O EXP=',I5,6X,  8I1, 1X,I1, 1X,23I1, 1X,24I1)
 
      PRINT 9044, JEXR,(MRE(J),J=36,1,-1)
 9044 FORMAT (' R EXP=',I5,6X,  8I1, 1X,I1, 1X,23I1, 1X,24I1)
 
   79 CONTINUE
      END
