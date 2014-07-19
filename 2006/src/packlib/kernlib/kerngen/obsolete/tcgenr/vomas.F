*
* $Id: vomas.F,v 1.1.1.1 1996/02/15 17:53:28 mclareni Exp $
*
* $Log: vomas.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:28  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION VOMAS  (IN)
C
C-    THIS FLOATING/INTEGER FUNCTION CANNOT BE WRITTEN TO SATISFY
C-    ALL COMPILERS.   PLEASE PATCHY-MODIFY IT AS NEEDED ON YOURS.
C-    WE ASSERT THIS ROUTINE WILL NOT BE CHANGED AGAIN.
C
      DIMENSION VMASS(21), IN(9)
      EQUIVALENCE (RESULT,IRESUL), (MASS,AMASS)
C
C--                        ,  NOUGHT  ,   GAMMA  ,    NU    , ELECTRON ,
C--  ,    MU    ,   PI0    ,   PI+-   ,   K+-    ,    K0    ,  PROTON  ,
C--  , NEUTRON  ,  LAMBDA  ,  SIGMA+  ,  SIGMA0  ,  SIGMA-  ,   XI0    ,
C--  ,   XI-    , DEUTERON ,  TRITON  ,   HE3++  ,   HE4++  ,          =
C
      DATA  VMASS/          0.        ,.1E-10    ,.1E-9     ,.000511006,
     +  .105659 ,.13501    ,.13960    ,.4938     ,.4980     ,.938256   ,
     +  .939550 ,1.11540   ,1.18941   ,1.1923    ,1.19708   ,1.3143    ,
     +  1.3208  ,1.87549   ,2.80879   ,2.80824   ,3.72718/
C
C
C-             RETURN 'IN' AS FUNCTION VALUE, IF 'IN' NOT IN RANGE
C
      IRESUL = IN(1)
      IF (IRESUL.LT.0)       GO TO 19
      IF (IRESUL.GE.4096)    GO TO 19
      IPC = MOD (IRESUL,64)
      IF (IPC.GE.22)         GO TO 31
      IF (IPC.EQ.0)          GO TO 19
C
C--                PARTICLES
C
      RESULT = VMASS(IPC)
   19 VOMAS = RESULT
      RETURN
C
C--                ANTIPARTICLES
C
   31 IF (IPC.GE.50)         GO TO 19
      IF (IPC.LT.42)         GO TO 19
      RESULT = -VMASS(IPC-32)
      GO TO 19
C
C
         ENTRY IOFMAS                                                    CDC
C        ENTRY IOFMAS  (IN)                                             -CDC
C
      MASS = IN(1)
      IRESUL = IUCOMP (ABS(AMASS),VMASS(1),21)
      IF (AMASS.LT.0.)  IRESUL=IRESUL+32
      VOMAS  = RESULT                                                    CDC
C     IOFMAS = IRESUL                                                   -CDC
      RETURN
      END
