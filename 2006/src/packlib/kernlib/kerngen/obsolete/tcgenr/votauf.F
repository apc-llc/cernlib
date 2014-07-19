*
* $Id: votauf.F,v 1.1.1.1 1996/02/15 17:53:28 mclareni Exp $
*
* $Log: votauf.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:28  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION VOTAUF(MCOD)
C
C
C
      DIMENSION VTAU (21)
C
C--                        ,  NOUGHT  ,   GAMMA  ,    NU    , ELECTRON ,
C    ,    MU    ,   PI0    ,   PI+-   ,   K+-    ,    K0    ,  PROTON  ,
C    , NEUTRON  ,  LAMBDA  ,  SIGMA+  ,  SIGMA0  ,  SIGMA-  ,   XI0    ,
C    ,   XI-    , DEUTERON ,  TRITON  ,   HE3++  ,   HE4++  ,
C
      DATA  VTAU           /1.0E8     ,1.0E8     ,1.0E8     ,1.0E8     ,
     +2.2001E-6 ,1.80E-16  ,2.55E-8   ,1.229E-8  ,0.92E-10  ,1.0E8     ,
     +1.0100E3  ,2.62E-10  ,7.88E-11  ,1.0E-15   ,1.58E-10  ,3.06E-10  ,
     +1.74E-10  ,1.0E8     ,1.0E8     ,1.0E8     ,1.0E8/
C
      MASCOD = MOD(MCOD, 32)
      VOTAUF = 1.0E10
      IF (MASCOD .GT. 0  .AND. MASCOD .LT. 22)  VOTAUF = VTAU(MASCOD)
      RETURN
      END
