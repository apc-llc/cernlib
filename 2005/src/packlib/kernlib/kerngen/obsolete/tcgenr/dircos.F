*
* $Id: dircos.F,v 1.1.1.1 1996/02/15 17:53:25 mclareni Exp $
*
* $Log: dircos.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:25  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE DIRCOS(A,B)
C
C
      COMMON /SLATE/G(9),F(9),ALAM,APHI,COSL,SINL,COSP,SINP,OUT(3),
     +                                                          XQX(13)
      DIMENSION A(2),B(3)
C
C--                VARIABLE TRANSFORMATION
C--                COS X = COS L  *  COS PHI
C--                COS Y = COS L  *  SIN PHI
C--                COS Z = SIN L
C
      ALAM= A(1)
      APHI= A(2)
      COSL= COS(ALAM)
      SINL= SIN(ALAM)
      COSP= COS(APHI)
      SINP= SIN(APHI)
      OUT(1)= COSL*COSP
      OUT(2)= COSL*SINP
      OUT(3)= SINL
      CALL UCOPY (OUT,B,3)
      RETURN
C
C--                JACOBIAN
C--                0   -SIN L * COS PHI    -COS L * SIN PHI
C--                0   -SIN L * SIN PHI     COS L * COS PHI
C--                0        COS L                 0
C
         ENTRY ERDIRC                                                    CDC
C        ENTRY ERDIRC  (A,B)                                            -CDC
C
      CALL UZERO (F,1,9)
      F(2)= -SINL*COSP
      F(3)= -OUT(2)
      F(5)= -SINL*SINP
      F(6)=  OUT(1)
      F(8)=  COSL
      CALL MXMPY  (F,A,G,3,3,3)
      CALL MXMPY1 (G,F,B,3,3,3)
      RETURN
      END
