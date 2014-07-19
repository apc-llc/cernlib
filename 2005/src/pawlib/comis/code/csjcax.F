*
* $Id: csjcax.F,v 1.1.1.1 1996/02/26 17:16:28 mclareni Exp $
*
* $Log: csjcax.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:28  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.01  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSJCAX(J,NP,M)
***------------------------------
      INTEGER CSINT1
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
         PARAMETER (KLK=45, KCALLS=87, KEXIT=113)
         INTEGER M(2)
         DATA I/0/
       IF(I.EQ.0)I=MHLOC(64)
       K=I+2*NP
       IQ(K)=KCALLS
       IQ(K+1)=J
       IQ(K+2)=NP
       I3=K+3
       I2=I
       DO 1 K=1,NP
         IQ(I3)=1
         IQ(I2)=KLK
         IQ(I2+1)=M(K)
         I2=I2+2
   1  I3=I3+1
      IQ(I3)=KEXIT
      INT=CSINT1(I)
**      IF(INT.LE.0)THEN
**         CONTINUE
**      ENDIF
**      CALL MHFREE(I)
      END
