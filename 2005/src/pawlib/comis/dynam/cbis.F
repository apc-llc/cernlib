*
* $Id: cbis.F,v 1.1.1.1 1996/02/26 17:16:57 mclareni Exp $
*
* $Log: cbis.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:57  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_IBM)||defined(CERNLIB_APOLLO)||!defined(CERNLIB_ASSEMB)
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CBIS(I,J,N)
C===========================================
C   CONVERTS INTEGER NUMBER I TO SYMBOLIC
C   REPRESENTATION, PLACED IN STRING (J,N).
C   J AND N MUST BE VARYABLES.
C===========================================
      DIMENSION M(30)
      DATA IZERO,MINUS,STAR/4H0   ,4H-   ,4H*   /
      NN=N
      IF(N.LE.0)NN=1
      IS=0
      IF(I.NE.0)GO TO 1
      IS=IS+1
      M(IS)=IZERO
      GO TO 3
    1 IN=I
      IF(IN.LT.0)IN=-I
    2 KD=IN-IN/10*10
      K=MKDIGI(KD)
      IS=IS+1
      M(IS)=K
      IN=IN/10
      IF(IN.NE.0)GO TO 2
      IF(I.GT.0)GO TO 3
      IS=IS+1
      M(IS)=MINUS
    3 IF(NN.GE.IS)GO TO 5
      IS=NN
      CALL CSETC(M(NN),STAR)
    5 J1=J
    4 K=M(IS)
      IS=IS-1
      CALL CSCHAR(J,K)
      J=J+1
      IF(IS.GT.0)GO TO 4
      N=NN-J+J1
      RETURN
      END
#endif
