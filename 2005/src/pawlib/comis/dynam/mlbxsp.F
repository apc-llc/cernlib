*
* $Id: mlbxsp.F,v 1.1.1.1 1996/02/26 17:16:57 mclareni Exp $
*
* $Log: mlbxsp.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:57  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MLBXSP(ID,IA,IR,KEY)
C      INTEGER CIFNCL
      COMMON /MDPOOL/ IQ(6)
      MLBXSP=0
      I=ID+13
      LBUF=IQ(I-1)
      LKEY=IQ(ID+2)
      J=I+2
      IF(LKEY.EQ.0) J=J+1
      IR=IQ(ID+7)
      IF(IR.LT.0) RETURN
      IPAG=IQ(ID+6)
      K=0
      IQ(I)=MIMPJW(IPAG,IR)
    1 IA=IR+K
      IR=IQ(I+K)
      IF(IR.EQ.0) RETURN
      CALL COPYPM(IPAG,IR,IQ(I),LBUF)
      IF(MLCMP8(KEY,IQ(J))) 2,4,3
    2 K=0
      GO TO 1
    3 K=1
      GO TO 1
    4 MLBXSP=I
      RETURN
      END
