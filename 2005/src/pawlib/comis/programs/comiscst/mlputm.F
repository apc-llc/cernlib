*
* $Id: mlputm.F,v 1.1.1.1 1996/02/26 17:16:43 mclareni Exp $
*
* $Log: mlputm.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:43  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.06  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MLPUTM(ID,IR,INF,L)
      COMMON /MDPOOL/ IQ(6)
      DIMENSION INF(1)
      MLPUTM=0
      IF(L.LE.0) RETURN
      I=IR
      LEN=IQ(ID+3)
      IF(LEN.EQ.0) LEN=IQ(IR-1)
      IF(IQ(ID+4).NE.0) GO TO 1
      I=IQ(IR)
      IF(I.NE.0) GO TO 1
      I=MDLOC(IQ(ID+6),LEN)
      CALL CASETW(IQ(I),LEN,0)
      IQ(IR)=I
    1 LEN=MIN0(L,LEN)
      CALL CCOPYA(INF(1),IQ(I),LEN)
      MLPUTM=LEN
      RETURN
      END
