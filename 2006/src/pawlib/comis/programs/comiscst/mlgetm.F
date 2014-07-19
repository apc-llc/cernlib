*
* $Id: mlgetm.F,v 1.1.1.1 1996/02/26 17:16:43 mclareni Exp $
*
* $Log: mlgetm.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:43  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.06  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MLGETM(ID,IR,INF,L)
      COMMON /MDPOOL/ IQ(6)
      DIMENSION INF(1)
      MLGETM=0
      IF(L.LE.0) RETURN
      I=IR
      IF(IQ(ID+4).NE.0) GO TO 1
      I=IQ(IR)
      IF(I.EQ.0) RETURN
    1 LINF=IQ(ID+3)
      IF(LINF.EQ.0) LINF=IQ(IR-1)
      LEN=MIN0(L,LINF)
      CALL CCOPYA(IQ(I),INF(1),LEN)
      MLGETM=LEN
      RETURN
      END
