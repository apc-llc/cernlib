*
* $Id: mlputp.F,v 1.1.1.1 1996/02/26 17:16:44 mclareni Exp $
*
* $Log: mlputp.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:44  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.06  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MLPUTP(ID,IR,INF,L)
      COMMON /MDPOOL/ IQ(6)
      DIMENSION INF(1)
      MLPUTP=0
      IF(L.LE.0) RETURN
      IPAG=IQ(ID+6)
      LPAG=IQ(ID+5)
      LINF=IQ(ID+3)
      IAD=IR-1
      IF(LINF.NE.0) GO TO 1
      IAD=IAD-1
      LINF=IQ(IR-1)
    1 LINF=MIN0(LINF,L)
      IF(LPAG.EQ.0) GO TO 2
      IF(IQ(ID+4).EQ.0) GO TO 2
      CALL CCOPYA(INF(1),IQ(IR),LINF)
      CALL COPYMP(IPAG,INF(1),IQ(IAD),LINF)
      GO TO 4
    2 IF(IQ(IR).NE.0) GO TO 3
#if defined(CERNLIB_ASSEMB)
      CALL CSBCLL(IQ(ID+9),0,IPAG,IQ(IR),LINF)
#endif
#if !defined(CERNLIB_ASSEMB)
      CALL CSBCLL(%VAL(IQ(ID+9)),4,0,IPAG,IQ(IR),LINF)
#endif
      IF(LPAG.NE.0) CALL COPYMP(IPAG,IQ(IR),IQ(IAD),2)
    3 CALL COPYMP(IPAG,INF(1),IQ(IR),LINF)
    4 MLPUTP=LINF
      RETURN
      END
