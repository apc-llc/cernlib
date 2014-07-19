*
* $Id: mlbisp.F,v 1.1.1.1 1996/02/26 17:16:43 mclareni Exp $
*
* $Log: mlbisp.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:43  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.06  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MLBISP(ID,KEY,L)
      COMMON /MDPOOL/ IQ(6)
      JAL=IQ(ID+9)
      IPAG=IQ(ID+6)
      IOFF=IQ(ID+11)
      LKEY=IQ(ID+2)
      I=ID+7
      IF(IQ(I).GE.0) GO TO 1
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB_ASSEMB))
      CALL CSBCLL(JAL,0,IPAG,IQ(I),1)
#endif
#if (defined(CERNLIB_VAX))&&(!defined(CERNLIB_ASSEMB))
      CALL CSBCLL(%VAL(JAL),4,0,IPAG,IQ(I),1)
#endif
      CALL MPSETW(IPAG,IQ(I),0)
    1 I=MLBXSP(ID,IA,IR,KEY)
      IF(I.EQ.0) GO TO 2
      IF(LKEY.EQ.0) IOFF=IQ(I+2)
      MLBISP=-I-IOFF
      RETURN
    2 I=ID+13
      CALL CASETW(IQ(I),2,0)
      LINF=IQ(ID+3)
      LR=LINF
      IF(LR.EQ.0) LR=L
      IF(IQ(ID+4).EQ.0) LR=2
      LK=LKEY
      JK=MJCHAR(KEY)
      IF(LKEY.NE.0) GO TO 3
      LK=MIKCHA(JK)
      IOFF=LK/4+5
      LK=LK+1
      IF(LINF.EQ.0) IOFF=IOFF+1
    3 LREC=IOFF+LR
      J=I+2
      IF(LKEY.NE.0) GO TO 4
      IQ(J)=IOFF
      J=J+1
    4 CALL CCOPYS(JK,MJCHAR(IQ(J)),LK)
      J=I+IOFF
      INAD=J-1
      IF(LINF.NE.0) GO TO 5
      IQ(J-1)=L
      INAD=INAD-1
    5 CALL CASETW(IQ(J),LR,0)
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB_ASSEMB))
      CALL CSBCLL(JAL,0,IPAG,IR,LREC)
#endif
#if (defined(CERNLIB_VAX))&&(!defined(CERNLIB_ASSEMB))
      CALL CSBCLL(%VAL(JAL),4,0,IPAG,IR,LREC)
#endif
      CALL MPSETW(IPAG,IA,IR)
      IQ(INAD)=IR+IOFF
      CALL COPYMP(IPAG,IQ(I),IR,LREC)
      MLBISP=J
      RETURN
      END
