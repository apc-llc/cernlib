*
* $Id: mlltot.F,v 1.1.1.1 1996/02/26 17:16:43 mclareni Exp $
*
* $Log: mlltot.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:43  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.06  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MLLTOT(IBT,ITABL)
      COMMON /MDPOOL/ IQ(6)
      ICOUNT=0
      IBD=IQ(IBT)
      IF(IBD.EQ.0) GO TO 5
      LKEY=IQ(IBD+2)
      LINF=IQ(IBD+3)
      IOFF=IQ(IBD+11)
      ILK=3
      IF(LKEY.EQ.0) ILK=4
      IND=IQ(IBD+1)
      IR=IQ(IBD+7)
      IF(IR.LT.0) GO TO 5
      IR=IQ(IR)
      IF(IR.EQ.0) GO TO 5
      IBEG=MDLOC(IND,2)
      ILAST=IBEG
      IQ(IBEG)=IR
      IQ(IBEG+1)=0
      GO TO 2
    1 I=IQ(IBEG+1)
      CALL MDFREE(IND,IBEG)
      IF(I.EQ.0) GO TO 5
      IBEG=I
      IR=IQ(I)
    2 DO 3 I=IR,IR+2,2
      J=IQ(I)
      IF(J.EQ.0) GO TO 3
      IL=MDLOC(IND,2)
      IQ(ILAST+1)=IL
      IQ(IL)=J
      IQ(IL+1)=0
      ILAST=IL
    3 CONTINUE
      L=LINF
      IOF=IOFF
      IF(LKEY.EQ.0) IOF=IQ(IR+3)
      IF(L.EQ.0) L=IQ(IR+IOF-1)
    4 ICOUNT=ICOUNT+1
      IT=MLINS(ITABL,IQ(IR+ILK),L)
      I=MDLOC(IND,L)
      L=MLGET(IBT,IR+IOF,IQ(I),L)
      IF(L.NE.0) L=MLPUT(ITABL,IT,IQ(I),L)
      CALL MDFREE(IND,I)
      GO TO 1
    5 MLLTOT=ICOUNT
      RETURN
      END
