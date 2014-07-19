*
* $Id: mlbinp.F,v 1.1.1.1 1996/02/26 17:16:57 mclareni Exp $
*
* $Log: mlbinp.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:57  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE MLBINP(ITABL,IND,LKEY,LINF,LFLAG,ID,IFUNC,JALFR)
      COMMON /MDPOOL/ IQ(6)
      LIA=IABS(LINF)
      IF(LFLAG.EQ.0) LIA=2
      LKA=IABS(LKEY)+3
      IF(LKEY.LE.0) LKA=LKA+1
      LKA=LKA/4
      LREC=LIA+LKA+3
      IF(LKEY.LE.0) LREC=LREC+1
      IF(LINF.LE.0) LREC=LREC+1
      LIA=LREC+13
      I=MDLOC(IND,LIA)
      IQ(ITABL)=I
      IQ(I)=LIA
      IQ(I+1)=IND
      IQ(I+2)=MAX0(LKEY,0)
      IQ(I+3)=MAX0(LINF,0)
      IQ(I+4)=LFLAG
      IQ(I+5)=1
      IQ(I+6)=ID
      IQ(I+7)=-1
      IQ(I+8)=0
      IQ(I+9)=JALFR
      IQ(I+10)=JUMPAD(IFUNC)
      IQ(I+11)=0
      IQ(I+12)=LREC
      CALL VFILL(IQ(I+13),LREC,0)
      IF(LKEY.LE.0) RETURN
      LKA=(LKEY+3)/4
      IF(LINF.LE.0) LKA=LKA+1
      IQ(I+11)=LKA+3
      RETURN
      END
