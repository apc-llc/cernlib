*
* $Id: mllmdl.F,v 1.1.1.1 1996/02/26 17:16:42 mclareni Exp $
*
* $Log: mllmdl.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:42  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.05  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE MLLMDL(ITABL,IFLAG)
C   DELETE ALL RECORDS AND CONTROL PART OF BALANCED TREE
C   THE HEAD OF THE TABLE REMAINS !
      COMMON /MDPOOL/ IQ(6)
      ID=IQ(ITABL)
      IF(ID.EQ.0) RETURN
      IND=IQ(ID+1)
      IR=IQ(ID+7)
      IF(IR.LT.0) GO TO 4
      IR=IQ(IR)
      IF(IR.EQ.0) GO TO 4
      IBEG=MDLOC(IND,2)
      ILAST=IBEG
      IQ(IBEG)=IR
      IQ(IBEG+1)=0
      GO TO 2
    1 I=IQ(IBEG+1)
      CALL MDFREE(IND,IBEG)
      IF(I.EQ.0) GO TO 4
      IBEG=I
      IR=IQ(I)
    2 DO 3 J=IR,IR+2,2
      I=IQ(J)
      IF(I.EQ.0) GO TO 3
      IL=MDLOC(IND,2)
      IQ(ILAST+1)=IL
      IQ(IL)=I
      IQ(IL+1)=0
      ILAST=IL
    3 CONTINUE
      CALL MDFREE(IND,IR)
      GO TO 1
    4 IQ(ID+7)=-1
      IQ(ID+13)=0
      IF(IFLAG.EQ.0) RETURN
      CALL MDFREE(IND,ID)
      IQ(ITABL)=0
      RETURN
      END
