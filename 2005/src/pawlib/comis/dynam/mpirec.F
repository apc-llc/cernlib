*
* $Id: mpirec.F,v 1.1.1.1 1996/02/26 17:16:57 mclareni Exp $
*
* $Log: mpirec.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:57  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MPIREC(IND,IAD,NRES)
C      INTEGER CIFNCL
      EXTERNAL MPDSK
      COMMON /MDPOOL/ IQ(6)
      COMMON /MPNORD/ NOREAD
C+SELF,IF=APOLLO,IF=-ASSEMB.
C      INTEGER JPNTR
C      POINTER/JPNTR/JNAME
C+SELF.
      DATA KEY/0/
      IF(KEY.EQ.0) GO TO 20
      CALL MDPAUS('?ILLEGAL TRAP IN PAGING MEMORY')
   20 KEY=1
      IADR=IAD
      IF(IADR.LT.0) IADR=-IADR-1
      NRSIZ=IQ(IND+3)
      JT=IQ(IND+12)
      NUSP=IQ(IND+6)
      NR=IADR/NRSIZ+IQ(IND+1)
      IN=MOD(IADR,NRSIZ)
      NRES=NRSIZ-IN
      IF(IQ(IND+2).LE.0) GO TO 1
      IF(NR.LT.IQ(IND+2)+IQ(IND+1)) GO TO 1
      CALL MDPAUS('?RECORD OUT OF BOUNDS')
    1 J=IQ(IND+9)
      IF(NUSP.EQ.0) GO TO 4
      DO 2 K=1,NUSP
      L=J
      J=IQ(J+2)
      IF(NR.EQ.IQ(J+1)) GO TO 7
    2 CONTINUE
      IF(JT.LE.0) GO TO 11
C+SELF,IF=VAX,IBM,APOLLO,IF=ASSEMB.
C      IF(CIFNCL(JT,IND,NR).EQ.0) GO TO 11
C+SELF,IF=APOLLO,IF=-ASSEMB.
C      JPNTR=JT
C      IF(CIFNCL(JNAME,2,IND,NR).EQ.0) GO TO 11
C+SELF,IF=VAX,IF=-ASSEMB.
C      IF(CIFNCL(%VAL(JT),2,IND,NR).EQ.0) GO TO 11
C+SELF.
      JT=0
      GO TO 1
   11 IF(NUSP.LT.IQ(IND+4)) GO TO 4
      IF(IQ(J).GT.0) GO TO 3
      CALL MPDSK(1,IQ(IND),IQ(J+1),IQ(J+4),NRSIZ)
      IQ(IND+11)=IQ(IND+11)+1
      IQ(J)=-IQ(J)
    3 IF(NUSP.EQ.1) GO TO 6
      IQ(IND+9)=L
      IQ(IND+7)=J
      GO TO 6
    4 L=J
      J=MDLOC(IQ(IND+5),NRSIZ+4)
      NUSP=NUSP+1
      IQ(J)=NUSP
      IQ(IND+6)=NUSP
      IQ(J+3)=IQ(IND+8)
      IQ(IND+8)=J
      IF(NUSP.EQ.1) GO TO 5
      IQ(L+2)=J
      IQ(J+2)=IQ(IND+7)
      IQ(IND+7)=J
      GO TO 6
    5 IQ(IND+7)=J
      IQ(IND+8)=J
      IQ(IND+9)=J
      IQ(J+2)=J
    6 IQ(J+1)=NR
      IF(IQ(IND+15).EQ.0) IQ(IND+15)=JUMPAD(MPDSK)
      IF(NOREAD.NE.0) GO TO 10
      CALL MPDSK(0,IQ(IND),NR,IQ(J+4),NRSIZ)
      IQ(IND+10)=IQ(IND+10)+1
      GO TO 10
    7 IF(J.EQ.IQ(IND+7)) GO TO 10
      IF(J.EQ.IQ(IND+9)) GO TO 8
      IQ(L+2)=IQ(J+2)
      L=IQ(IND+9)
      IQ(L+2)=J
      IQ(J+2)=IQ(IND+7)
      GO TO 9
    8 IQ(IND+9)=L
    9 IQ(IND+7)=J
   10 MPIREC=J+IN+4
      KEY=0
      IF(IAD.GT.0) RETURN
      IQ(J)=ISIGN(IQ(J),-1)
      RETURN
      END
