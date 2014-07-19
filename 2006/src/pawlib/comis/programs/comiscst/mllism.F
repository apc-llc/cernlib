*
* $Id: mllism.F,v 1.1.1.1 1996/02/26 17:16:42 mclareni Exp $
*
* $Log: mllism.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:42  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.13/00 27/03/92  14.15.43  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MLLISM(ID,KEY,L)
      INTEGER CIFNCL
      COMMON /MDPOOL/ IQ(6)
#include "comis/cssysd.inc"
      IF(IQ(ID+7).LT.0) IQ(ID+7)=ID+13
      IOFF=IQ(ID+11)
      LKEY=IQ(ID+2)
      I=MLLXSM(ID,KEY,IP,IT,IS)
      IF(I.EQ.0) GO TO 1
      IF(LKEY.EQ.0) IOFF=IQ(I+3)
      MLLISM=-I-IOFF
      RETURN
C   CREATE THE RECORD HEADER:
    1 LINF=IQ(ID+3)
      JK=MJCHAR(KEY)
      LR=LINF
      IF(LR.EQ.0) LR=L
      IF(IQ(ID+4).EQ.0) LR=2
      LK=LKEY
      IF(LKEY.NE.0) GO TO 2
      LK=MIKCHA(JK)
      IOFF=LK/4+5
      LK=LK+1
      IF(LINF.EQ.0) IOFF=IOFF+1
    2 I=MDLOC(IQ(ID+1),IOFF+LR)
      IQ(IP)=I
      IQ(I)=0
      IQ(I+1)=0
      IQ(I+2)=0
      J=I+3
      IF(LKEY.NE.0) GO TO 21
      IQ(J)=IOFF
      J=J+1
   21 CALL CCOPYS(JK,MJCHAR(IQ(J)),LK)
      IR=I+IOFF
      IF(LINF.EQ.0) IQ(IR-1)=L
      CALL CASETW(IQ(IR),LR,0)
      MLLISM=IR
C   IF IT IS THE FIRST RECORD IN TABLE -> RETURN
      IF(IS.EQ.0) RETURN
C   ELSE BEGIN BALANCING:
      JFUN=IQ(ID+10)
      IOF=3
      IF(LKEY.EQ.0) IOF=4
#if defined(CERNLIB_ASSEMB)
      IA=CIFNCL(JFUN,KEY,IQ(IS+IOF))
#endif
#if !defined(CERNLIB_ASSEMB)
      IA=CIFNCL(%VAL(JFUN),2,KEY,IQ(IS+IOF))
#endif
      IR=IQ(IS+IA+1)
      IP=IR
C   CORRECT THE BALANCING TAGS:
    3 IF(IP.EQ.I) GO TO 4
#if defined(CERNLIB_ASSEMB)
      IK=CIFNCL(JFUN,KEY,IQ(IP+IOF))
#endif
#if !defined(CERNLIB_ASSEMB)
      IK=CIFNCL(%VAL(JFUN),2,KEY,IQ(IP+IOF))
#endif
      IQ(IP+1)=IK
      IP=IQ(IP+IK+1)
      GO TO 3
C   CHECK IF BALANCING IS NEEDED:
    4 IK=IQ(IS+1)
      IF(IK.NE.0) GO TO 5
      IQ(IS+1)=IA
      IR=ID+12
      IQ(IR)=IQ(IR)+1
      RETURN
    5 IF(IK.EQ.IA) GO TO 6
      IQ(IS+1)=0
      RETURN
    6 IK=IR+1
      IF(IQ(IK).EQ.-IA) GO TO 7
C   ONE-TIME ROTATION:
      IP=IR
      ISS=IS+1
      IQ(ISS+IA)=IQ(IK-IA)
      IQ(IK-IA)=IS
      IQ(IK)=0
      IQ(ISS)=0
      GO TO 11
    7 IP=IQ(IK-IA)
C   TWO-TIMES ROTATION:
      ISS=IP+1
      IQ(IK-IA)=IQ(ISS+IA)
      IQ(ISS+IA)=IR
      IQ(IS+1+IA)=IQ(ISS-IA)
      IQ(ISS-IA)=IS
      IF(IQ(ISS).EQ.IA) GO TO 8
      IQ(IS+1)=0
      IF(IQ(ISS).EQ.0) GO TO 9
      IQ(IK)=IA
      GO TO 10
    8 IQ(IS+1)=-IA
    9 IQ(IK)=0
   10 IQ(ISS)=0
   11 IQ(IT)=IP
      RETURN
      END
