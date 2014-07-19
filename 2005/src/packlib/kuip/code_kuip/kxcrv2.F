*
* $Id: kxcrv2.F,v 1.1.1.1 1996/03/08 15:32:54 mclareni Exp $
*
* $Log: kxcrv2.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:54  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  1.68/00 05/12/91  17.42.09  by  Fons Rademakers
*-- Author :
      SUBROUTINE KXCRV2(VNAME,LENGTH,TYPE)
*
********************************************************************************
*
* Called by KXCRV1
*
*     Input :
*
      CHARACTER*(*) VNAME
      INTEGER LENGTH(3)
      CHARACTER*(*) TYPE
*
********************************************************************************
*
#include "kuip/kcmcmd.inc"
#include "kuip/kcgen.inc"
#include "kuip/kcgen2.inc"
#include "kuip/kcques.inc"
#include "kuip/kcvect.inc"
*     CHARACTER*38 PAR
*     CHARACTER*15 VAL
      CHARACTER*32 VNAMB,B32
      PARAMETER (B32=' ')
      INTEGER VNAMH(8),VTYPH
*
      IQUEST(1)=0
      NCHNAM=LENOCC(VNAME)
      I1=INDEX(VNAME,'(')
      IF (I1.GT.1) NCHNAM=I1-1

      IF (LENGTH(1).LE.0.OR.LENGTH(2).LE.0.OR.LENGTH(3).LE.0) THEN
        IQUEST(1)=1
        CALL KUALFA
        PRINT *,'*** Vector length less than or equal to 0 - ',
     +          'Vector not created'
        GO TO 999
      ENDIF

      IF (TYPE.NE.'R'.AND.TYPE.NE.'I'.AND.TYPE.NE.'C') THEN
        IQUEST(1)=1
        CALL KUALFA
        PRINT *,'*** Unknown vector type ''',TYPE(1:1),''' - ',
     +          'Vector not created'
        GO TO 999
      ENDIF
*
* Check that vector is not already existing
*
      LVECN=LQ(LVEC-1)
      DO 10 I=1,NUMVEC
        CALL UHTOC(IQ(LVECN+1),4,VNAMB,32)
        IF (VNAMB.EQ.VNAME) THEN
          IQUEST(1)=1
          CALL KUALFA
          PRINT *,'*** Vector ',VNAME(1:NCHNAM),' already existing'
          GO TO 999
        ENDIF
        LVECN=LQ(LVECN)
10    CONTINUE
*
* Check if there is enough space
*
      LENTOT=LENGTH(1)*LENGTH(2)*LENGTH(3)
      IF (TYPE.EQ.'C') LENTOT=LENTOT*2
      LLL=50+LENTOT
      CALL MZNEED(IXKUIP,LLL,'G')
      IQUEST(1)=0
      IF (IQUEST(11).LT.0) THEN
        PRINT *,'*** Not enough memory - Vector not created'
        IQUEST(1)=1
        GO TO 999
      ENDIF
*
      NL=0
      NS=0
      ND=14+LENTOT
      IF (NUMVEC.EQ.0) THEN
        CALL MZBOOK(IXKUIP,LVECN,LVEC,-1,'VECN',NL,NS,ND,IOVECN,0)
      ELSE
        LVECN=LZLAST(IXKUIP,LVEC-1)
*----------------------------------------------------------------------v
*       PRINT *,'------------------ Before MZBOOK'
*       CALL DZVERI('Before garbage',IXKUIP,'CFLSU')
*       CALL MZGARB(IXKUIP,0)
*       CALL DZVERI('After garbage',IXKUIP,'CFLSU')
*----------------------------------------------------------------------^
        CALL MZBOOK(IXKUIP,LVECN,LVECN,0,'VECN',NL,NS,ND,IOVECN,0)
*----------------------------------------------------------------------v
*       PRINT *,'------------------ After  MZBOOK'
*       CALL DZVERI('Before garbage',IXKUIP,'CFLSU')
*       CALL MZGARB(IXKUIP,0)
*       CALL DZVERI('After garbage',IXKUIP,'CFLSU')
*----------------------------------------------------------------------^
      ENDIF
      NUMVEC=NUMVEC+1
      VNAMB=VNAME(1:NCHNAM)//B32
      CALL UCTOH(VNAMB,VNAMH,4,32)
      DO 20 I=1,8
        IQ(LVECN+I)=VNAMH(I)
20    CONTINUE
      CALL UCTOH('    ',VTYPH,4,4)
      CALL UCTOH(TYPE  ,VTYPH,4,1)
      IQ(LVECN+9)=VTYPH
      IQ(LVECN+10)=LENTOT
      IQ(LVECN+11)=LENGTH(1)
      IQ(LVECN+12)=LENGTH(2)
      IQ(LVECN+13)=LENGTH(3)
      DO 30 NDIMS=3,1,-1
        IF (LENGTH(NDIMS).GT.1) GO TO 40
30    CONTINUE
      NDIMS=1
40    IQ(LVECN+14)=NDIMS
*
999   END



