*
* $Id: kxcrv1.F,v 1.1.1.1 1996/03/08 15:32:54 mclareni Exp $
*
* $Log: kxcrv1.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:54  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  1.61/01 23/06/89  09.49.19  by
*-- Author :
      SUBROUTINE KXCRV1(VNAME,TYPE)
*
********************************************************************************
*
* Called by KXCREV
*
*     Input :
*
      CHARACTER*(*) VNAME
      CHARACTER*(*) TYPE
*
********************************************************************************
*
#include "kuip/kcques.inc"
      CHARACTER*32 VNAM
      DIMENSION LENFR(3),LENTO(3)
*
      IQUEST(1)=0
      NCHNAM=LENOCC(VNAME)
      I1=INDEX(VNAME,'(')
      I2=INDEX(VNAME,')')
      IF ((I1.EQ.0.AND.I2.EQ.0).OR.(I2.EQ.I1+1)) THEN
        LENFR(1)=1
        LENFR(2)=1
        LENFR(3)=1
        LENTO(1)=1
        LENTO(2)=1
        LENTO(3)=1
        I1=NCHNAM+1
        GO TO 20
      ENDIF
      IF (I1.GT.I2) GO TO 910
      IF (I1.EQ.1.OR.I2.NE.NCHNAM) GO TO 910

      NCHSUB=I2-I1-1
      LENTO(1)=1
      LENTO(2)=1
      LENTO(3)=1
      IF (NCHSUB.GT.0) THEN
        CALL KIVIND(VNAME(I1+1:I2-1),NCHSUB,LENFR,LENTO)
        IF (IQUEST(1).NE.0) GO TO 999
      ENDIF

      DO 10 I=1,3
        IF (LENFR(I).LE.0.OR.LENTO(I).LE.0) GO TO 920
        IF (LENTO(I).GT.LENFR(I)) LENFR(I)=LENTO(I)
10    CONTINUE

20    VNAM=' '
      VNAM=VNAME(1:I1-1)
      CALL KXCRV2(VNAM,LENFR,TYPE)
      GO TO 999

910   IQUEST(1)=1
      CALL KUALFA
      PRINT *,'*** Illegal syntax in vector name',
     + ' - Vector not created'
      GO TO 999

920   IQUEST(1)=1
      CALL KUALFA
      PRINT *,'*** Vector dimensions cannot be less than or equal to 0',
     + ' - Vector not created'
      GO TO 999

999   END



