*
* $Id: label.F,v 1.1.1.1 1996/01/11 14:14:39 mclareni Exp $
*
* $Log: label.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:39  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION LABEL(IDEN)
C     ********************
C-- GIVES LABEL OF PARTICLE WITH CODE IDEN
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/data1.inc"
#include "cojets/data3.inc"
      CHARACTER*8 LABEL
      CHARACTER*8 BLANK
      CHARACTER*1 CN(5),BL,ACH,ZCH,DOT
      CHARACTER*5 CN5
      EQUIVALENCE (CN,CN5)
      DATA BL/' '/,ACH/'A'/,ZCH/'0'/,DOT/'.'/
      DATA BLANK/'        '/
      IDENA=ABS(IDEN)
      LABEL=BLANK
      IF(IDEN.EQ.0) RETURN
      IF(IDENA.GT.0.AND.IDENA.LE.6) THEN
        IF(IDEN.GT.0) LABEL=CHA4(IDENA,1)
        IF(IDEN.LT.0) LABEL=CHA4(IDENA,2)
      ELSEIF(IDEN.EQ.IDGL) THEN
        LABEL=CHAGL
      ELSEIF(MOD(IDENA,100).EQ.0) THEN
        K1=IDENA/1000
        K2=MOD(IDENA/100,10)
        IF(IDEN.GT.0) THEN
          CN(1)=CHA4(K1,1)
          CN(2)=CHA4(K2,1)
          CN(3)=ZCH
          CN(4)=DOT
          CN(5)=BL
        ELSE
          CN(1)=ACH
          CN(2)=CHA4(K1,1)
          CN(3)=CHA4(K2,1)
          CN(4)=ZCH
          CN(5)=DOT
        ENDIF
        LABEL=CN5
      ELSE
        IN=INTID(IDEN)
        IF(IN.NE.0) LABEL=CHA2(IN)
      ENDIF
      RETURN
      END
