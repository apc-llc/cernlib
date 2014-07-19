*
* $Id: afromi.F,v 1.1.1.1 1996/02/15 17:53:25 mclareni Exp $
*
* $Log: afromi.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:25  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION AFROMI  (K)
C
C
      EQUIVALENCE (BIN,IBIN)
C
      DIMENSION ALPHA(64)
      DATA  ALPHA /                   1H0,
     +    1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,
     +    1H9,1H ,1H=,1H",1H ,1H ,1H ,1H+,
     +    1HA,1HB,1HC,1HD,1HE,1HF,1HG,1HH,
     +    1HI,1H ,1H.,1H),1H ,1H ,1H ,1H-,
     +    1HJ,1HK,1HL,1HM,1HN,1HO,1HP,1HQ,
     +    1HR,1H ,1H$,1H*,1H ,1H ,1H ,1H ,
     +    1H/,1HS,1HT,1HU,1HV,1HW,1HX,1HY,
     +    1HZ,1H ,1H,,1H(,1H ,1H ,1H /
C
C
C--                ENTRY AFROMI
C
      L=K
      IF ((L.LT.0) .OR. (L.GT.63))  L=48
      BIN= ALPHA(L+1)
      GO TO 99
C
         ENTRY IFROMA                                                    CDC
C        ENTRY IFROMA  (K)                                              -CDC
C
      IBIN= K
      I= IUCOMP (BIN,ALPHA,64)
      IF (I.EQ.0)   I=49
      IF (I.EQ.11)  I=49
      IBIN= I-1
   99 AFROMI=  BIN                                                      -IBM
C  99 IFROMA= IBIN                                                       IBM
      RETURN
      END
