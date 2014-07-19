*
* $Id: idiqrk.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: idiqrk.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION IDIQRK(IQ1,IQ2,IQ3,IFLAG)
C.----------------------------------------------------------------------
C.
C.    DETERMINE THE BARYON COMPOSED OF (ANTI)QUARKS IQ1, IQ2 AND IQ3,
C.    - IFLAG = 0: Q2Q3 - Q2BQ3B ARE GENERATED FROM VACUUM,
C.    - IFLAG = 1:   Q3 - Q3B ARE GENERATED FROM VACUUM.
C.    LAST UPDATE: 10/04/88
C.
C.----------------------------------------------------------------------
#include "eurodec/frgfix.inc"
      RQ=EURRAN(IQDUM)
      IF (IFLAG.EQ.1) THEN
C--
C--   GENERATE FLAVOUR (U, D, S) FROM VACUUM
        FRU=(1.-PROS)/2.
        FRD=2.*FRU
        IQ3=1
        IF (RQ.GT.FRU) IQ3=2
        IF (RQ.GT.FRD) IQ3=3
      ELSE
C--
C--   GENERATE DIQUARK COMBINATIONS
        FRUU=(1.-2.*PRUS-PRSS)/3.
        FRUD=FRUU+FRUU
        FRDD=FRUD+FRUU
        FRUS=FRDD+PRUS
        FRDS=FRUS+PRUS
        IF (RQ.LE.FRUU) THEN
          IQ2=1
          IQ3=1
        ELSEIF (RQ.LE.FRUD) THEN
          IQ2=1
          IQ3=2
        ELSEIF (RQ.LE.FRDD) THEN
          IQ2=2
          IQ3=2
        ELSEIF (RQ.LE.FRUS) THEN
          IQ2=1
          IQ3=3
        ELSEIF (RQ.LE.FRDS) THEN
          IQ2=2
          IQ3=3
        ELSE
          IQ2=3
          IQ3=3
        ENDIF
      ENDIF
C--
C--   DETERMINE BARYON
      IDIQRK=IBARYN(IABS(IQ1),IABS(IQ2),IQ3)
      IF (IQ1.LT.0) THEN
        IQ2=-IABS(IQ2)
        IQ3=-IQ3
        IDIQRK=-IDIQRK
      ENDIF
      RETURN
      END
