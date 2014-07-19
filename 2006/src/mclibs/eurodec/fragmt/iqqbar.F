*
* $Id: iqqbar.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: iqqbar.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION IQQBAR(IQ1,IQ2)
C.----------------------------------------------------------------------
C.
C.    DETERMINE THE MESON COMPOSED OF (ANTI)QUARKS IQ1 AND IQ2,
C.    THE PAIR Q2 - Q2BAR IS GENERATED FROM THE VACUUM.
C.    LAST UPDATE: 10/04/88
C.
C.----------------------------------------------------------------------
#include "eurodec/frgfix.inc"
C--
C--   GENERATE FLAVOUR (U, D, S) FROM VACUUM
      RQ=EURRAN(IQDUM)
      FRU=(1.-PROS)/2.
      FRD=2.*FRU
      IQ2=1
      IF (RQ.GT.FRU) IQ2=2
      IF (RQ.GT.FRD) IQ2=3
C--
C--   DETERMINE MESON
      IQQBAR=MESON(IABS(IQ1),IQ2)
      IF (IQ1.GT.0) THEN
        IQ2=-IQ2
      ELSE
        IF (IABS(IQ1).NE.IQ2) IQQBAR=-IQQBAR
      ENDIF
      RETURN
      END
