*
* $Id: ibaryn.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: ibaryn.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION IBARYN(IQ1,IQ2,IQ3)
C.----------------------------------------------------------------------
C.
C.    DETERMINE THE BARYON COMPOSED OF QUARKS IQ1, IQ2 AND IQ3.
C.    RETURNS PARTICLE CODE: IBARYN.
C.    LAST UPDATE: 14/02/89
C.
C.----------------------------------------------------------------------
#include "eurodec/frgfix.inc"
C--
C--   THE PROBABILITY FOR A SPIN-1/2 BARYON IS GIVEN BY 'PROPSE',
      IF ((IQ1.EQ.IQ2).AND.(IQ1.EQ.IQ3)) THEN
        IBARYN=IQ1*1110+1
      ELSE
        IQMAX=MAX(IQ1,IQ2,IQ3)
        IQMIN=MIN(IQ1,IQ2,IQ3)
        IQMID=IQ1+IQ2+IQ3-IQMAX-IQMIN
        ISPIN=0
        IF (IQMAX.LT.4) THEN
          IF (EURRAN(IPRDUM).GT.PROPSE(1)) ISPIN=1
        ELSE
          IF (EURRAN(IPRDUM).GT.PROPSE(2)) ISPIN=1
        ENDIF
C--
C--   FOR HEAVY SPIN 0 BARYONS THE LAST TWO QUARKS ARE NOT ORDERED
C--   THIS IS ALSO TRUE FOR THE LAMBDA: 3120!
        IF ((IQMAX.GT.IQMID).AND.(ISPIN.EQ.0)) THEN
          IF (EURRAN(IRDUM).LE.0.5) THEN
            IQINT=IQMID
            IQMID=IQMIN
            IQMIN=IQINT
          ENDIF
        ENDIF
        IBARYN=IQMAX*1000+IQMID*100+IQMIN*10+ISPIN
      ENDIF
      RETURN
      END
