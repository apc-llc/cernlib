*
* $Id: sqd.F,v 1.1.1.1 1996/03/08 16:58:51 mclareni Exp $
*
* $Log: sqd.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:51  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION SQD(X)
C.----------------------------------------------------------------------
C.
C.    INTEGRAND FOR SINGLE QUARK HEAVY ONIUM DECAYS
C.    LAST UPDATE: 14/08/89
C.
C.----------------------------------------------------------------------
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION X,SQD
#endif
#include "eurodec/sqdpar.inc"
      SQD=(OMXRH2+X*(1.+XMU-2.*X))*SQRT(OMXRH2+X*(X-2.*XMU-2.))
     &/((1.-X*XRH)**2+XGW)
      RETURN
      END
