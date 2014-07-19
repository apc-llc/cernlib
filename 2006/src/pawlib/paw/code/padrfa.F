*
* $Id: padrfa.F,v 1.1.1.1 1996/03/01 11:38:39 mclareni Exp $
*
* $Log: padrfa.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:39  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/14 27/09/93  12.42.33  by  O.Couet
*-- Author :    O.Couet   15/09/92
      SUBROUTINE PADRFA(ICODES,XYZ,NP,IFACE,T)
*.===========>
*.
*. Draw the faces for the Gouraud Shaded Ntuples
*.
*..==========> (O.Couet)
#include "paw/pcolv.inc"
      REAL      XYZ(3,*),P3(3,12),X(12),Y(12),T(*)
      INTEGER   ICODES(*),IFACE(*)
*.______________________________________
*
      IF(ICODES(1).EQ.1)CALL IHCSPE(NCOLOR,FMIN,FMAX,IC1,1,IREP)
      IF(ICODES(1).EQ.2)CALL IHCSPE(NCOLOR,FMIN,FMAX,IC2,1,IREP)
      IF(ICODES(1).EQ.3)CALL IHCSPE(NCOLOR,FMIN,FMAX,IC3,1,IREP)
      DO 10 I=1,NP
        K       = IFACE(I)
        IF(K.LT.0) K = -K
        CALL IHWTON(XYZ(1,K),P3(1,I))
        X(I)   = P3(1,I)
        Y(I)   = P3(2,I)
  10  CONTINUE
*
      CALL IHPFF(NP,P3,T)
*
      END
