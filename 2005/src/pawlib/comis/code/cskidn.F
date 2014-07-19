*
* $Id: cskidn.F,v 1.1.1.1 1996/02/26 17:16:18 mclareni Exp $
*
* $Log: cskidn.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:18  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.36  by  Rene Brun
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSKIDN(J,L)
***--------------------------
*      returns default type of ident
***--------------------------
      INTEGER CSILET,CSKIDE
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cspnts.inc"
      N=IQ(IBIMPL)
      IF(N.EQ.0)THEN
        CSKIDN=CSKIDE(J,L)
      ELSE
        K=MKCHAR(J)
        LI=CSILET(K)
        I=IBIMPL+1
        DO 1 K=1,N
        IF(LI.GE.IQ(I).AND.LI.LE.IQ(I+1))GO TO 2
  1     I=I+4
        CSKIDN=CSKIDE(J,L)
        GO TO 3
  2     CSKIDN=IQ(I+2)
        L=IQ(I+3)
  3     CONTINUE
      ENDIF
      END
