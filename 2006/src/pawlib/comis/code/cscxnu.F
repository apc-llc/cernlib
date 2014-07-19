*
* $Id: cscxnu.F,v 1.1.1.1 1996/02/26 17:16:22 mclareni Exp $
*
* $Log: cscxnu.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:22  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 17/02/94  18.17.28  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSCXNU(JJ,NN,CX)
******
*    it parses complex constant i.e. ( 1. , 2. )
*    cscxnu=
*            -15 if was error in number
*              0 if it is not cx-const
*              7 if it is cx-const
******
#include "comis/csichv.inc"
      COMPLEX CX
      INTEGER CSSGNU
      REAL RE,IM
      J=JJ
      N=NN
      JGSST=JJ
      NGSST=NN
      CSCXNU=0
      KCH=MKBLAN(J,N)
      IF(KCH.NE.ICHBRA)RETURN
      J=J+1
      N=N-1
      I=CSSGNU(J,N,RE)
      IF(I.EQ.0)THEN
        GO TO 1
      ELSE IF(I.LT.0)THEN
        CSCXNU=I
        JJ=J
        NN=N
        RETURN
      END IF
      KCH=MKBLAN(J,N)
      IF(KCH.NE.ICHCOMM)GO TO 1
      J=J+1
      N=N-1
      CSCXNU=-15
      JJ=J
      NN=N
      KCH=MKBLAN(J,N)
      IF(N.LE.0)RETURN
      I=CSSGNU(J,N,IM)
      IF(I.LE.0)RETURN
      KCH=MKBLAN(J,N)
      IF(KCH.NE.ICHKET)RETURN
      JJ=J+1
      NN=N-1
      CSCXNU=7
      CX=CMPLX(RE,IM)
      RETURN
 1    JJ=JGSST
      NN=NGSST
      END
