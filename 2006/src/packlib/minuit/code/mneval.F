*
* $Id: mneval.F,v 1.1.1.1 1996/03/07 14:31:29 mclareni Exp $
*
* $Log: mneval.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:29  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNEVAL(FCN,ANEXT,FNEXT,IEREV,FUTIL)
#include "minuit/d506dp.inc"
CC      Evaluates the function being analyzed by MNCROS, which is
CC      generally the minimum of FCN with respect to all remaining
CC      variable parameters.  Common block /MN7XCR/ contains the
CC      data necessary to know the values of U(KE1CR) and U(KE2CR)
CC      to be used, namely     U(KE1CR) = XMIDCR + ANEXT*XDIRCR
CC      and (if KE2CR .NE. 0)  U(KE2CR) = YMIDCR + ANEXT*YDIRCR
#include "minuit/d506cm.inc"
CC
      EXTERNAL FCN,FUTIL
                          U(KE1CR) = XMIDCR + ANEXT*XDIRCR
      IF ( KE2CR .NE. 0)  U(KE2CR) = YMIDCR + ANEXT*YDIRCR
      CALL MNINEX(X)
      NPARX = NPAR
      CALL FCN(NPARX,GIN,FNEXT,U,4,FUTIL)
      NFCN = NFCN + 1
      IEREV = 0
      IF (NPAR .GT. 0)  THEN
         ITAUR = 1
         AMIN = FNEXT
         ISW(1) = 0
         CALL MNMIGR(FCN,FUTIL)
         ITAUR = 0
         FNEXT = AMIN
         IF (ISW(1) .GE. 1)  IEREV = 1
         IF (ISW(4) .LT. 1)  IEREV = 2
      ENDIF
      RETURN
      END
