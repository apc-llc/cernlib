*
* $Id: kxdsig.F,v 1.1 1998/12/17 14:20:01 couet Exp $
*
* $Log: kxdsig.F,v $
* Revision 1.1  1998/12/17 14:20:01  couet
* - new routine to delete the temporary vectors created
*  by $SIGMA. On NT a direct call to kuvdel didn't work
*
*
#include "kuip/pilot.h"
*-- Author : O.Couet
      SUBROUTINE KXDSIG(NB)
*
********************************************************************************
*
* Delete the temporary vectors create after a $SIGMA command
* 
********************************************************************************
*
      CHARACTER*16 VNAME
      CHARACTER*8 CI
*
      VNAME = '?SIG'
      CI = ' '
      DO 20 I=1,NB
         WRITE (CI,'(I5)') I
         J = 1
 10      IF (CI(J:J).EQ.' ') THEN
            J=J+1
            GOTO 10
         ENDIF
         VNAME(5:) = CI(J:)
         CALL KUVDEL(VNAME)
 20   CONTINUE
*
      END
