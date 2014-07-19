*
* $Id: kaxshr.F,v 1.1.1.1 1996/03/08 11:40:53 mclareni Exp $
*
* $Log: kaxshr.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:53  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXSHR(IZ,IR,N)
*
*.....SHIFT DATA INTO THE RIGHT BROTHER OF A BLOCK
*
      INTEGER IR(*), IZ(*)
*
*-----------------------------------------------------------------------
*
      IF ( N .GT. 0 )  THEN
*
*........CREATE A GAP TO RECEIVE THE DATA
         IF ( IR(7)-IR(6) .GT. 0 )
     +      CALL UCOPY2( IR(IR(6)), IR(IR(6)+N), IR(7)-IR(6) )
         IR(7) = IR(7) + N
*
*........SHIFT DATA INTO THE GAP
         CALL UCOPY( IZ(IZ(7)-N), IR(IR(6)), N )
         IZ(7) = IZ(7) - N
      ENDIF
*
      END
