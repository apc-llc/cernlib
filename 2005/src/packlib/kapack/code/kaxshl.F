*
* $Id: kaxshl.F,v 1.1.1.1 1996/03/08 11:40:53 mclareni Exp $
*
* $Log: kaxshl.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:53  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXSHL(IZ,IL,N)
*
*.....SHIFT DATA INTO THE LEFT BROTHER OF A BLOCK
*
      INTEGER IL(*), IZ(*)
*
*-----------------------------------------------------------------------
*
      IF ( N .GT. 0 )  THEN
*
*........SHIFT DATA TO THE LEFT
         CALL UCOPY( IZ(IZ(6)), IL(IL(7)), N )
         IL(7) = IL(7) + N
*
*........CLOSE THE GAP
         IF ( IZ(7)-IZ(6)-N .GT. 0 )
     +      CALL UCOPY( IZ(IZ(6)+N), IZ(IZ(6)), IZ(7)-IZ(6)-N )
         IZ(7) = IZ(7) - N
      ENDIF
*
      END
