*
* $Id: iclks.F,v 1.1.1.1 1996/02/14 13:10:27 mclareni Exp $
*
* $Log: iclks.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:27  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.06/06 09/03/89  15.02.58  by  O.Couet
*-- Author :
      SUBROUTINE ICLKS
*.===========>
*.
*.   Purpose:
*.   --------
*.     Terminate DI-3000.
*.     User does not need to call this routine, but should call IGEND directly.
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/di3dev.inc"
*.______________________________________
*
      IF (ENTRY_FLAG) CALL JEND
      ENTRY_FLAG = .FALSE.
*
      END
#endif
