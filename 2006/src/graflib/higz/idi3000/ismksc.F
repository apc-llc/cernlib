*
* $Id: ismksc.F,v 1.1.1.1 1996/02/14 13:10:29 mclareni Exp $
*
* $Log: ismksc.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:29  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.06/06 09/03/89  15.50.20  by  O.Couet
*-- Author :
      SUBROUTINE ISMKSC(RMSCF)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Set the marker scale factor.
*.
*.   DI-3000 access:
*.   ---------------
*.     The marker SIZE cannot be changed in DI-3000.
*.     See JCMARK
*.
*.   Parameters:
*.   -----------
*.     No parameters.
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
*.______________________________________
*
      IF(RMSCF.EQ.RMKSC)RETURN
*
*           The marker size cannot be changed in DI-3000
*           But this marker size is usefull fo the HIGZ markers
*
      RMKSC=RMSCF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(IMKSCO)
#endif
*
      END
#endif
