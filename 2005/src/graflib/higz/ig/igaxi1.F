*
* $Id: igaxi1.F,v 1.1.1.1 1996/02/14 13:10:33 mclareni Exp $
*
* $Log: igaxi1.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:33  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/03 17/10/88  13.48.10  by  Rene Brun
*-- Author :
      SUBROUTINE IGAXI1(X,Y,CFI,SFI,XT,YT,U,V)
*.==========>
*.
*.   Underlaying routine IGAXIS .
*.
*..=========>
      U=CFI*X-SFI*Y+XT
      V=SFI*X+CFI*Y+YT
*
      END


