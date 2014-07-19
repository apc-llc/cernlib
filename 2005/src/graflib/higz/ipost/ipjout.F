*
* $Id: ipjout.F,v 1.1.1.1 1996/02/14 13:11:06 mclareni Exp $
*
* $Log: ipjout.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:06  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.12/00 08/02/91  16.41.22  by  Unknown
*-- Author :
      SUBROUTINE IPJOUT(NX,NY)
*.===========>
*.
*.   Write coordinate pair (nx,ny) in LaTeX file.
*.
*..==========> (A.Nathaniel)
      CHARACTER*20 STR
*.______________________________________
*
      STR='('
      CALL IZITOC(NX,STR(2:))
      L=LENOCC(STR)
      STR(L+1:)=','
      CALL IZITOC(NY,STR(L+2:))
      L=LENOCC(STR)
      STR(L+1:)=')'
      CALL IPPSTR(STR)
*
      END
