*
* $Id: sicmpr.F,v 1.1.1.1 1995/12/12 14:36:15 mclareni Exp $
*
* $Log: sicmpr.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:15  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.18  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SICMPR(L)
C
#include "sigma/sicph1.inc"
#include "sigma/sicph2.inc"
#include "sigma/sichel.inc"
      CHARACTER L*6
#include "sigma/sigc.inc"
      IF (SITRAK(5) .EQ. 0) GOTO 999
      WRITE(NPRINT,1) L,KS,NT,MG,MOR,KK,LITH,LIT,IST,KS,IC,IPOINT
    1 FORMAT(A6,I6,1X,I4,I5,1X,I4,2X,I4,3X,2A4,I5,3I4)
 999  END
