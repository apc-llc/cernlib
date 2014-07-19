*
* $Id: izsenv.F,v 1.1.1.1 1996/02/14 13:11:14 mclareni Exp $
*
* $Log: izsenv.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:14  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.06/03 19/01/89  10.58.13  by  O.Couet
*-- Author :
      SUBROUTINE IZSENV
*.===========>
*.
*.   This routine copies the graphic environnement in the current picture .
*.
*..==========> (O.Couet)
#include "higz/hiatnb.inc"
*.______________________________________
*
*              Real attributes .
*
      DO 10 I=50,50+NBRATT-1
         CALL IZATT(I)
 10   CONTINUE
*
*              Integer attributes .
*
      DO 20 I=80,80+NBIATT-1
         CALL IZATT(I)
 20   CONTINUE
*
      END
#endif
