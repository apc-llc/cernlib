*
* $Id: izseta.F,v 1.1.1.1 1996/02/14 13:11:14 mclareni Exp $
*
* $Log: izseta.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:14  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/03 13/12/88  09.53.36  by  O.Couet
*-- Author :
      SUBROUTINE IZSETA
*.===========>
*.
*. This restores the attributes environment.
*.
*..==========> (O.Couet)
#include "higz/hiatnb.inc"
#include "higz/hisava.inc"
#include "higz/hiatnm.inc"
*.______________________________________
*
      DO 10 I=1,NBIATT
         CALL IGSET(CHIATT(I),FLOAT(ISAVA(I)))
 10   CONTINUE
      DO 20 I=1,NBRATT
         CALL IGSET(CHRATT(I),RSAVA(I))
 20   CONTINUE
*
      END
