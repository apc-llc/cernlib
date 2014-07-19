*
* $Id: izsava.F,v 1.1.1.1 1996/02/14 13:11:13 mclareni Exp $
*
* $Log: izsava.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:13  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/03 12/12/88  17.12.44  by  O.Couet
*-- Author :
      SUBROUTINE IZSAVA
*.===========>
*.
*. This saves the attributes environment.
*.
*..==========> (O.Couet)
#include "higz/hiatnb.inc"
#include "higz/hiatt.inc"
#include "higz/hisava.inc"
      DIMENSION RATVAL(NBRATT)
      EQUIVALENCE (RATVAL(1),RMKSC)
      DIMENSION IATVAL(NBIATT)
      EQUIVALENCE (IATVAL(1),IPLCI)
*.______________________________________
*
      DO 10 I=1,NBIATT
         ISAVA(I)=IATVAL(I)
 10   CONTINUE
      DO 20 I=1,NBRATT
         RSAVA(I)=RATVAL(I)
 20   CONTINUE
*
      END
