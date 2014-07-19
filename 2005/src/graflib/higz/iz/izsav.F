*
* $Id: izsav.F,v 1.1.1.1 1996/02/14 13:11:13 mclareni Exp $
*
* $Log: izsav.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:13  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/03 12/12/88  17.12.44  by  O.Couet
*-- Author :
      SUBROUTINE IZSAV
*.===========>
*.
*. This saves the attributes environment.
*.
*..==========> (O.Couet)
#include "higz/hiatnb.inc"
#include "higz/hiatt.inc"
#include "higz/hisave.inc"
      DIMENSION RATVAL(NBRATT)
      EQUIVALENCE (RATVAL(1),RMKSC)
      DIMENSION IATVAL(NBIATT)
      EQUIVALENCE (IATVAL(1),IPLCI)
*.______________________________________
*
      DO 10 I=1,NBIATT
         ISAVE(I)=IATVAL(I)
 10   CONTINUE
      DO 20 I=1,NBRATT
         RSAVE(I)=RATVAL(I)
 20   CONTINUE
      WNVPSV(1)=RWXMIN
      WNVPSV(2)=RWXMAX
      WNVPSV(3)=RWYMIN
      WNVPSV(4)=RWYMAX
      WNVPSV(5)=RVXMIN
      WNVPSV(6)=RVXMAX
      WNVPSV(7)=RVYMIN
      WNVPSV(8)=RVYMAX
      ISVNT=INTR
*
      END
