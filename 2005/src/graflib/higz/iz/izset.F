*
* $Id: izset.F,v 1.1.1.1 1996/02/14 13:11:14 mclareni Exp $
*
* $Log: izset.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:14  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/06 10/03/89  08.46.53  by  O.Couet
* Test if ISVNT=0
*-- Author :
      SUBROUTINE IZSET
*.===========>
*.
*. This restores the attributes environment.
*.
*..==========> (O.Couet)
#include "higz/hiatnb.inc"
#include "higz/hisave.inc"
#include "higz/hiatnm.inc"
*.______________________________________
*
      DO 10 I=1,NBIATT
         CALL IGSET(CHIATT(I),FLOAT(ISAVE(I)))
 10   CONTINUE
      DO 20 I=1,NBRATT
         CALL IGSET(CHRATT(I),RSAVE(I))
 20   CONTINUE
      IF(ISVNT.NE.0)THEN
         CALL ISWN(ISVNT,WNVPSV(1),WNVPSV(2),WNVPSV(3),WNVPSV(4))
         CALL ISVP(ISVNT,WNVPSV(5),WNVPSV(6),WNVPSV(7),WNVPSV(8))
      ENDIF
      CALL ISELNT(ISVNT)
*
      END
