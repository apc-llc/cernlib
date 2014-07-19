*
* $Id: hplpro.F,v 1.1.1.1 1996/01/19 10:50:11 mclareni Exp $
*
* $Log: hplpro.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:11  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.04/12 16/01/89  12.08.22  by  O.Couet
*-- Author :
      SUBROUTINE HPLPRO(ID,CHXTIT,CHYTIT)
*.==========>
*.       Plots scatter plots with associated X and Y projection
*.       on a 2*2 grid
*..=========>
#include "hplot/hpl1.inc"
      CHARACTER*(*) CHXTIT,CHYTIT
      LOGICAL LOLD
*._____________________________
*
      NARG=3
      CALL NOARG(NARG)
      LOLD=LOUTIT
*----
*     Plot Y projection in top LH corner,
*     rotated and reversed
*----
      CALL HPLZON(2,2,1,' ')
      IF(NARG.GE.3)THEN
         IF(CHYTIT.NE.' ')THEN
            LOUTIT=.TRUE.
            CALL HPLOT(ID,' ','PROY',0)
            CALL HPLTIT(CHYTIT)
            LOUTIT=LOLD
            GOTO 10
         ENDIF
      ENDIF
      CALL HPLOT(ID,' ','PROY',0)
*----
*     Plot X projection in bottom RH corner
*     reset rotation and reverse options
*----
   10 CALL HPLZON(2,2,4,'SAME')
      IF(NARG.NE.1)THEN
         IF(CHXTIT.NE.' ')THEN
            LOUTIT=.TRUE.
            CALL HPLOT(ID,' ','PROX',0)
            CALL HPLTIT(CHXTIT)
            LOUTIT=LOLD
            GOTO 20
         ENDIF
      ENDIF
      CALL HPLOT(ID,' ','PROX',0)
*----
*     Plot scatter plot in top right hand corner
*----
   20 CALL HPLZON(2,2,2,'S')
      CALL HPLOT(ID,' ','HIST',0)
*----
*     Reset window option
*----
      CALL HPLZON(1,1,1,' ')
*
      END

