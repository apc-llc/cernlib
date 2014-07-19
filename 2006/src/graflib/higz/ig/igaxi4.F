*
* $Id: igaxi4.F,v 1.1.1.1 1996/02/14 13:10:33 mclareni Exp $
*
* $Log: igaxi4.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:33  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/03 09/12/88  10.33.29  by  O.Couet
*-- Author :
      SUBROUTINE IGAXI4(A1,A2,NAA,BL,BH,NB,BWID)
*.==========>
*.
*.   This routine adjusts the bining of the axis
*.   in order to have integer value for labelling.
*.
*. _Input parameters:
*.
*.  REAL A1,A2 : Old WMIN,WMAX .
*.  REAL BL,BH : New WMIN,WMAX .
*.  INTEGER NAA : Old NDIV (primary divisions)
*.  INTEGER NB : New NDIV .
*.
*..=========>
*
      BWID=ABS(A2-A1)/NAA
      IF(BWID.LE.1.)THEN
         BWID=1.
         BL=INT(A1)
      ELSE
         IWID=INT(BWID/5)+1
         BWID=5.*IWID
         BL=INT(A1/BWID)*BWID
*
*     We determine BL to have one tick mark at 0
*     if there are negative labels.
*
         IF(A1.LT.0.)THEN
            IC=0
   10       IC=IC+1
            RBL=BL/BWID
            IBL=INT(BL/BWID)
            IF(((RBL-IBL).NE.0.).AND.(IC.LE.IWID))THEN
              BL=BL-5.
              GOTO 10
            ENDIF
         ENDIF
      ENDIF
      BH=INT(A2)
*
      NB=0
      XB=BL
   20 XB=XB+BWID
      IF(XB.LE.BH)THEN
         NB=NB+1
         GOTO 20
      ENDIF
*
      BH=XB-BWID
*
   99 RETURN
      END


