*
* $Id: hpoly.F,v 1.1.1.1 1996/01/16 17:08:12 mclareni Exp $
*
* $Log: hpoly.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:12  mclareni
* First import
*
*
#include "sys/CERNLIB_machine.h"
#include "_hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.52  by  Rene Brun
*-- Author :
      FUNCTION HPOLY (ARGUM)
*.==========>
*.           CALCULATES VALUE OF THE POLYNOMIAL
*.           AND ITS DERIVATIVES
*..=========> ( D.Lienart )
#include "hcfit1.inc"
#include "hcfit2.inc"
#include "hcfit3.inc"
#include "hcbook.inc"
*.___________________________________________
      EPS=0.01
*
      IF(LINEAR.EQ.2)GO TO 20
*
      Q(IDERIV)=1.
      HPOLY =Q(IPARAM)
      IF(NPOLYN.EQ.0)RETURN
      DO 10 I=1,NPOLYN
         Q(IDERIV+I)=Q(IDERIV+I-1)*ARGUM
         HPOLY =HPOLY +Q(IPARAM+I)*Q(IDERIV+I)
  10  CONTINUE
      RETURN
*
*             CHEBYSCHEV EXPANSION
*
  20  Q(IDERIV)=1.
      HPOLY=Q(IPARAM)*0.5
      IF(NPOLYN.EQ.0) GO TO 40
      Q(IDERIV+1)=ARGUM
      HPOLY=HPOLY+Q(IPARAM+1)*Q(IDERIV+1)
      IF(NPOLYN.EQ.1)GO TO 40
*
      DO 30 I=2,NPOLYN
         Q(IDERIV+I)=Q(IDERIV+I-1)*2.*ARGUM-Q(IDERIV+I-2)
         HPOLY=HPOLY+Q(IPARAM+I)*Q(IDERIV+I)
  30  CONTINUE
  40  CONTINUE
      Q(IDERIV)=0.5
      END
