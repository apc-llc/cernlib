*
* $Id: dzdifa.F,v 1.1.1.1 1996/03/04 16:13:14 mclareni Exp $
*
* $Log: dzdifa.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:14  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDIFA(X0, X0D, Y0, Y0D, IFAIS, ICOL)
      REAL X0, X0D, Y0, Y0D, XPOL(5),YPOL(5)
      XPOL(1)=X0
      XPOL(2)=X0D
      XPOL(3)=X0D
      XPOL(4)=X0
      XPOL(5)=X0
      XPOL(1)=X0
      YPOL(1)=Y0
      YPOL(2)=Y0
      YPOL(3)=Y0D
      YPOL(4)=Y0D
      YPOL(5)=Y0
      CALL ISFAIS(IFAIS)
      IF(ICOL.GE.0)CALL ISFACI(ICOL)
      CALL IFA(5,XPOL,YPOL)
      CALL ISPLCI(1)
      CALL IPL(5,XPOL,YPOL)
      END
************************************************************************
