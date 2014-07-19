*
* $Id: hstelf.F,v 1.1.1.1 1996/01/16 17:07:48 mclareni Exp $
*
* $Log: hstelf.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:48  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.54  by  Rene Brun
*-- Author :
#if !defined(CERNLIB_DOUBLE)
      FUNCTION HSTELF (ITYP,IDEG,X)
#endif
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION FUNCTION HSTELF (ITYP,IDEG,X)
#endif
*.==========>
*.      GIVES THE VALUE COMPUTED AT X OF A STANDARD
*.      ELEMENTARY FUNCTION (POLYNOMIAL) OF TYPE ITYP
*.      AND DEGREE IDEG
*.      ITYP = 0   MONOMIAL
*.             1   TCHEBYSHEV POLYNOMIAL
*.             2   LEGENDRE POLYNOMIAL
*.             3   SHIFTED TCHEBYSHEV POLYNOMIAL
*.             4   LAGUERRE POLYNOMIAL
*.             5   HERMITE POLYNOMIAL
*..=========> ( D.Lienart )
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION P0,P1,P2
#endif
*.___________________________________________
      P0=1
      I1=ITYP+1
      GOTO (10,20,30,40,50,60),I1
*
   10 P1=X
      DO 15 I=2,IDEG
         P2=P1*X
         P1=P2
   15 CONTINUE
      GOTO 70
*
   20 P1=X
      DO 25 I=2,IDEG
         P2=2.*X*P1-P0
         P0=P1
         P1=P2
   25 CONTINUE
      GOTO 70
*
   30 P1=X
      DO 35 I=2,IDEG
         P2=(2.*I-1.)/I*X*P1-(I-1.)/I*P0
         P0=P1
         P1=P2
   35 CONTINUE
      GOTO 70
*
   40 P1=2.*X-1.
      DO 45 I=2,IDEG
         P2=2.*(2.*X-1.)*P1-P0
         P0=P1
         P1=P2
   45 CONTINUE
      GOTO 70
*
   50 P1=1.-X
      DO 55 I=2,IDEG
         P2=(2.*I-1.-X)*P1-(I-1.)**2*P0
         P0=P1
         P1=P2
   55 CONTINUE
      GOTO 70
*
   60 P1=2.*X
      DO 65 I=2,IDEG
         P2=2.*X*P1-2.*(I-1.)*P0
         P0=P1
         P1=P2
   65 CONTINUE
*
   70 HSTELF=P1
      END
