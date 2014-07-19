*
* $Id: igconv.F,v 1.1.1.1 1996/02/14 13:10:34 mclareni Exp $
*
* $Log: igconv.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:34  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.10/00 24/04/90  12.07.19  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGCONV(N,X,Y)
*.===========>
*.
*. Convert X and Y coordinates from X1 to X2 2D repere
*.
*..==========> (O.Couet)
#include "higz/hiconv.inc"
      DIMENSION X(*),Y(*)
*.______________________________________
*
      DO 10 I=1,N
         X(I)=((X(I)-X1MIN)*(X2MAX-X2MIN))/(X1MAX-X1MIN)+X2MIN
         Y(I)=((Y(I)-Y1MIN)*(Y2MAX-Y2MIN))/(Y1MAX-Y1MIN)+Y2MIN
  10  CONTINUE
      END
