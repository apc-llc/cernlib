*
* $Id: ifpl.F,v 1.2 1996/03/07 17:38:14 couet Exp $
*
* $Log: ifpl.F,v $
* Revision 1.2  1996/03/07 17:38:14  couet
* IFALCO selection was wrong
*
* Revision 1.1.1.1  1996/02/14 13:10:31  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/09 17/12/91  13.44.59  by  O.Couet
*-- Author :    O.Couet   11/12/91
      SUBROUTINE IFPL(N,X,Y)
#include "higz/hiatt.inc"
      DIMENSION X(*),Y(*)
      XFALCO=(RVXMIN+XRATIO*(X(1)-RWXMIN))
      YFALCO=(RVYMIN+YRATIO*(Y(1)-RWYMIN))
      CALL IFMOVE(XFALCO,YFALCO)
      DO 10 I=2,N
         XFALCO=(RVXMIN+XRATIO*(X(I)-RWXMIN))
         YFALCO=(RVYMIN+YRATIO*(Y(I)-RWYMIN))
         CALL IFDRAW(XFALCO,YFALCO)
   10 CONTINUE
      CALL IFPUT(0)
      END
