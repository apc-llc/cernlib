*
* $Id: hwuci2.F,v 1.1.1.1 1996/03/08 17:02:17 mclareni Exp $
*
* $Log: hwuci2.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:17  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWUCI2.
*CMZ :-        -23/08/94  13.22.29  by  Mike Seymour
*-- Author :    Ulrich Baur & Nigel Glover, adapted by Ian Knowles
C------------------------------------------------------------------------
      FUNCTION HWUCI2(A,B,Y0)
C     Integral  LOG(A-EPSI-BY(1-Y))/(Y-Y0)
C------------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION A,B,Y0,ZERO,ONE,FOUR,HALF
#if !defined(CERNLIB_IBM)
      DOUBLE COMPLEX HWUCI2,HWULI2,EPSI,Y1,Y2,Z1,Z2,Z3,Z4
#endif
#if defined(CERNLIB_IBM)
      COMPLEX*16 HWUCI2,HWULI2,EPSI,Y1,Y2,Z1,Z2,Z3,Z4
#endif
      PARAMETER (ZERO=0.D0, ONE =1.D0, FOUR= 4.D0, HALF=0.5D0)
      EXTERNAL HWULI2
      COMMON/SMALL/EPSI
      IF(B.EQ.ZERO)THEN
         HWUCI2=CMPLX(ZERO,ZERO)
      ELSE
         Y1=HALF*(ONE+SQRT(ONE-FOUR*(A+EPSI)/B))
         Y2=ONE-Y1
         Z1=Y0/(Y0-Y1)
         Z2=(Y0-ONE)/(Y0-Y1)
         Z3=Y0/(Y0-Y2)
         Z4=(Y0-ONE)/(Y0-Y2)
         HWUCI2=HWULI2(Z1)-HWULI2(Z2)+HWULI2(Z3)-HWULI2(Z4)
      ENDIF
      RETURN
      END
