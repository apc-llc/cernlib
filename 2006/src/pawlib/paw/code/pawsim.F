*
* $Id: pawsim.F,v 1.1.1.1 1996/03/01 11:38:44 mclareni Exp $
*
* $Log: pawsim.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:44  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 18/04/95  11.44.46  by  O.Couet
*-- Author :
      FUNCTION PAWSIM(X)
*
*          Evaluates fitting function for combinations
*          of simple expressions
*
#include "paw/pcfitf.inc"
#include "hbook/hcfitd.inc"
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION DD,XX,PFUNC,F(10),DELX,DEV
#endif
#if !defined(CERNLIB_DOUBLE)
      DIMENSION F(10)
#endif
*
      XX=X
      PFUNC=0.
      IPAR=0
      DO 20 I=1,NOPER
         IF(IFTYPE(I).EQ.1)THEN
            DELX = ABS(XX-FITPAD(IPAR+2))
            DEV = ABS(FITPAD(IPAR+3))
            IF (DELX .GT. 13.114870*DEV) THEN
C           IF (DELX .GT. 10.0*DEV) THEN
                DD = -86.0
            ELSEIF (DEV .EQ. 0.0) THEN
                DD = 0.0
            ELSE
C               DD=-0.5*((XX-FITPAD(IPAR+2))/FITPAD(IPAR+3))**2
                DD=-0.5*(DELX/DEV)**2
            ENDIF
            IF (DD .GT. 20.0) THEN
                PRINT 100,DD
            ENDIF
100             FORMAT('PAWSIM DD',F10.5)
            F(I)=FITPAD(IPAR+1)*EXP(DD)
            IPAR=IPAR+3
         ELSEIF(IFTYPE(I).EQ.2)THEN
            DD=FITPAD(IPAR+1)+XX*FITPAD(IPAR+2)
            IF (DD.LT.-86.) DD=-86.
            IF (DD.GT.86.) DD=86.
            F(I)=EXP(DD)
            IPAR=IPAR+2
         ELSE
            F(I)=FITPAD(IPAR+1)
            DO 10 J=2,NPF(I)
               F(I)=F(I)+FITPAD(IPAR+J)*XX**(J-1)
  10        CONTINUE
            IPAR=IPAR+NPF(I)
         ENDIF
  20  CONTINUE
*
      DO 30 I=2,NOPER
         IF(LOPER(I).EQ.2)THEN
            F(I)=F(I)*F(I-1)
            F(I-1)=0.
         ENDIF
  30  CONTINUE
      DO 40 I=1,NOPER
         PFUNC=PFUNC+F(I)
  40  CONTINUE
      FITFUN=PFUNC
      PAWSIM=PFUNC
      END
