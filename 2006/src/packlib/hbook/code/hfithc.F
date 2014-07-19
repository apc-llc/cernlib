*
* $Id: hfithc.F,v 1.1.1.1 1996/01/16 17:07:37 mclareni Exp $
*
* $Log: hfithc.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:37  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/05 16/06/94  15.43.29  by  Rene Brun
*-- Author :
      FUNCTION HFITHC(XX)
*.==========>
*          Evaluates fitting function for combinations
*          of simple expressions
*..=========> ( R.Brun)
#include "hbook/hcfitf.inc"
#include "hbook/hcfitd.inc"
#if !defined(CERNLIB_DOUBLE)
      DIMENSION F(10)
#endif
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION F(10)
      DOUBLE PRECISION HF,X
#endif
*.___________________________________________
      X=XX
      IPAR=0
      DO 20 I=1,NOPER
         IF(IFTYPE(I).EQ.1)THEN
            IF(FITPAD(IPAR+3).EQ.0.0)FITPAD(IPAR+3)=1.
            F(I)=FITPAD(IPAR+1)*EXP(-0.5*((X-FITPAD(IPAR+2))
     +           /FITPAD(IPAR+3))**2)
            IPAR=IPAR+3
         ELSEIF(IFTYPE(I).EQ.2)THEN
            F(I)=EXP(FITPAD(IPAR+1)+X*FITPAD(IPAR+2))
            IPAR=IPAR+2
         ELSE
            F(I)=FITPAD(IPAR+1)
            DO 10 J=2,NPF(I)
               F(I)=F(I)+FITPAD(IPAR+J)*X**(J-1)
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
      HF=0.
      DO 40 I=1,NOPER
         HF=HF+F(I)
  40  CONTINUE
      FITFUN=HF
      HFITHC=HF
      END
 
