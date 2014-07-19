*
* $Id: fpmninit.F,v 1.3 1996/05/31 16:24:36 couet Exp $
*
* $Log: fpmninit.F,v $
* Revision 1.3  1996/05/31 16:24:36  couet
* - Cleaned up useless commented lines.
*
* Revision 1.2  1996/05/31 16:06:56  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:10  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 21/04/95  16.53.12  by  Gregory Kozlovsky
*-- Author :    Gregory Kozlovsky   20/04/95
      SUBROUTINE FPMNINIT
*.===========> Author: G. Kozlovsky, 1994
*.
*. Initialize Minuit
*..==========>
*
#include "hbook/hcunit.inc"
*
      DOUBLE PRECISION ARGLIS(3)
      EXTERNAL HFCNH
      EXTERNAL PAWSIM
*
      IRD=5
      ISAV=7
      CALL MNINIT(IRD,LOUT,ISAV)
      ARGLIS(1)=1.E-7
      NARG=1
      CALL MNEXCM(HFCNH,'SET EPS',ARGLIS,NARG,IERFL,PAWSIM)
*
*     Set error criterion for Chi**2 or Liklihood fits
*
      NARG=1
      ARGLIS(1) = 1.0
      CALL MNEXCM(HFCNH,'SET ERR',ARGLIS,NARG,IERFL,PAWSIM)
*
*        Calculate iofset
*
      IOFSET=0
*
*     Initializations
*
      ARGLIS(1)=-1.
      NARG=1
      CALL MNEXCM(HFCNH,'SET PRINT',ARGLIS,NARG,IERFL,PAWSIM)
      ARGLIS(1)=0.
      NARG=0
      CALL MNEXCM(HFCNH,'SET NOW',ARGLIS,NARG,IERFL,PAWSIM)

*
      END
