*
* $Id: hgetfl.F,v 1.1.1.1 1996/01/16 17:07:38 mclareni Exp $
*
* $Log: hgetfl.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:38  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 05/03/89  09.22.01  by  Rene Brun
*-- Author :
      SUBROUTINE HGETFL(ICONTR)
*.==========>
*.           SETS  FLAGS  LINEAR,IFLSF,IFLBUF,IDER,IWEIGH,ITFUM,ISUPIM
*.                 INTO THE /HCFIT2/
*..=========> ( I.Ivanchenko )
#include "hbook/hcfit2.inc"
      COMMON/HCGARB/GA(16),JCONTR
      DIMENSION IFLARR(7)
      EQUIVALENCE (IFLARR(1),LINEAR)
      SAVE NFL
      DATA NFL/7/
*.___________________________________________
      LINEAR=0
      IFLSF=0
      IFLBUF=0
      IDER  =0
      ITFUM =-1
      IWEIGH=0
      ISUPIM=2
*
      JCONTR=ICONTR
      DO 10 I=1,NFL
         IFLARR(I)=JCONTR/10**(NFL-I)
         JCONTR=JCONTR-IFLARR(I)*10**(NFL-I)
  10  CONTINUE
      IF(IFLFUN.EQ.2)IFLSF=0
      ITFUM=ITFUM-1
      IF(IFLSF.EQ.0)GO TO 99
      IFLBUF=0
      IDIMPN=IFLSF+2
*
  99  RETURN
      END
