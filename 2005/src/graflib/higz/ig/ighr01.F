*
* $Id: ighr01.F,v 1.1.1.1 1996/02/14 13:10:35 mclareni Exp $
*
* $Log: ighr01.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:35  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.12/07 22/03/91  09.15.59  by  O.Couet
*-- Author :    O.Couet   08/06/90
      REAL FUNCTION IGHR01(RN1,RN2,HUEI)
      HUE=HUEI
      IF(HUE.GT.360.)HUE=HUE-360.
      IF(HUE.LT.0.)HUE=HUE+360.
      IF(HUE.LT.60.)THEN
         IGHR01=RN1+(RN2-RN1)*HUE/60.
      ELSEIF(HUE.LT.180.)THEN
         IGHR01=RN2
      ELSEIF(HUE.LT.240.)THEN
         IGHR01=RN1+(RN2-RN1)*(240.-HUE)/60.
      ELSE
         IGHR01=RN1
      ENDIF
      END

