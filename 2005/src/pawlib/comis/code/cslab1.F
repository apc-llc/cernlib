*
* $Id: cslab1.F,v 1.1.1.1 1996/02/26 17:16:29 mclareni Exp $
*
* $Log: cslab1.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:29  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/07 20/08/93  11.49.29  by  Vladimir Berezhnoi
*-- Author :
      SUBROUTINE CSLAB1(IPCE)
      INTEGER CSLTLL,CSITLL
#include "comis/cspar.inc"
#include "comis/comis.inc"
#include "comis/cstab.inc"
 999  I=CSLTLL(IPVS)
      IF(I.EQ.0)THEN
        LABST=0
        LABADR =IPCE
        KOD(IPCE)=0
        IW=CSITLL(IPVS)
      ELSE
        KOD(IPCE)=LABADR
        IF(LABST.EQ.0)THEN
             LABADR=IPCE
          CALL CSRTLL(I)
        ENDIF
      ENDIF
      IPCE=IPCE+1
      END
