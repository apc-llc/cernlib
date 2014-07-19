*
* $Id: csslab.F,v 1.1.1.1 1996/02/26 17:16:29 mclareni Exp $
*
* $Log: csslab.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:29  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/07 20/08/93  12.44.01  by  Vladimir Berezhnoi
*-- Author :
      SUBROUTINE CSSLAB(IPCE,IPCB)
      INTEGER CSLTLL,CSITLL
#include "comis/cspar.inc"
#include "comis/comis.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
      COMMON /CSGSCM/IGSST,JGSST,NGSST,NGSPAR,JGSSB,GSNLAB
  998 I=CSLTLL(IPVS)
      IF(I.EQ.0)THEN
        LABLL=LAB
        LABST=1
        LABADR=IPCE-IPCB
        I=CSITLL(IPVS)
      ELSE IF(LABST.EQ.1)THEN
        GO TO 705
*       ----------
      ELSE
        IPCL=LABADR
        L=IPCE-IPCB
 1007   IF(IPCL.EQ.0) GO TO 1008
        K=KOD(IPCL)
        KOD(IPCL)=L
        IPCL=K
        GO TO 1007
 1008   LABST=1
        LABADR=L
        CALL CSRTLL(I)
      ENDIF
      RETURN
  705 IGSST=-5
      END
