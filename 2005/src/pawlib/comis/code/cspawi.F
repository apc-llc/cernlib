*
* $Id: cspawi.F,v 1.1.1.1 1996/02/26 17:16:22 mclareni Exp $
*
* $Log: cspawi.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:22  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_PAW)
*CMZ :          26/10/95  16.21.47  by  Julian Bunn
*-- Author : V.Berezhnoi
      SUBROUTINE CSPAWI
***--------------------------------------------
*       paw-vectors parsing init
***--------------------------------------------
      EXTERNAL CSMKVD
#include "comis/cspar.inc"
#include "comis/cspnts.inc"
#include "comis/cskucs.inc"
#include "comis/cstab.inc"
      INTEGER CSLTGB,CSITGB
      COMMON/PAWC/NWPAW,IXPAWC,IHDIV,IXHIGZ,IXKU,FENC(5),LMAIN,HCV(9989)
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
****      JKUVBS=LOCF(IQ(1))-1
      JKUVBS=LOCF(IQ(1))+KUVOFS-1
      KUVCUR=0
      CALL CCOPYS(MJSCHA('CSPAWC  '),JID,8)
      NCIDEN=6
      NWIDEN=2
      I=CSLTGB(IPVS)
      IADGB=JKUVBS
      ICGB=1
      LENGB=8000
      I=CSITGB(IPVS)
      CALL MZLINK(IXPAWC,'/CSKULK/',KUVLNK(1),KUVLNK(1),KUVLNK(KUVTOT))
      CALL KUCOMV(CSMKVD)
      call cscom('CSKULK#',jkuvbs,i,i,i,i,i,i,i,i,i)
      END
#endif
