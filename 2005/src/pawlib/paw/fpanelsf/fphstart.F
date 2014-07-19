*
* $Id: fphstart.F,v 1.3 1996/05/31 16:24:35 couet Exp $
*
* $Log: fphstart.F,v $
* Revision 1.3  1996/05/31 16:24:35  couet
* - Cleaned up useless commented lines.
*
* Revision 1.2  1996/05/31 16:06:55  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:10  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 09/04/95  23.20.13  by  Gregory Kozlovsky
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPHSTART(FCN,UFCN)
      EXTERNAL FCN,UFCN
*.===========> Author: G. Kozlovsky, 1994
*.
*. Start Histo Fit Panel invoked from histo/fit command
*..==========>
*
#include "hbook/hcfits.inc"
#include "paw/pawcom.inc"
#include "paw/pcchar.inc"
#include "paw/pcrang.inc"
#include "paw/pcbuff.inc"
*
      COMMON/PAWPAR/PAR(200)
*
      DIMENSION VALP(50),SIGPAR(50)
      DIMENSION COV(1000),PMIN(50),PMAX(50),STEP(50)
      EQUIVALENCE (PAWBUF(1),VALP(1)),(PAWBUF(101),SIGPAR(1))
      EQUIVALENCE (PAWBUF(501),COV(1)),(PAWBUF(1501),PMIN(1))
      EQUIVALENCE (PAWBUF(1601),PMAX(1)),(PAWBUF(1701),STEP(1))
      EXTERNAL PAWFUN,PAWFUY,PAWSIM
*
      PARAMETER (MAX_PARNUMBER=35)
      INTEGER NP
      DOUBLE PRECISION FMIN, FEDM, ERRDEF
      DOUBLE PRECISION DPAR(MAX_PARNUMBER),DPMIN(MAX_PARNUMBER)
      DOUBLE PRECISION DPMAX(MAX_PARNUMBER),DSTEP(MAX_PARNUMBER)
      DOUBLE PRECISION DEPARAB(MAX_PARNUMBER)
      CHARACTER*12 PARNAME(MAX_PARNUMBER)
C.
C.    ------------------------------------------------------------------
C.

      CALL MNSTAT(FMIN, FEDM, ERRDEF, NPARI, NP, ISTAT)
      DO 1 I=1,NP
          CALL MNPOUT(I,PARNAME(I),
     2        DPAR(I),DSTEP(I),DPMIN(I),DPMAX(I),IVARBL)
          PAR(I) = DPAR(I)
          PMIN(I) = DPMIN(I)
          PMAX(I) = DPMAX(I)
          STEP(I) = DSTEP(I)
1     CONTINUE

      CALL FPHCMNDPANEL(ID, CHFILE,
     1    FCN, UFCN, NP,
     2    DPAR, DPMIN, DPMAX, DSTEP, DEPARAB)

      END
