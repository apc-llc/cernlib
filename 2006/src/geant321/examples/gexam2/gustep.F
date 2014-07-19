*
* $Id: gustep.F,v 1.2 1996/02/01 14:54:51 ravndal Exp $
*
* $Log: gustep.F,v $
* Revision 1.2  1996/02/01 14:54:51  ravndal
* Clean up the repository
*
* Revision 1.1.1.1  1995/10/24 10:22:11  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE GUSTEP
*
************************************************************************
*                                                                      *
*             User routine called at the end of each tracking step     *
*             MEC   is the mechanism origin of the step                *
*             INWVOL is different from 0 when the track has reached    *
*                    a volume boundary                                 *
*             ISTOP is different from 0 if the track has stopped       *
*                                                                      *
************************************************************************
*
#include "geant321/gctmed.inc"
#include "geant321/gckine.inc"
#include "geant321/gcking.inc"
#include "geant321/gcflag.inc"
#include "geant321/gctrak.inc"
#include "geant321/gcvolu.inc"
#include "cdhscom.inc"
*
*     -----------------------------------------------------------------
*
*             Accumulate energy deposited in cscintillator
*
      IF(NUMED.EQ.2)THEN
         IF(DESTEP.GT.0.)THEN
            INDEX  = (NUMBER(2) - 1)*4 + (NUMBER(3)-1)/5+1
            IPL=(VECT(2)+188.)/47. + 1
            ESINT(IPL,INDEX) = ESINT(IPL,INDEX) + DESTEP
         ENDIF
      ENDIF
*
*             Something generated ?
*
      IF(NGKINE.GT.0) THEN
        DO 5 I=1,NGKINE
          ITYPA  = GKIN(5,I)
          IF(ITYPA.NE.4) CALL GSKING(I)
   5    CONTINUE
      ENDIF
*
*             Debug/plot event
*
      IF(IDEBUG.NE.0) THEN
        IF((ISWIT(2).EQ.1).OR.(ISWIT(3).EQ.1)) CALL GSXYZ
        IF (ISWIT(2).EQ.2) CALL GPCXYZ
#if defined(CERNLIB_HIGZ)
        IF (ISWIT(2).GT.2)THEN
           IF(ISWIT(2).EQ.3) CALL GDCXYZ
           IF(ISWIT(2).EQ.4.AND.CHARGE.NE.0.) CALL GDCXYZ
           IF(ISWIT(2).EQ.5.AND.
     +        (IPART.EQ.2.OR.IPART.EQ.3)) CALL GDCXYZ
        ENDIF
#endif
      ENDIF
      END
