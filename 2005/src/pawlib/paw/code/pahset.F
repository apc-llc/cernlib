*
* $Id: pahset.F,v 1.1.1.1 1996/03/01 11:38:40 mclareni Exp $
*
* $Log: pahset.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:40  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  1.05/02 03/01/89  10.12.16  by  Rene Brun
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAHSET
*
*           /HISTOGRAM/SET
*           To set HBOOK options
*
#include "hbook/hcbook.inc"
#include "paw/pawcom.inc"
#include "paw/pcpatl.inc"
      CHARACTER*8 CHOPT
*
      CALL KUPATL(CHPATL,NPAR)
*
      IF(CHPATL.EQ.'MAXIMUM')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(ID.NE.0.AND.LCID.LE.0)GO TO 99
         CALL KUGETR(VMAX)
         CALL HMAXIM(ID,VMAX)
         CALL HSETCD
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'MINIMUM')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(ID.NE.0.AND.LCID.LE.0)GO TO 99
         CALL KUGETR(VMIN)
         CALL HMINIM(ID,VMIN)
         CALL HSETCD
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'NORMALIZE_FACTOR')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(ID.NE.0.AND.LCID.LE.0)GO TO 99
         CALL KUGETR(VNORM)
         CALL HNORMA(ID,VNORM)
         CALL HSETCD
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'SCALE_FACTOR_2D')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(ID.NE.0.AND.LCID.LE.0)GO TO 99
         CALL KUGETR(VSCALE)
         CALL HSCALE(ID,VSCALE)
         CALL HSETCD
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'IDOPT')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(ID.NE.0.AND.LCID.LE.0)GO TO 99
         CALL KUGETC(CHOPT,NCH)
         CALL HIDOPT(ID,CHOPT)
         CALL HSETCD
         GO TO 99
      ENDIF
*
  99  END
