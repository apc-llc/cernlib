*
* $Id: hlabnb.F,v 1.1.1.1 1996/01/16 17:07:41 mclareni Exp $
*
* $Log: hlabnb.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:41  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/09 05/02/93  16.35.43  by  Rene Brun
*-- Author :    P.Aubert   20/11/92
      INTEGER FUNCTION HLABNB(ID1,CHOPT)
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C HLABNB : HBOOK LABEL NUMBER
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CHOPT = 'X','Y' or 'Z'
C 'X' is the default
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C PARAMETER
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INTEGER KNCHX
      PARAMETER(KNCHX=2)
      INTEGER LOCATI
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      LOGICAL HLABEQ
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      CHARACTER*(*) CHOPT
      INTEGER NBLABEL,IOPT(2)
C ----------------------------------------------------------------------
C Init value
C ----------------------------------------------------------------------
      IF(ID1.NE.IDLAST)THEN
         ID     = ID1
         IDPOS  = LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
         IDLAST = ID1
         LCID   = LQ(LTAB-IDPOS)
         LCONT  = LQ(LCID-1)
         CALL HDCOFL
      ENDIF
C ----------------------------------------------------------------------
C -- options ???
C ----------------------------------------------------------------------
      CALL UOPTC(CHOPT,'XY',IOPT)
C ----------------------------------------------------------------------
C -- id with label ?
C ----------------------------------------------------------------------
      HLABNB=0
      IF(.NOT.HLABEQ(ID,CHOPT))THEN
         CALL HBUG('No labels on this histogram','HLABNB',ID)
         RETURN
      ENDIF
C ----------------------------------------------------------------------
C NB OF LABEL
C ----------------------------------------------------------------------
      NBLABEL = IQ(LCID-2)
      IF(I1.NE.0)THEN
C     -- 1D
         IF(IOPT(2).EQ.1)THEN
            CALL HBUG('No labels on Y axis','HLABNB',ID)
            RETURN
         ELSE
            HLABNB = IQ(LQ(LCID-8)+KNCHX)
         ENDIF
      ELSE IF(I230.NE.0)THEN
C     -- 2D
         IF(IOPT(2).EQ.1)THEN
C           -- Y
            HLABNB = IQ(LQ(LCID-9)+KNCHX)
         ELSE
C           -- X
            HLABNB = IQ(LQ(LCID-8)+KNCHX)
         ENDIF
      ELSE
         CALL HBUG('Bad number of links','HLABNB',ID)
      ENDIF
C ----------------------------------------------------------------------
      RETURN
      END
