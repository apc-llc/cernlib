*
* $Id: hlabeq.F,v 1.1.1.1 1996/01/16 17:07:41 mclareni Exp $
*
* $Log: hlabeq.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:41  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :    P.Aubert   18/11/92
      LOGICAL FUNCTION HLABEQ(IDD,CHOPT)
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C HLABEQ : HBOOK ALPHA LABEL ?
C TRUE IF ALPHA LABEL EXIST
C ELSE FALSE
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
C
      INTEGER KNCHX
      PARAMETER(KNCHX=2)
C
      INTEGER LCHX,NCHX,LCHY,NCHY
      EQUIVALENCE(LCHX ,LHDUM(1))
      EQUIVALENCE(LCHY ,LHDUM(2))
C
      INTEGER IDD,IOPT(2),IOPTX,IOPTY
      CHARACTER*(*) CHOPT
      CHARACTER*8 CHOPTU
C ----------------------------------------------------------------------
      CHOPTU=CHOPT
      CALL CLTOU(CHOPTU)
      IOPTX=INDEX(CHOPTU,'X')
      IOPTY=INDEX(CHOPTU,'Y')
C ----------------------------------------------------------------------
      HLABEQ = .FALSE.
      CALL HFIND(IDD,'HLABEQ')
      IF(LCID.EQ.0)GO TO 999
      CALL HDCOFL
      IF(I123.EQ.0)GO TO 999
C ----------------------------------------------------------------------
      NBLINK = IQ(LCID-2)
      CALL HUOPTC(CHOPT,'XY',IOPT)
      IF(I230.EQ.0)THEN
C     -- 1D
         IF(NBLINK.LT.8)GO TO 999
         IF(IOPT(2).EQ.1)GO TO 999
         LCHX = LQ(LCID-8)
         IF(LCHX.LE.0)GO TO 999
         NCHX = IQ(LCHX+KNCHX)
         IF(NCHX.GE.1) HLABEQ = .TRUE.
      ELSE
C     -- 2D
         IF(NBLINK.LT.9)GO TO 999
C        -- X
         IF(IOPTY.NE.0)THEN
            LCHY = LQ(LCID-9)
            IF(LCHY.LE.0)GO TO 999
            NCHY = IQ(LCHY+KNCHX)
            IF(NCHY.GE.1) HLABEQ = .TRUE.
C        -- Y
         ELSE IF(IOPTX.NE.0)THEN
            LCHX = LQ(LCID-8)
            IF(LCHX.LE.0)GO TO 999
            NCHX = IQ(LCHX+KNCHX)
            IF(NCHX.GE.1) HLABEQ = .TRUE.
         ELSE
            CALL HBUG('FOR A 2-D HISTOGRAM :                           '
     +      //'          CHOPT MUST CONTAIN X or Y', 'HLABEQ',ID)
            RETURN
         ENDIF
      ENDIF
  999 END
