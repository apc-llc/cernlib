*
* $Id: hlgnxt.F,v 1.1.1.1 1996/01/16 17:07:42 mclareni Exp $
*
* $Log: hlgnxt.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:42  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/03 13/01/93  21.07.52  by  Rene Brun
*-- Author :    P.Aubert   18/11/92
      SUBROUTINE HLGNXT(IDD,IPOS,CHLAB,CHOPT)
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C HLGNXT : HBOOK GET NEXT LABEL
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C IDD
C IPOS
C CHLAB
C CHOPT 'X','Y' OR 'Z' 'X' BY DEFAULT
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C PARAMETER
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INTEGER KLGRCX,KBLAB,KNCHX
      PARAMETER(KLGRCX=16,KBLAB=7,KNCHX=2)
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INTEGER IDD,IPOS
      CHARACTER*(*) CHLAB,CHOPT
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INTEGER IOPT(3),IDPOS
      CHARACTER*16 CHTEMP
C ----------------------------------------------------------------------
C Init value
C ----------------------------------------------------------------------
      IF(IDD.NE.IDLAST)THEN
         ID     = IDD
         IDPOS  = LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
         IF(IDPOS.LE.0)RETURN
         IDLAST = ID
         LCID   = LQ(LTAB-IDPOS)
         CALL HDCOFL
      ENDIF
C     -- options ???
      CALL UOPTC(CHOPT,'XYZ',IOPT)
C     -- DIM ?
      IF(I1.NE.0)THEN
C     -- 1D
         LCH = LQ(LCID-8)
      ELSE
C     -- 2D
C        -- AXE Y
         IF(IOPT(2).EQ.1)THEN
            LCH = LQ(LCID-9)
C        -- AXE X or default
         ELSE
            LCH = LQ(LCID-8)
         ENDIF
      ENDIF
C     -- test
      IF((IPOS.LT.0).OR.(IPOS.GT.IQ(LCH+KNCHX)))THEN
         CALL HBUG('IPOS out of range','HLGNXT',ID)
      ENDIF
      CALL UHTOC(IQ(LCH+KBLAB+4*(IPOS-1)),4,CHTEMP,KLGRCX)
C
      LGR = MIN(LEN(CHLAB),KLGRCX)
      CHLAB(1:LGR) = CHTEMP(1:LGR)
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      END
