*
* $Id: hlpos.F,v 1.1.1.1 1996/01/16 17:07:42 mclareni Exp $
*
* $Log: hlpos.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:42  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/08 03/02/93  13.49.23  by  Pierre Aubert
*-- Author :    P.Aubert   19/11/92
      SUBROUTINE HLPOS(IDD,CHX,IPOS,CHOPT)
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C HLPOS : HBOOK LABEL POSITION
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C IN
C --
C
C IDD   : IDentifier
C CHX   : Character  describe a LABEL on an  axis discribe by CHOPT
C CHOPT : 'X' or default 'Y','Z'
C
C OUT
C ---
C
C IPOS  : If Found -> return IPOS else retunr -1
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C PARAMETER
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INTEGER KLGRCX,KBLAB,KNCHX
      PARAMETER(KLGRCX=16,KBLAB=7,KNCHX=2)
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C COMMON
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      LOGICAL HLABEQ
      INTEGER LCHX,NCHX,LCHY
      EQUIVALENCE(LCHX ,LHDUM(1))
      EQUIVALENCE(LCHY ,LHDUM(2))
      INTEGER LOCATI,HLCCMP
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C VARIABLE
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INTEGER IDD
      CHARACTER*(*) CHX,CHOPT
      INTEGER IPOS
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C LOCAL VARIABLE
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INTEGER IOPT(3),BITPOS,NCCHX,LEFT,RIGHT,MEDIAN
      CHARACTER AXE
      CHARACTER*16 CHANNELX
      LOGICAL LINEAR,EXIST
C ----------------------------------------------------------------------
C Init value
C ----------------------------------------------------------------------
      IF(IDD.NE.IDLAST)THEN
         ID     = IDD
         IDPOS  = LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
         IDLAST = ID
         LCID   = LQ(LTAB-IDPOS)
         LCONT  = LQ(LCID-1)
      ENDIF
C ----------------------------------------------------------------------
C -- options ???
C ----------------------------------------------------------------------
      CALL UOPTC(CHOPT,'XYZ',IOPT)
      IF(IOPT(3).EQ.1)THEN
         AXE    = 'Z'
         BITPOS = 3
         CALL HBUG('OPTION Z NOT IMPLEMENTED','HLPOS',ID)
         RETURN
      ELSE IF(IOPT(2).EQ.1)THEN
         AXE    = 'Y'
         BITPOS = 2
         LCHX   = LQ(LCID-9)
         NCHX   = IQ(LCHY+KNCHX)
      ELSE
         AXE    = 'X'
         BITPOS = 1
         LCHX   = LQ(LCID-8)
         NCHX   = IQ(LCHX+KNCHX)
      ENDIF
C ----------------------------------------------------------------------
C DOES LABEL EXIST ?
C ----------------------------------------------------------------------
      IF(.NOT.HLABEQ(ID,AXE))THEN
         CALL HBUG('LABEL does not exist','HLPOS',ID)
         RETURN
      ENDIF
C ----------------------------------------------------------------------
C CHANNELX
C ----------------------------------------------------------------------
      CHANNELX(1:KLGRCX) = '                '
      NCCHX = LENOCC(CHX)
      IF(NCCHX.GT.KLGRCX) THEN
         NCCHX=KLGRCX
      ENDIF
      CHANNELX(1:NCCHX) = CHX(1:NCCHX)
C ----------------------------------------------------------------------
C FIND CHANEL
C ----------------------------------------------------------------------
      EXIST = .FALSE.
      LINEAR = .FALSE.
C -- linear or binary search
      IF(JBIT(IQ(LCHX),BITPOS).EQ.0)THEN
         LINEAR = .TRUE.
      ENDIF
C ----------------------------------------------------------------------
C linear search
C ----------------------------------------------------------------------
      IF(LINEAR)THEN
         IF(NCHX.GE.1)THEN
            IPOS=0
 10         IPOS=IPOS+1
            IF(HLCCMP(IQ(LCHX+KBLAB+(IPOS-1)*4),
     $           CHANNELX,KLGRCX).EQ.0)THEN
               EXIST = .TRUE.
            ENDIF
            IF((IPOS.LT.NCHX).AND.(.NOT.EXIST)) GO TO 10
         ENDIF
      ELSE
C ----------------------------------------------------------------------
C binary search
C ----------------------------------------------------------------------
         LEFT  = 1
         RIGHT = NCHX
C     -- while
 20      IF((LEFT.LE.RIGHT).AND.(.NOT.EXIST))THEN
            MEDIAN = ( LEFT + RIGHT ) /2
            CMP = HLCCMP(IQ(LCHX+KBLAB+(MEDIAN-1)*4),
     $           CHANNELX,KLGRCX)
            IF(CMP.LT.0)THEN
               LEFT = MEDIAN+1
            ELSE IF(CMP.EQ.0)THEN
               EXIST = .TRUE.
               IPOS = MEDIAN
            ELSE
               RIGHT = MEDIAN-1
            ENDIF
            GO TO 20
         ENDIF
      ENDIF
C ----------------------------------------------------------------------
      IF(.NOT.EXIST)THEN
         IPOS = -1
      ENDIF
C ----------------------------------------------------------------------
      END
 
 
