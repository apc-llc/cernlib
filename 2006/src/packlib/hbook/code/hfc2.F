*
* $Id: hfc2.F,v 1.1.1.1 1996/01/16 17:07:36 mclareni Exp $
*
* $Log: hfc2.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:36  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :    P.Aubert   18/11/92
      SUBROUTINE HFC2(ID1,IPOS,CHX,JPOS,CHY,W,CHOPT)
C **********************************************************************
C HFC2
C **********************************************************************
C ID1   : Histogram identifier
C CHX   : Channel X
C CHY   : Channel Y
C W     : Weight
C CHOPT : Options 'N' Normal filling
C                 'S' or default automatically Sort
C                 'U' UNDEFLOW IF 'U' IS SET AND THE CHANNEL DOESN T
C                     EXIST THEN THE UNDERFLOW CHANNEL IS INCREMENTED
C **********************************************************************
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcprin.inc"
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C PARAMETER
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INTEGER KNCHX
      PARAMETER(KNCHX =2)
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C COMMON
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INTEGER LCHX,NCHX,LCHY,NCHY
      EQUIVALENCE(LCHX ,LHDUM(1))
      EQUIVALENCE(LCHY ,LHDUM(2))
      INTEGER LOCATI
C ----------------------------------------------------------------------
      LOGICAL HLABEQ
C ----------------------------------------------------------------------
      INTEGER ID1,IPOS,JPOS,IIPOS,JJPOS
      CHARACTER*(*) CHX,CHY
      REAL W
      CHARACTER*(*) CHOPT
C ----------------------------------------------------------------------
C Init value
C ----------------------------------------------------------------------
      IF(ID1.NE.IDLAST)THEN
         ID = ID1
         IDPOS = LOCATI(IQ(LTAB + 1),IQ(LCDIR + KNRH),ID)
         IF(IDPOS.LE.0)RETURN
         IDLAST = ID1
         LCID = LQ(LTAB - IDPOS)
         LCONT = LQ(LCID - 1)
      ENDIF
C ----------------------------------------------------------------------
C IF THE LABEL DON''T EXIST
C ----------------------------------------------------------------------
      IF(.NOT.HLABEQ(ID,'X'))THEN
         IF(.NOT.HLABEQ(ID,'Y'))THEN
            CALL HBUG('No labels : CALL HLABEL first','HFC2',ID)
            RETURN
         ENDIF
      ENDIF
C ----------------------------------------------------------------------
C FIND IPOS AND JPOS
C ----------------------------------------------------------------------
      IIPOS = IPOS
      JJPOS = JPOS
      IF(IIPOS.EQ.0)THEN
         IF(.NOT.HLABEQ(ID,'X'))THEN
            CALL HBUG('Could not reference bin on axis X','HFC2',ID)
            CALL HBUG('No labels : CALL HLABEL first on axis X',
     +        'HFC2',ID)
            RETURN
         ENDIF
         CALL HLPOS(ID,CHX,IIPOS,'X')
      ENDIF
      IF(JJPOS.EQ.0)THEN
         IF(.NOT.HLABEQ(ID,'Y'))THEN
            CALL HBUG('Could not reference bin on axis Y','HFC2',ID)
            CALL HBUG('No labels : CALL HLABEL first on axis Y',
     +        'HFC2',ID)
            RETURN
         ENDIF
         CALL HLPOS(ID,CHY,JJPOS,'Y')
      ENDIF
C ----------------------------------------------------------------------
C LIMITS PROBLEMS ? GO TO OVERFLOW                                   BAD
C We use the underflow channel if the position IIPOS or JJPOS is out of
C limit
C ----------------------------------------------------------------------
      IF(HLABEQ(ID,'X'))THEN
         LCHX = LQ(LCID - 8)
         NCHX = IQ(LCHX + KNCHX)
      ELSE
         NCHX = IQ( LCID + KNCX )
      ENDIF
      IF(HLABEQ(ID,'Y'))THEN
         LCHY = LQ(LCID - 9)
         NCHY = IQ(LCHY + KNCHX)
      ELSE
         NCHY = IQ( LCID + KNCY )
      ENDIF
      IF((IIPOS.GT.NCHX).OR.(IIPOS.LT.0))THEN
         IIPOS = 0
      ENDIF
      IF((JJPOS.GT.NCHY).OR.(JJPOS.LT.0))THEN
         JJPOS = 0
      ENDIF
C ----------------------------------------------------------------------
C STORE THE VALUE
C ----------------------------------------------------------------------
      IQ(LCONT + KNOENT) = IQ(LCONT + KNOENT)+1
      ICHAN = (IQ(LCID + KNCY) - JJPOS + 1)*(IQ(LCID + KNCX) + 2) +
     +  IIPOS
      IF(IQ(LCONT + KNBIT).LT.32)THEN
         NBSCAT = IQ(LCONT + KNBIT)
         NB = 32/NBSCAT
         LWORD = ICHAN/NB
         LBIT = (NB - 1 - MOD(ICHAN,NB))*NBSCAT + 1
         IAD = LWORD + LCONT + KCON2
         INC = JBYT(IQ(IAD),LBIT,NBSCAT) + W + 0.5
         IF(INC.GT.MAXBIT(NBSCAT))INC = MAXBIT(NBSCAT)
         CALL SBYT(INC,IQ(IAD),LBIT,NBSCAT)
      ELSE
         LWORD = LCONT + ICHAN + KCON2
         Q(LWORD) = Q(LWORD) + W
      ENDIF
C ----------------------------------------------------------------------
      RETURN
      END
