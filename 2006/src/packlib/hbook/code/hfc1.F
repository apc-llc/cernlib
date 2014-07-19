*
* $Id: hfc1.F,v 1.3 2001/10/10 08:12:07 couet Exp $
*
* $Log: hfc1.F,v $
* Revision 1.3  2001/10/10 08:12:07  couet
* - Do not call MZFORM any more. MZFORM for alphanumeric labels date banks
*   is now called  at initialisation time (HLIMIT or HLIMAP).
*
* Revision 1.2  1998/12/02 09:05:19  couet
* - clean up
*
* Revision 1.1.1.1  1996/01/16 17:07:36  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :    P.Aubert   18/11/92
      SUBROUTINE HFC1(ID1,IPOS,CHX,W,CHOPT)
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C HFC1
C ID1   : Histogram identifier
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CHX   : Channel
C W     :
C CHOPT : Options 'N' Normal filling
C                 'S' or default automatically Sort
C                 'U' UNDEFLOW IF 'U' IS SET AND THE CHANNEL DOESN T
C                     EXIST THEN THE UNDERFLOW CHANNEL IS INCREMENTED
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcprin.inc"
#include "hbook/hcform.inc"
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C PARAMETER
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INTEGER KLGRCX,KBLAB,KNCHX
      PARAMETER(KLGRCX=16,KBLAB=7,KNCHX=2)
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C COMMON
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INTEGER LCHX,NCHX
      EQUIVALENCE(LCHX ,LHDUM(1))
      INTEGER LOCATI
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INTEGER HLCCMP,LENOCC
      LOGICAL HLABEQ
      REAL HCX
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INTEGER ID1,IPOS,JBIT,NBPROX,IKNOENT
      CHARACTER*(*) CHX
      REAL W
      CHARACTER*(*) CHOPT
C ----------------------------------------------------------------------
      INTEGER NCCHX,ICHAN,IDPOS,IOPT(3),LEFT,RIGHT,MEDIAN,CMP,I,J,
     +     L1,L2,LW,IOCC
      REAL Z,SWX(2)
      CHARACTER*16 CHANNELX
      LOGICAL EXIST
C ----------------------------------------------------------------------
C Init value
C ----------------------------------------------------------------------
      IF(ID1.NE.IDLAST)THEN
         ID     = ID1
         IDPOS  = LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
         IF(IDPOS.LE.0)RETURN
         IDLAST = ID1
         LCID   = LQ(LTAB-IDPOS)
         LCONT  = LQ(LCID-1)
      ENDIF
      IKNOENT= IQ(LCONT+KNOENT)
C -- options ???
      CALL UOPTC(CHOPT,'NSU',IOPT)
C ----------------------------------------------------------------------
C IF THE BANK CHX DOESN T EXIST
C ----------------------------------------------------------------------
      IF(.NOT.HLABEQ(ID,' '))THEN
C -- add link
         NBLINK=IQ(LCID-2)
         CALL MZPUSH(IHDIV,LCID,8-NBLINK,0,' ')
C -- create LCHX : WARNING NO PACKING IODES=IOCF1
         CALL MZBOOK(IHDIV,LCHX,LCID,-8,'HLCX',0,0,KBLAB,IOCC,0)
C -- first word is the number of character in each label
         IQ(LCHX+1)=KLGRCX
C -- second word is the number of channel ( we don t count
C                                             underflow )
         IQ(LCHX+2)=0
C -- initialise channel 0 : by default underflow
         CALL UCTOH('UNDERFLOW       ',IQ(LCHX+3),4,KLGRCX)
         Q(LCONT+KCON1)=0.0
C -- describe id dim
         CALL SBIT0(IQ(LCHX),17)
         CALL SBIT0(IQ(LCHX),18)
C -- update binwidth xmin and xmax
         Q(LCID+KXMIN)   = 1.0
         Q(LCID+KXMAX)   = 2.0
         IQ(LCID+KNCX)   = 1
      ENDIF
      LCHX   = LQ(LCID-8)
      NBPROX = IQ(LCONT+KNBIT)
      NB     = 32/NBPROX
      NCHX   = IQ(LCHX+KNCHX)
C ----------------------------------------------------------------------
C Find channel
C ----------------------------------------------------------------------
      IF(IPOS.NE.0)THEN
         EXIST = .TRUE.
         ICHAN = IPOS
         IF((IPOS.GT.NCHX).OR.(IPOS.LT.0))THEN
            CALL HBUG('IPOS out of range','HFC1',ID1)
            RETURN
         ENDIF
      ELSE
C 2/ be care of length must be equal KLGRCX
         CHANNELX(1:KLGRCX) = '                '
         NCCHX = LENOCC(CHX)
         IF(NCCHX.GT.KLGRCX) THEN
            NCCHX=KLGRCX
         ENDIF
         CHANNELX(1:NCCHX) = CHX(1:NCCHX)
C 3/ does the channel exist ?
         EXIST = .FALSE.
C 31/ linear search
         IF(IOPT(1).EQ.1)THEN
C           -- set the first status bit at 1
            CALL SBIT0(IQ(LCHX),1)
            IF(NCHX.GE.1)THEN
               ICHAN=0
   10          ICHAN=ICHAN+1
               IF(HLCCMP(IQ(LCHX+KBLAB+(ICHAN-1)*4),
     +              CHANNELX,KLGRCX).EQ.0)THEN
                  EXIST = .TRUE.
               ENDIF
C
               IF((ICHAN.LT.NCHX).AND.(.NOT.(EXIST))) GO TO 10
            ENDIF
C -- pour verifier l invariant de boucle
            IF(.NOT.EXIST)THEN
               ICHAN = NCHX + 1
            ENDIF
         ELSE
C 32/ binary search
C           -- set the first status bit at 0
            CALL SBIT1(IQ(LCHX),1)
            LEFT  = 1
            RIGHT = NCHX
C            -- while
   20       IF((LEFT.LE.RIGHT).AND.(.NOT.EXIST))THEN
               MEDIAN = ( LEFT + RIGHT ) /2
               CMP = HLCCMP(IQ(LCHX+KBLAB+(MEDIAN-1)*4),
     +              CHANNELX,KLGRCX)
               IF(CMP.LT.0)THEN
                  LEFT = MEDIAN+1
               ELSE IF(CMP.EQ.0)THEN
                  EXIST = .TRUE.
                  ICHAN = MEDIAN
               ELSE
                  RIGHT = MEDIAN-1
               ENDIF
               GO TO 20
            ENDIF
            IF(.NOT.EXIST)THEN
               ICHAN = LEFT
            ENDIF
         ENDIF
      ENDIF
C ----------------------------------------------------------------------
C IF 'U' IS SET
C ----------------------------------------------------------------------
      IF(IOPT(3).EQ.1)THEN
         IF(.NOT.EXIST)THEN
            ICHAN = 0
            EXIST = .TRUE.
         ENDIF
      ENDIF
C ----------------------------------------------------------------------
C Compute the new value if channel exist else create it
C ----------------------------------------------------------------------
      IF(.NOT.EXIST)THEN
         NCHX             = NCHX+1
         IQ(LCHX+KNCHX)   = NCHX
         IQ(LCID+KNCX)    = NCHX
         Q(LCID+KXMAX)    = FLOAT(NCHX)+1.0
C NEED MORE SPACE ?
         IF(((IQ(LCHX-1)-1)/4).LE.NCHX)THEN
C ALLOCATION IN BANK 'LCONT' AND 'LCHX'
C TRY WITH 100 NEW CHANNEL
            L1 = 100 / NB
            L2 = 400 / NB
            CALL HSPACE(L1+L2,'HFC1',ID)
            IF(IERR.EQ.0)THEN
               CALL MZPUSH(IHDIV,LCONT,0,L1,' ')
               CALL MZPUSH(IHDIV,LCHX ,0,L2,' ')
            ELSE
C TRY WITH 1
               L1 = 1 / NB
               L2 = 4 / NB
               CALL HSPACE(L1+L2,'HFC1',ID)
               IF(IERR.EQ.0)THEN
                  CALL MZPUSH(IHDIV,LCONT,0,L1,' ')
                  CALL MZPUSH(IHDIV,LCHX ,0,L2,' ')
               ELSE
                  CALL HBUG('Not enough space in memory','HFC1',ID)
                  RETURN
               ENDIF
            ENDIF
         ENDIF
C -- ICHAN = numero du canal a inserer
C -- NCHX a deja la taille du nouveau tableau
         IF(ICHAN.NE.NCHX)THEN
C           -- shift label ( newer packed )
            DO 100 I=NCHX,ICHAN+1,-1
               DO 110 J=1,4
                  IQ(LCHX+KBLAB+4*(I-1)+J-1)=IQ(LCHX+KBLAB+4*(I-2)+J-1)
  110          CONTINUE
  100       CONTINUE
C HORRIBLE HACK see hcx.f
            NB = NBPROX
C           -- shift cont
            DO 130 I=NCHX,ICHAN+1,-1
               CALL HFCX(I,HCX(I-1,1))
  130       CONTINUE
         ENDIF
         CALL UCTOH(CHANNELX,IQ(LCHX+KBLAB+4*(ICHAN-1)),4,KLGRCX)
C -- ADD AN ENTRY
      ENDIF
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C -- Value W
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF(EXIST)THEN
C HORRIBLE HACK see hcx.f
         NB = NBPROX
         CALL HFCX(ICHAN,HCX(ICHAN,1)+W)
      ELSE
         CALL HFCX(ICHAN,W)
      ENDIF
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C -- ERROR
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF(LQ(LCONT).NE.0)THEN
         IF(ICHAN.GT.0.AND.ICHAN.LE.IQ(LPRX))THEN
            LW=LQ(LCONT)
            Q(LW+ICHAN)=Q(LW+ICHAN)+W*W
         ENDIF
      ENDIF
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C -- STATISTIQUES
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      I7 = JBIT(IQ(LCID+KBITS),7)
      IF(I7.NE.0)THEN
         IF(ICHAN.GT.0.AND.ICHAN.LE.IQ(LPRX))THEN
            Z=ABS(W)
            Q(LCONT+KSTAT1)=Q(LCONT+KSTAT1)+Z
            Q(LCONT+KSTAT1+1)=Q(LCONT+KSTAT1+1)+Z*Z
            CALL UCOPY(Q(LCONT+KSTAT1+2),SWX,4)
C            SWX(1)=SWX(1)+Z*X
C            SWX(2)=SWX(2)+Z*X*X
            CALL UCOPY(SWX,Q(LCONT+KSTAT1+2),4)
         ENDIF
      ENDIF
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C -- ADD ONE ENTRY
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IQ(LCONT+KNOENT)     = IKNOENT + 1
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      RETURN
      END
