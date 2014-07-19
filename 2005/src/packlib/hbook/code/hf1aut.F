*
* $Id: hf1aut.F,v 1.1.1.1 1996/01/16 17:07:35 mclareni Exp $
*
* $Log: hf1aut.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:35  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 26/04/93  11.57.51  by  Rene Brun
*-- Author :
      SUBROUTINE HF1AUT(X,W)
*.==========>
*.           computes bin width at filling time
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcprin.inc"
*.___________________________________________
#include "hbook/jbyt.inc"
      LAUTO=LQ(LCONT-2)
      IF(LAUTO.LE.0)GO TO 99
      NOENT=IQ(LCONT+KNOENT)
*
*              Special case for first entry
*
      IF(NOENT.EQ.0)THEN
         BWID=0.001*ABS(X)
         IF(BWID.EQ.0.)BWID=1.E-30
         XMIN=X-BWID
         XMAX=X+BWID
      ELSE
*
*              Find mean value and RMS.
*
         XMEAN=Q(LAUTO+1)
         XRMS=XMEAN*XMEAN
         XXMIN=XMEAN
         XXMAX=XMEAN
         DO 10 I=2,NOENT
            XX=Q(LAUTO+2*I-1)
            XMEAN=XMEAN+XX
            XRMS=XRMS+XX*XX
            XXMIN=MIN(XX,XXMIN)
            XXMAX=MAX(XX,XXMAX)
  10     CONTINUE
         XMEAN=XMEAN/NOENT
         XRMS=SQRT(ABS(XRMS/NOENT -XMEAN*XMEAN))
*
*               Find nice binning
*
         CALL HBIN(0.,6.*XRMS,IQ(LCID+KNCX),XMIN,X1,ICN,BWID)
*
         DXM=IQ(LCID+KNCX)*BWID/2.
         IF(XXMIN.GE.0.)THEN
            XMIN=XXMIN-MOD(XXMIN,10.*BWID)
            IF(XMIN.LT.0.)XMIN=0.
            IF(XXMAX-XXMIN.GT.XXMIN)XMIN=0.
         ENDIF
         IF(XXMAX.LE.0.)THEN
            XMAX=XXMAX-MOD(XXMAX,10.*BWID)
            IF(XMAX.GT.0.)XMAX=0.
            IF(XXMIN-XXMAX.LT.XXMAX)XMAX=0.
            XMIN=XMAX-2.*DXM
         ENDIF
         IF(XXMIN*XXMAX.LT.0..AND.(XMEAN-DXM)*(XMEAN+DXM).LT.0.)THEN
            XMIN=XMEAN-MOD(XMEAN,10.*BWID)-DXM
         ENDIF
         XMAX=XMIN+2.*DXM
      ENDIF
*
*              Reset old entries (except statistics)
*              and fill again if new binning
*
      IF(XMIN.NE.Q(LCID+KXMIN).OR.XMAX.NE.Q(LCID+KXMAX))THEN
         CALL VZERO(IQ(LCONT+KCON1),IQ(LCONT-1)-KCON1+1)
         LW=LQ(LCONT)
         IF(LW.NE.0)CALL VZERO(IQ(LW+1),IQ(LW-1))
         Q(LCID+KXMIN)=XMIN
         Q(LCID+KXMAX)=XMAX
         DO 20 I=1,NOENT
            XX=Q(LAUTO+2*I-1)
            WW=Q(LAUTO+2*I)
            ICHAN=(XX-XMIN)/BWID +1
            IF(ICHAN.LE.0)ICHAN=0
            IF(ICHAN.GT.IQ(LCID+KNCX))ICHAN=IQ(LCID+KNCX)+1
            IF(IQ(LCONT+KNBIT).GE.32)THEN
               Q(LCONT+ICHAN+KCON1)=Q(LCONT+ICHAN+KCON1)+WW
            ELSE
               NBPROX=IQ(LCONT+KNBIT)
               NB=32/NBPROX
               LWORD=ICHAN/NB
               LBIT=(ICHAN-NB*LWORD)*NBPROX+1
               INC=JBYT(IQ(LCONT+LWORD+KCON1),LBIT,NBPROX)
               INC=INC+W+0.5
               IF(INC.GT.MAXBIT(NBPROX))INC=MAXBIT(NBPROX)
               CALL SBYT(INC,IQ(LCONT+LWORD+KCON1),LBIT,NBPROX)
            ENDIF
            IF(LW.NE.0)THEN
               IF(ICHAN.GT.0.AND.ICHAN.LE.IQ(LCID+KNCX))THEN
                  Q(LW+ICHAN)=Q(LW+ICHAN)+WW*WW
               ENDIF
            ENDIF
  20     CONTINUE
      ENDIF
*
*              Stop automatic binning algorithm after 50 entries
*
      IF(NOENT.LT.100)THEN
         Q(LAUTO+2*NOENT+1)=X
         Q(LAUTO+2*NOENT+2)=W
      ELSE
         CALL MZDROP(IHDIV,LAUTO,' ')
         CALL SBIT0(IQ(LCID+KBITS),5)
         I5=0
      ENDIF
*
  99  RETURN
      END
