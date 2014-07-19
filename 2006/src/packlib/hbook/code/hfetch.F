*
* $Id: hfetch.F,v 1.1.1.1 1996/01/16 17:07:36 mclareni Exp $
*
* $Log: hfetch.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:36  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/06 11/12/90  09.47.14  by  Rene Brun
*-- Author :
      SUBROUTINE HFETCH(IDD,LUN1)
*.==========>
*.            Reads histograms previously stored with HBOOK3 routine
*.            HSTORE.
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcprin.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcform.inc"
      CHARACTER*80 KTITL
      DIMENSION IHCV(99)
      EQUIVALENCE (IHCV(1),HCV(1))
*.___________________________________________
      N=0
      LUN=IABS(LUN1)
      IF(LUN1.GT.0)THEN
         REWIND LUN
      ENDIF
*
  10  CONTINUE
      READ(LUN,END=900)ID1,NWHIST,IC
      CALL HSPACE(2*NWHIST+100,'HFETCH',IDD)
      IF(IERR.NE.0)GO TO 999
      IF(ID1.EQ.0)GO TO 900
* This is necessary for an old NORD compiler
#if (defined(CERNLIB_NORD))&&(defined(CERNLIB_AREAD))
      READ(LUN,END=900)IDUM,(HCV(K),K=1,NWHIST)
#endif
#if !defined(CERNLIB_AREAD)
      READ(LUN,ERR=900,END=900)(HCV(K),K=1,NWHIST)
#endif
      IF(IDD.NE.0.AND.ID1.NE.IDD)GO TO 10
*
      ID=ID1
      N=1
      IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
      IF(IDPOS.GT.0)THEN
         CALL HDELET(ID1)
         CALL HBUG('Already existing histogram replaced','HFETCH',ID1)
      ENDIF
*
      K1=JBIT(IC,1)
      K2=JBYT(IC,2,2)
      IF(K1.EQ.0.AND.K2.EQ.0)THEN
         CALL HARRAY(ID1,NWHIST,ILOC)
         CALL UCOPY(IHCV,IQ(LCID+1),NWHIST)
         GO TO 800
      ENDIF
*
      JNEXT=IHCV(1)+1
      IFW=JNEXT+IHCV(JNEXT+2)
      NBP=IHCV(IFW-1)
      IF(NBP.GE.32)THEN
         VMX=0.
      ELSE
         VMX=MAXBIT(NBP)
      ENDIF
      NCX=IHCV(2)
      XMIN=HCV(3)+0.000001/HCV(5)
      XMAX=HCV(4)
      IWT=6
      IF(K2.NE.0)THEN
         NCY=IHCV(6)
         YMIN=HCV(7)+0.000001/HCV(9)
         YMAX=HCV(8)
         IWT=IWT+4
      ENDIF
      KTITL=' '
      NCTIT=NCHAR*(IHCV(1)-IWT+1)
      NWT=MIN(79,NCTIT)
      IF(NWT.GT.0)THEN
         CALL UHTOC(IHCV(IWT),4,KTITL,NWT)
      ENDIF
      IF(K1.NE.0)THEN
         CALL HBOOK1(ID1,KTITL,NCX,XMIN,XMAX,VMX)
         LCONT=LQ(LCID-1)
         LPRX=LCID+KNCX
         DO 11 I=0,NCX+1
            IF(NBP.EQ.NBIT)THEN
               W=HCV(IFW+I+1)
            ELSE
               L1=I*NBP
               NBITH=NBIT-MOD(NBIT,NBP)
               L2=MOD(L1,NBITH)+1
               L1=IFW+1+L1/NBITH
               W=JBYT(IHCV(L1),L2,NBP)
            ENDIF
            CALL HFCX(I,W)
  11     CONTINUE
         IF(JBIT(IC,9).NE.0)THEN
            CALL MZBOOK(IHDIV,LW,LCONT,0,'HI1E',0,0,NCX,3,0)
            IQ(LCID+KNTOT)=IQ(LCID+KNTOT)+NCX+10
            NBB=NBIT/NBP
            NW=(NCX+2)/NBB+1
            LW=LQ(LCONT)
            DO 12 I=1,NCX
               J=IFW+I+NW+1
               Q(LW+I)=HCV(J)
  12        CONTINUE
         ENDIF
         IF(JBIT(IC,12).NE.0)THEN
            CALL MZBOOK(IHDIV,LFUNC,LCONT,-1,'HFUN',0,0,NCX+2,IOCF2,0)
            IQ(LCID+KNTOT)=IQ(LCID+KNTOT)+NCX+12
            L2=1+IHCV(1)
            LFUNC=LQ(LCONT-1)
            IQ(LFUNC+1)=1
            IQ(LFUNC+2)=NCX
            DO 13 I=1,NCX
               J=L2+IHCV(L2)+I+1
               Q(LFUNC+I+2)=HCV(J)
  13        CONTINUE
         ENDIF
      ELSE
         CALL HBOOK2(ID1,KTITL,NCX,XMIN,XMAX,NCY,YMIN,YMAX,VMX)
         NW=NBIT/NBP
         NB=NBP
         DO 15 J=0,NCY+1
            DO 14 I=0,NCX+1
               K=(NCY-J+2)*(NCX+3)
               L2=I+K
               L1=L2/NW+IFW+1
               IF(NW.EQ.1)THEN
                  W=HCV(L1)
               ELSE
                  L2=(NW-1-MOD(L2,NW))*NBP+1
                  W=JBYT(IHCV(L1),L2,NBP)
               ENDIF
               CALL HFCXY(I,J,W)
  14        CONTINUE
  15     CONTINUE
      ENDIF
      IQ(LCONT+KNOENT)=IHCV(IFW)
      IQ(LCID+KBITS)=IC
      CALL HDCOFL
      IF(I17.NE.0)CALL HBIGBI(ID1,HCV(NWHIST-I17-I18-I19-I20-I21))
      IF(I18.NE.0)CALL HNORMA(ID1,HCV(NWHIST-I18-I19-I20-I21))
      IF(I19.NE.0)CALL HSCALE(ID1,HCV(NWHIST-I19-I20-I21))
      IF(I20.NE.0)CALL HMAXIM(ID1,HCV(NWHIST-I20-I21))
      IF(I21.NE.0)CALL HMINIM(ID1,HCV(NWHIST-I21))
C
  20  JNEXT=JNEXT+IHCV(JNEXT)
      IF(IHCV(JNEXT).EQ.0)GO TO 800
      JTYPE=IHCV(JNEXT+1)
      IF(JTYPE.EQ.9)GO TO 800
      IF(JTYPE.EQ.5.OR.JTYPE.EQ.6)THEN
         IFW=JNEXT+IHCV(JNEXT+3)
      ELSE
         IFW=JNEXT+IHCV(JNEXT+2)
      ENDIF
      NBP=IHCV(IFW-1)
      IF(NBP.GE.32)THEN
         VMX=0.
      ELSE
         VMX=MAXBIT(NBP)
      ENDIF
C
      GO TO(20,20,50,60,70,80,100,110,800),JTYPE
C
C             PROJECTION X
C
  50  CALL HBPROX(ID1,VMX)
      LCONT=LQ(LCID-2)
      LPRX=LCID+KNCX
      CALL HFETC1(NBP,NCX)
      GO TO 20
C
C             PROJECTION Y
C
  60  CALL HBPROY(ID1,VMX)
      LCONT=LQ(LCID-3)
      LPRX=LCID+KNCY
      CALL HFETC1(NBP,NCY)
      GO TO 20
C
C             SLICES X
C
  70  NSL=IHCV(JNEXT+2)
      CALL HBSLIX(ID1,NSL,VMX)
      LPRX=LCID+KNCX
      DO 78 NUM=1,NSL
         IFW=JNEXT+IHCV(JNEXT+NUM+2)
         LCONT=LQ(LSLIX-NUM)
         CALL HFETC1(NBP,NCX)
  78  CONTINUE
      GO TO 20
C
C             SLICES Y
C
  80  NSL=IHCV(JNEXT+2)
      CALL HBSLIY(ID1,NSL,VMX)
      LPRX=LCID+KNCY
      DO 88 NUM=1,NSL
         IFW=JNEXT+IHCV(JNEXT+NUM+2)
         LCONT=LQ(LSLIY-NUM)
         CALL HFETC1(NBP,NCY)
  88  CONTINUE
      GO TO 20
C
C             BAND  X
C
 100  YLOW=HCV(JNEXT+3)
      YUP =HCV(JNEXT+4)
      CALL HBANDX(ID1,YLOW,YUP,VMX)
      LPRX=LCID+KNCX
      CALL HFETC1(NBP,NCX)
      GO TO 20
C
C             BAND  Y
C
 110  XLOW=HCV(JNEXT+3)
      XUP =HCV(JNEXT+4)
      CALL HBANDY(ID1,XLOW,XUP,VMX)
      LPRX=LCID+KNCY
      CALL HFETC1(NBP,NCY)
      GO TO 20
*
 800  IF(IDD.EQ.0)GO TO 10
*
 900  IF(IDD.NE.0.AND.N.EQ.0)
     +CALL HBUG('Unknown histogram','HFETCH',IDD)
*
 999  RETURN
      END
