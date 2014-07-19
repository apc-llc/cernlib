*
* $Id: pafith.F,v 1.3 1997/04/25 12:05:43 couet Exp $
*
* $Log: pafith.F,v $
* Revision 1.3  1997/04/25 12:05:43  couet
* - ana error message was printed when HI/FIT was used with option U
*
* Revision 1.2  1996/06/04 15:53:27  couet
* - Mods to handel properly the FILECASE KEEP
*
* Revision 1.1.1.1  1996/03/01 11:38:39  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/08 16/08/95  15.35.21  by  O.Couet
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAFITH
*
*           /HISTOGRAM/FIT
*
#include "hbook/hcbook.inc"
#include "paw/pawcom.inc"
#include "hbook/hcbits.inc"
#include "paw/pcpatl.inc"
#include "paw/pcchar.inc"
#include "paw/pcrang.inc"
#include "paw/quest.inc"
#include "paw/pcfitf.inc"
#include "paw/pcbuff.inc"
#include "paw/pcwk.inc"
#include "hbook/hcminpu.inc"
#include "paw/fpadr.inc"
#include "paw/fpcoms.inc"
*
      COMMON/PAWPAR/PAR(200)
*
      LOGICAL LODRAW
      CHARACTER*8 CHOPT,CHP
      DIMENSION VALP(50),SIGPAR(50)
      DIMENSION COV(1000),PMIN(50),PMAX(50),STEP(50)
      EQUIVALENCE (PAWBUF(1),VALP(1)),(PAWBUF(101),SIGPAR(1))
      EQUIVALENCE (PAWBUF(501),COV(1)),(PAWBUF(1501),PMIN(1))
      EQUIVALENCE (PAWBUF(1601),PMAX(1)),(PAWBUF(1701),STEP(1))
      EXTERNAL PAWFUN,PAWFUY,PAWSIM
*.______________________________________
*
#if defined(CERNLIB_FPANELS)
* With option M, FPHSTART is called in PAW++, and PMNCOMD in PAW
      IF (IWK.EQ.999) IADINP = JMFPH
*
#endif
      VPARNAM   = ' '
      VLOWNAM   = ' '
      VUPPNAM   = ' '
      VSTEPNAM  = ' '
      VERRORNUM = ' '
*
      CALL KUPATL(CHPATL,NPAR)
*
      NOPER=0
      LODRAW=.TRUE.
      CALL KUGETC(CHID,N)
      CALL HGETID(CHID)
      IF(LCID.LE.0)GO TO 999
      IF (LFIX.NE.0) THEN
         CALL HBUG('Cannot fit projections. Do a copy first',
     +             'PAFITH',ID)
         GO TO 999
      ENDIF
      CALL KUGETF(CHFILE,NCHF)
CCC      CALL CLTOU(CHFILE)
      IF (NCHF.LE.0)THEN
         CALL HSETCD
         GO TO 999
      ENDIF
*
      CALL PAFITF(CHFILE,NP)
      IF(NOPER.EQ.0)THEN
         CALL PAWFCA(CHFILE,NCHF,JAD,1)
         IF(JAD.EQ.0)THEN
            CALL HSETCD
            GO TO 999
         ENDIF
      ELSE
         JAD=0
      ENDIF
      IF(NPAR.LT.5)THEN
         IF(JAD.NE.0.OR.NOPER.GT.1)THEN
            CALL HBUG('Missing parameters','PAFITH',ID)
            CALL HSETCD
            GO TO 999
         ENDIF
      ENDIF
      CALL KUGETC(CHP,NCHOPT)
      IF(ICRANG.EQ.0)THEN
         CHOPT=CHP
      ELSE
         CHOPT='R'//CHP
         NCHOPT=NCHOPT+1
      ENDIF
      CHFOPTN = CHOPT
      IOPTN=INDEX(CHOPT,'N')
      IOPT0=INDEX(CHOPT,'0')
      IOPTB=INDEX(CHOPT,'B')
      IOPTU=INDEX(CHOPT,'U')
      IOPTE=INDEX(CHOPT,'E')
      IOPTL=INDEX(CHOPT,'L')
      IF(IOPTN.NE.0)IOPT0=1
      IF(IOPT0.NE.0.OR.I1.EQ.0)LODRAW=.FALSE.
      IF(JAD.EQ.0.AND.NOPER.EQ.1)THEN
         IC=2
         IOPTQ=INDEX(CHOPT,'Q')
         IOPTV=INDEX(CHOPT,'V')
         IOPTW=INDEX(CHOPT,'W')
         IOPTM=INDEX(CHOPT,'M')
         IOP2=10
         IF(IOPTN.NE.0)IC=0
         IF(IOPTV.NE.0)IOPTQ=0
         IF(IOPTQ.NE.0)IOP2=0
         IF(IOPTV.NE.0.AND.IOPTE.NE.0)IOP2=60
         IF(IOPTV.NE.0.AND.IOPTE.EQ.0)IOP2=50
         IF(IOPTV.EQ.0.AND.IOPTE.NE.0)IOP2=20
         IC=IC+IOP2
         IF(IOPTW.NE.0)IC=IC+100
         IF(IOPTM.NE.0)IC=IC+10000
         IF(ICRANG.NE.0)IC=IC+100000
         IF(IOPTL.NE.0)IC=IC+1000000
      ENDIF
      CALL KUGETI(NPP)
      IF(JAD.NE.0)NP=NPP
      LPAR1=0
      LPAR2=0
      IF(NPAR.GE.5)CALL KUGETV(VPARNAM,LPAR1,LPAR2)
      IF(LPAR1.NE.0.AND.LPAR2-LPAR1+1.LT.NP)THEN
         CALL HBUG('Vector of parameters too short','PAFITH',ID)
         CALL HSETCD
         GO TO 999
      ENDIF
      DO 10 I=1,NP
         IF(LPAR1.NE.0)PAR(I)=Q(LPAR1+I-1)
         STEP(I)=-1.
         PMIN(I)=0.
         PMAX(I)=0.
   10 CONTINUE
*
      LSTEP1=0
      LPMIN1=0
      LPMAX1=0
      IF(NPAR.GE.6)CALL KUGETV(VSTEPNAM,LSTEP1,LSTEP2)
      IF(NPAR.GE.7)CALL KUGETV(VLOWNAM,LPMIN1,LPMIN2)
      IF(NPAR.GE.8)CALL KUGETV(VUPPNAM,LPMAX1,LPMAX2)
      IF(IOPTB.NE.0)THEN
         IF(LSTEP1.NE.0)CALL UCOPY(Q(LSTEP1),STEP,NP)
         IF(LPMIN1.NE.0)CALL UCOPY(Q(LPMIN1),PMIN,NP)
         IF(LPMAX1.NE.0)CALL UCOPY(Q(LPMAX1),PMAX,NP)
      ENDIF
      LERR1=0
      LERR2=0
      IF(NPAR.GE.9)CALL KUGETV(VERRORNUM,LERR1,LERR2)
      CALL KUALFA
      IF(ICRANG.NE.0)THEN
         IQUEST(11)=ICX1
         IQUEST(12)=ICX2
         IQUEST(13)=ICY1
         IQUEST(14)=ICY2
      ENDIF
      IF(JAD.NE.0)THEN
         IF(I1.NE.0)THEN
            CALL HFITH(ID,PAWFUN,CHOPT,NP,PAR,STEP,PMIN,PMAX, SIGPAR,
     +      CHI2)
         ELSE
            CALL HFITH(ID,PAWFUY,CHOPT,NP,PAR,STEP,PMIN,PMAX, SIGPAR,
     +      CHI2)
         ENDIF
      ELSE
         IF(NOPER.GT.1.OR.NPP.EQ.NP)THEN
            CALL HFITH(ID,PAWSIM,CHOPT,NP,PAR,STEP,PMIN,PMAX, SIGPAR,
     +      CHI2)
         ELSE
            IF(IFTYPE(1).EQ.1)THEN
               CALL HFITGA(ID,PAR(1),PAR(2),PAR(3),CHI2,IC,SIGPAR)
            ELSEIF(IFTYPE(1).EQ.2)THEN
               CALL HFITEX(ID,PAR(1),PAR(2),CHI2,IC,SIGPAR)
            ELSEIF(IFTYPE(1).EQ.3)THEN
               CALL HFITPO(ID,NP,PAR,CHI2,IC,SIGPAR)
            ENDIF
         ENDIF
      ENDIF
      DO 20 I=1,NP
         IF(LPAR1.NE.0)Q(LPAR1+I-1)=PAR(I)
         IF(NPAR.GE.9.AND.LERR2-LERR1+1.GE.NP)THEN
            Q(LERR1+I-1)=SIGPAR(I)
         ENDIF
   20 CONTINUE
*
*            Is plot required ?
*
#if defined(CERNLIB_FPANELS)
      IF (INDEX(CHOPT,'M').NE.0.AND.IWK.EQ.999) LODRAW = .FALSE.
#endif
      IF (LODRAW) THEN
#if defined(CERNLIB_IBM)
         IF(IWK.GT.10)THEN
            CALL KUPROC('Type CR to continue',CHP,NCH)
         ENDIF
#endif
         DO 30 I=1,NCHOPT
            IF(CHOPT(I:I).EQ.'L')CHOPT(I:I)='X'
            IF(CHOPT(I:I).EQ.'B')CHOPT(I:I)='X'
   30    CONTINUE
         IF(IWK.NE.0)THEN
            IF(IOPTB.NE.0)CHOPT(IOPTB:IOPTB)='?'
            IF(IOPTE.NE.0)CHOPT(IOPTE:IOPTE)='?'
            IF(IOPTL.NE.0)CHOPT(IOPTL:IOPTL)='?'
            IF(IOPTU.NE.0)CHOPT(IOPTU:IOPTU)='?'
            CALL HPLOT(ID,CHOPT,' ',0)
         ELSEIF(IWK.EQ.0.OR.IWK.EQ.-2)THEN
            CALL HPRINT(ID)
         ENDIF
      ENDIF
*
      CALL HSETCD
*
  999 END
