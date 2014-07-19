*
* $Id: pafitv.F,v 1.3 1999/08/31 08:47:16 couet Exp $
*
* $Log: pafitv.F,v $
* Revision 1.3  1999/08/31 08:47:16  couet
* - A new set of PAW commands using Neural Networks.
*   These new commands are using the MLPfit package.
*
* Revision 1.2  1996/06/04 15:53:31  couet
* - Mods to handel properly the FILECASE KEEP
*
* Revision 1.1.1.1  1996/03/01 11:38:39  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/08 16/08/95  15.36.13  by  O.Couet
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAFITV
*
*           /VECTOR/FIT
*
#include "hbook/hcbook.inc"
#include "paw/pawcom.inc"
#include "paw/pcpatl.inc"
#include "paw/pcchar.inc"
#include "paw/quest.inc"
#include "paw/pawnpu.inc"
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
      CHARACTER*32 CHOPT,CHP
#if defined(CERNLIB_MLP)
      CHARACTER*32 CHEXPR
#endif
      DIMENSION VALP(50),SIGPAR(50)
      DIMENSION COV(1000),PMIN(50),PMAX(50),STEP(50)
      EQUIVALENCE (PAWBUF(1),VALP(1)),(PAWBUF(101),SIGPAR(1))
      EQUIVALENCE (PAWBUF(501),COV(1)),(PAWBUF(1501),PMIN(1))
      EQUIVALENCE (PAWBUF(1601),PMAX(1)),(PAWBUF(1701),STEP(1))
      EXTERNAL PAWFUN,PAWSIM,PAWFITZ
      DATA SQRT2P /2.506628/
*.______________________________________
*
#if defined(CERNLIB_FPANELS)
* With option M, FPVSTART is called in PAW++, and PMNCOMD in PAW
      IF (IWK.EQ.999) IADINP = JMFPV
*
#endif
      XVECNAM   = ' '
      YVECNAM   = ' '
      EYVECNAM  = ' '
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
      IEY=1
      CALL KUGETV(XVECNAM,LX1,LX2)
      NDIM=IQUEST(31)-IQUEST(21)+1
      NVAR=IQUEST(32)-IQUEST(22)+1
      CALL KUGETV(YVECNAM,LY1,LY2)
      CALL KUGETV(EYVECNAM,LEY1,LEY2)
      NX=LX2-LX1+1
      NY=LY2-LY1+1
      N=MIN(NX,NY)
      IF(NX*NY.LE.0)GO TO 999
      IF(EYVECNAM(1:1).EQ.'?')THEN
         IEY = 0
         EYVECNAM = ' '
      ENDIF
      CALL KUGETF(CHFILE,NCHF)
      IF (NCHF.LE.0)GO TO 999
      CALL PAFITF(CHFILE,NP)
#if defined(CERNLIB_MLP)
      IOPTNN = 0
      CHEXPR = ' '
      CHEXPR(1:2) = CHFILE(1:2)
      CALL CLTOU(CHEXPR)
      IF(CHEXPR(1:2) .EQ. 'NN') IOPTNN = 1
C
C fit using a multi-layer perceptron        
      IF(IOPTNN .EQ. 1) THEN
         CHP=' '
         CALL KUGETC(CHP,NCHOPT)
         IF(IEY.EQ.0)THEN
            CHOPT='W'//CHP
         ELSE
            CHOPT=CHP
         ENDIF
         IOPTN=INDEX(CHOPT,'N')
         IOPT0=INDEX(CHOPT,'0')
         IOPTB=INDEX(CHOPT,'B')
         IOPTP=INDEX(CHOPT,'+')
         IOPTV=INDEX(CHOPT,'V')
         IOPTI=INDEX(CHOPT,'I')
         IF(IOPTN.NE.0)IOPT0=1
         IF(IOPT0.NE.0)LODRAW=.FALSE.
         IF(IWK.EQ.0)LODRAW=.FALSE.
C
C parameters for MLP fit      
         INIT  = 1
         IVERB = 0
	 INIT0 = 0
         NEP   = 100
         IF (IOPTP.NE.0) INIT=0
         IF (IOPTV.NE.0) IVERB=1
	 IF (IOPTI.NE.0) INIT0=1
         CALL KUGETE(CHEXPR,LENGTH)
         CALL KICTOI(CHEXPR, INEP)
         IF(INEP.GT.0) NEP=INEP
C
C number of neurons
         IVIRG = INDEX(CHFILE,',')
         IF(IVIRG .EQ. 0) THEN		 
            CALL KICTOI(CHFILE(3:),Nneur1)
	    Nneur2 = 0
         ELSE
	    CALL KICTOI(CHFILE(3:IVIRG-1),Nneur1)
	    CALL KICTOI(CHFILE(IVIRG+1:),Nneur2)
         ENDIF
C
C call the fitting routine	 
         CALL MLPFIT(N,Q(LX1),Q(LY1),Q(LEY1),Nneur1,Nneur2,init,init0,
     >               iverb,nep)         
      ELSE
#endif
         IF(NOPER.EQ.0)THEN
            CALL PAWFCA(CHFILE,NCHF,JAD,1)
            IF(JAD.EQ.0)GO TO 999
         ELSE
            JAD=0
         ENDIF
         IF(NPAR.LT.7)THEN
            IF(JAD.NE.0.OR.NOPER.GT.1)THEN
               CALL HBUG('Missing parameters','PAFITV',0)
               GO TO 999
            ENDIF
         ENDIF
         CHP=' '
         CALL KUGETC(CHP,NCHOPT)
         IF(IEY.EQ.0)THEN
            CHOPT='W'//CHP
         ELSE
            CHOPT=CHP
         ENDIF
* Save CHOPT for use in Vector Fit Panel
         CHFOPTN = CHOPT
         CHPOPTN = CHP
*
         IOPTN=INDEX(CHOPT,'N')
         IOPT0=INDEX(CHOPT,'0')
         IOPTB=INDEX(CHOPT,'B')
         IOPTZ=INDEX(CHOPT,'Z')
         IF(IOPTN.NE.0)IOPT0=1
         IF(IOPT0.NE.0)LODRAW=.FALSE.
         IF(IWK.EQ.0)LODRAW=.FALSE.
         NPP=0
         CALL KUGETI(NPP)
         IF(JAD.NE.0)NP=NPP
         LPAR1=0
         LPAR2=0
         IF(NPAR.GE.7)CALL KUGETV(VPARNAM,LPAR1,LPAR2)
         IF(LPAR1.NE.0.AND.LPAR2-LPAR1+1.LT.NP)THEN
            CALL HBUG('Vector of parameters too short','PAFITV',ID)
            GO TO 999
         ENDIF
         DO 10 I=1,NP
            IF(LPAR1.NE.0)PAR(I)=Q(LPAR1+I-1)
            STEP(I)=-1.
            PMIN(I)=0.
            PMAX(I)=0.
   10    CONTINUE
*
         LSTEP1=0
         LPMIN1=0
         LPMAX1=0
         IF(NPAR.GE. 8)CALL KUGETV(VSTEPNAM,LSTEP1,LSTEP2)
         IF(NPAR.GE. 9)CALL KUGETV(VLOWNAM,LPMIN1,LPMIN2)
         IF(NPAR.GE.10)CALL KUGETV(VUPPNAM,LPMAX1,LPMAX2)
         IF(IOPTB.NE.0)THEN
            IF(LSTEP1.NE.0)CALL UCOPY(Q(LSTEP1),STEP,NP)
            IF(LPMIN1.NE.0)CALL UCOPY(Q(LPMIN1),PMIN,NP)
            IF(LPMAX1.NE.0)CALL UCOPY(Q(LPMAX1),PMAX,NP)
         ENDIF
         LERR1=0
         LERR2=0
         IF(NPAR.GE.11)CALL KUGETV(VERRORNUM,LERR1,LERR2)
         CALL KUALFA
*
         IF(JAD.NE.0) THEN
            IF(IOPTZ.NE.0)THEN
               CALL HFITV(N,NDIM,NVAR,Q(LX1),Q(LY1),Q(LEY1),PAWFITZ,
     +         CHOPT,NP,PAR,STEP,PMIN,PMAX,SIGPAR,CHI2)
               GO TO 999
            ELSE
               CALL HFITV(N,NDIM,NVAR,Q(LX1),Q(LY1),Q(LEY1),PAWFUN,
     +         CHOPT,NP,PAR,STEP,PMIN,PMAX,SIGPAR,CHI2)
            ENDIF
         ELSE
            IF(NOPER.EQ.1.AND.NPP.LT.NP)THEN
               IF(IFTYPE(1).EQ.1)THEN
                  SX=0.
                  SX2=0.
                  ALLCHA=0.
                  DO 20 I=1,N
                     X = Q(LX1+I-1)
                     Y = Q(LY1+I-1)
                     W = ABS(Y)
                     SX = SX+W*X
                     SX2= SX2+W*X**2
                     ALLCHA=ALLCHA+Y
   20             CONTINUE
                  PAR(2)=SX/ALLCHA
                  PAR(3)=SQRT(ABS(SX2/ALLCHA -PAR(2)*PAR(2)))
                  BINWID=(Q(LX1+N-1)-Q(LX1))/FLOAT(N)
                  PAR(1)=BINWID*ALLCHA/(SQRT2P*PAR(3))
               ELSEIF(IFTYPE(1).EQ.2)THEN
                  CALL PALLSQ(-N,Q(LX1),Q(LY1),PAR(1),PAR(2),IFAIL)
                  IF (IFAIL.NE.0) GO TO 999
               ELSEIF(IFTYPE(1).EQ.3)THEN
                  IF(N.LE.1.OR.NP.EQ.1)THEN
                     ALLCHA=0.
                     DO 30 I=1,N
                        Y = Q(LY1+I-1)
                        ALLCHA=ALLCHA+Y
   30                CONTINUE
                     PAR(1)=ALLCHA/FLOAT(N)
                  ELSE
                     CALL PALSQ(N,Q(LX1),Q(LY1),NP,PAR)
                  ENDIF
               ENDIF
            ENDIF
            CALL HFITV(N,NDIM,NVAR,Q(LX1),Q(LY1),Q(LEY1),PAWSIM, 
     +                 CHOPT,NP,PAR,STEP,PMIN,PMAX,SIGPAR,CHI2)
         ENDIF
#if defined(CERNLIB_MLP)
      ENDIF
#endif
*
      DO 40 I=1,NP
         IF(LPAR1.NE.0)Q(LPAR1+I-1)=PAR(I)
         IF(NPAR.GE.11.AND.LERR2-LERR1+1.GE.NP)THEN
            Q(LERR1+I-1)=SIGPAR(I)
         ENDIF
   40 CONTINUE
*
*            Is plot required ?
*
#if defined(CERNLIB_FPANELS)
      IF (INDEX(CHP,'M').NE.0.AND.IWK.EQ.999) LODRAW = .FALSE.
#endif
      IF (LODRAW) THEN
#if defined(CERNLIB_IBM)
         IF(IWK.GT.10)THEN
            CALL KUPROC('Type CR to continue',CHTEMP,NCH)
         ENDIF
#endif
* Inquire if LOG scale are set
         CALL PAHLOG(LOGX,LOGY,LOGZ)
* IOPTS = 1 : Option Same is on (draw on the same plot)
         IOPTS=INDEX(CHOPT,'S')
         IF(IOPTS.EQ.0)THEN
            CHOPT=CHP
            YMIN=VMIN(Q(LY1),N)
            YMAX=VMAX(Q(LY1),N)
            DY=0.05*(YMAX-YMIN)
            IF(DY.EQ.0.)DY=0.05*YMAX
            IF(DY.EQ.0.)DY=1.
            XMIN=VMIN(Q(LX1),N)
            XMAX=VMAX(Q(LX1),N)
            DX=0.05*(XMAX-XMIN)
            IF(DX.EQ.0.)DX=0.05*XMAX
            IF(DX.EQ.0.)DX=1.
            X1=XMIN-DX
            X2=XMAX+DX
            Y1=YMIN-DY
            Y2=YMAX+DY
            IF(X1.LT.0..AND.XMIN.GE.0.)X1=0.
            IF(X2.GT.0..AND.XMAX.LE.0.)X2=0.
            IF(Y1.LT.0..AND.YMIN.GE.0.)Y1=0.
            IF(Y2.GT.0..AND.YMAX.LE.0.)Y2=0.
            IF(LOGX.NE.0)THEN
               X1=0.5*XMIN
               X2=2.*XMAX
            ENDIF
            IF(LOGY.NE.0)THEN
               Y1=0.5*YMIN
               Y2=2.*YMAX
            ENDIF
            CALL HPLFRA(X1,X2,Y1,Y2,' ')
            FPXMIN = X1
            FPXMAX = X2
            FPYMIN = Y1
            FPYMAX = Y2
         ELSE
            CHOPT=CHP
* Option 'S' is not valid after
            CHOPT(IOPTS:IOPTS)='?'
         ENDIF
*
*                Draw the vector
*
* If LOGY is ON CHOPT for IGRAPH should contain 'GY'
* If LOGX is ON CHOPT for IGRAPH should contain 'GX'
*
         IF (LOGX.NE.0.OR.LOGY.NE.0)CHOPT(32:32) = 'G'
         IF (LOGX.NE.0)CHOPT(31:31) = 'X'
         IF (LOGY.NE.0)CHOPT(30:30) = 'Y'
* Option 'B' is not valid after
         IF (IOPTB.NE.0)CHOPT(IOPTB:IOPTB) = ' '
*
         CALL IGRAPH(N,Q(LX1),Q(LY1),CHOPT)
*
*                Draw fitted function
*
         X1 = Q(LX1)
         XN = Q(LX1+N-1)
         DXF = (XN-X1)/FLOAT(NPFUNC)
         DO 50 I=1,NPFUNC
            XF = X1+(I-1)*DXF
            PAWBUF(1000+I) = XF
#if defined(CERNLIB_MLP)
	    IF(IOPTNN .EQ. 1) THEN
	          PAWBUF(I) = PAWMLP1(XF)
            ELSE
#endif
               IF(JAD.NE.0)THEN
                  PAWBUF(I) = PAWFUN(XF)
               ELSE
                  PAWBUF(I) = PAWSIM(XF)
               ENDIF
#if defined(CERNLIB_MLP)
	    ENDIF   
#endif
   50    CONTINUE
         CHOPT(1:29) = ' '
         CHOPT(1:1) = 'C'
         CALL IGRAPH(NPFUNC,PAWBUF(1001),PAWBUF,CHOPT)
*
*                Draw fit parameters
*
         LCIDS = LCID
         LCID = 0
         CALL HPLFIT
         LCID = LCIDS
      ENDIF
*
  999 END
