*
* $Id: hpr2.F,v 1.1.1.1 1996/01/16 17:07:46 mclareni Exp $
*
* $Log: hpr2.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:46  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  09.50.31  by  Julian Bunn
*-- Author :
      SUBROUTINE HPR2(C,A,W)
*.==========>
*.           CONTROL ROUTINE TO PRINT A 2-DIM HISTOGRAM
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcprin.inc"
      COMMON/HEXPO/ISI,IE1,IE2,NBS,FACT
      DIMENSION C(3),A(3),W(1)
      INTEGER CHAR,A,W
*.___________________________________________
      NCX=IQ(LCID+KNCX)
      NCY=IQ(LCID+KNCY)
      X0=Q(LCID+KXMIN)
      Y0=Q(LCID+KYMIN)
      XSIZE=(Q(LCID+KXMAX)-Q(LCID+KXMIN))/FLOAT(IQ(LCID+KNCX))
      YSIZE=(Q(LCID+KYMAX)-Q(LCID+KYMIN))/FLOAT(IQ(LCID+KNCY))
      NH=NH+1
      NBSCAT=IQ(LSCAT+KNBIT)
      NBS=NBSCAT
      IF(IQ(LSCAT+KNOENT).EQ.0)THEN
         NOENT=2
         CALL HPTIT(1,0,0.,0.)
         GO TO 999
      ENDIF
      NOENT=1
      NOLD=4
      FACTOR=1.
      IF(I19.NE.0)FACTOR=Q(LCID+KSCAL2)
      IF(I3.EQ.0)THEN
         NHT=NCOLPA-28
         NHT=(NCX-2+NHT)/NHT
         ICHAN=NCOLPA-29
         MSTEP=1
      ELSE
         IF(NBSCAT.GE.32)THEN
            ICMIN=10
         ELSE
            Y=MAXBIT(NBSCAT)
            ICMIN=LOG10(Y)+2
         ENDIF
         MSTEP=ICMIN
         NHT=(NCOLPA-25-3*ICMIN)/ICMIN
         ICHAN=NHT-1
         NHT=(NCX-2+NHT)/NHT
      ENDIF
      ICMAX=0
      XLOW=X0-XSIZE
*
      XMINI=BIGP
      XMAXI=-XMINI
      IF(I2.EQ.0)THEN
         IF(NBSCAT.GE.32)THEN
            DO 43 ICY=1,NCY
               DO 42 ICX=1,NCX
                  C(1)=HCXY(ICX,ICY,1)
                  XMAXI=MAX(XMAXI,C(1))
                  XMINI=MIN(XMINI,C(1))
  42           CONTINUE
  43        CONTINUE
         ENDIF
      ENDIF
      XM1=XMAXI
      XM2=XMINI
      C(1)=XM1
      C(2)=XM2
      FACT8=1.
      W5=0.
      IF(I3.NE.0.AND.NBSCAT.GE.32)CALL HFACT(C,2,A(1),A(2),A(3),FACT8)
*
      DO 90 IH=1,NHT
         Y=Y0+FLOAT(NCY+1)*YSIZE
         ICMIN=ICMAX+1
         CHAR=IDG(41)
         IF(IH.EQ.1)CHAR=IDG(2)
         IF(IH.EQ.NHT)CHAR=IDG(3)
         IF(NHT.EQ.1)CHAR=IDG(4)
         IBI=0
         IF(IH.EQ.  1)IBI=IBI+1
         IF(IH.EQ.NHT)IBI=IBI+1
         ICMAX=ICMIN+ICHAN+IBI
         IF(ICMAX.GT.NCX+2)ICMAX=NCX+2
*
*             PRINTING OF TITLE AND CHANNEL NUMBER
*
         CALL HPTIT(1,1,0,0)
*
         ICN=ICMAX-ICMIN+1
         IBI=ICMIN
         IF(IH.GT.1)IBI=IBI-1
         IF(I3.EQ.0)THEN
            ICT=ICN
            IF(IH.EQ.  1)ICT=ICT-1
            IF(IH.EQ.NHT)ICT=ICT-1
            CALL HPCHAN(CHAR,IBI,ICMAX,ICT,A)
         ELSE
            NW=(ICMAX-ICMIN+1)*MSTEP
            IFW=ICMIN+NW-1
            ICT=NW
            IF(IH.EQ.  1)ICT=ICT-MSTEP
            IF(IH.EQ.NHT)ICT=ICT-MSTEP
            CALL HPCHAN(CHAR,IBI,IFW,ICT,A)
         ENDIF
*
*             AUTOMATIC SCALING IF FACTOR=0.
*             SEMI-AUTOMATIC SCALING IF FACTOR.NE.0.
*
         XMINI=0.
         STEP=FACTOR
         IF(I19.EQ.0)GO TO 49
         IF(FACTOR.NE.0.)GO TO 49
         XMINI=BIGP
         XMAXI=-XMINI
*
         DO 48 ICY=1,NCY
            DO 47 ICX=ICMIN,ICMAX
               C(1)=HCXY(ICX-1,ICY,1)
               XMINI=MIN(XMINI,C(1))
               XMAXI=MAX(XMAXI,C(1))
  47        CONTINUE
  48     CONTINUE
         IF(XMAXI.EQ.0.)XMAXI=1.
         IF(XMAXI.EQ.XMINI)XMAXI=XMAXI+1.
*
         CALL HBIN(XMINI,XMAXI,37,XMINI,XMAXI,C(1),STEP)
         IF(I24.NE.0)STEP=MAX(STEP,1.)
         FACTOR=1./STEP
         XMINI=-FACTOR*XMINI
*
  49     CONTINUE
*
*             DETERMINATION OF LINE CONTENTS
*
         C(1)=Y
         C(2)=Y-YSIZE*FLOAT(NCY)
         CALL HFACT(C,2,ISI,IE1,IE2,FACT)
         Y2=Y*FACT
         YSI2=YSIZE*FACT
*
*
         DO 70 ICYY=NCY+1,0,-1
            ICY=ICYY
            Y2=Y2-YSI2
            J=0
            DO 50 ICX=ICMIN,ICMAX
               J=J+1
               C(J)=HCXY(ICX-1,ICY,1)*FACT8
  50        CONTINUE
            W5=W5+VSUM(C,J)
*
            IF(ICYY.EQ.NCY+1)ICY=IDG(25)
            IF(ICYY.EQ.0    )ICY=IDG(31)
*
            IF(I3.EQ.0)THEN
               CALL HPSCA(C,ICN,ICY,Y2,FACTOR,XMINI)
            ELSE
               CALL HPTA(C,ICN,ICY,Y2)
            ENDIF
*
  70     CONTINUE
*
*             DEFINITION AND PRINTING OF LOW-EDGE
*
         C(1)=XLOW+XSIZE
         DO 75 ICX=2,ICN
            C(ICX)=C(ICX-1)+XSIZE
            XLOW=C(ICX)
  75     CONTINUE
         IF(IH.EQ.1)XLOW=XLOW-XSIZE
*
         CALL HFACT(C,ICN,ISIGNE,IEXP1,IEXP2,FACT)
         XMAXI=VMAX(C,ICN)
         XMAXI=ABS(XMAXI)
         IF(ABS(C(1)).GT.XMAXI)XMAXI=C(1)
         IF(I3.EQ.0)THEN
            ICT=ICN
            IF(IH.EQ.  1)ICT=ICT-1
            IF(IH.EQ.NHT)ICT=ICT-1
            CALL HPCONT('LOW-EDGE',C,ICT,5,XMAXI,A,2,W,ISIGNE,
     +                  IEXP1,IEXP2)
         ELSE
            ICN=ICN*MSTEP
            ICT=ICN
            IF(IH.EQ.  1)ICT=ICT-MSTEP
            IF(IH.EQ.NHT)ICT=ICT-MSTEP
            CALL HPCONT('LOW-EDGE',C,ICT,5,XMAXI,A,2*MSTEP,W,ISIGNE,
     +                 IEXP1,IEXP2)
         ENDIF
  90  CONTINUE
*
*             PRINTING OF PLOT STATISTICS
*
      IF(I25.EQ.0)THEN
         IF(I2.NE.0)THEN
            CALL HFRAME(IQ(LSCAT+KNOENT),-XMINI/FACTOR,STEP,W5)
         ELSE
            C(1)=XM1
            C(2)=XM2
            CALL HFACT(C,2,ISI,IE1,IE2,FACT)
            CALL HFRAME(IQ(LSCAT+KNOENT),0.,0.,W5)
         ENDIF
      ENDIF
 999  RETURN
      END
