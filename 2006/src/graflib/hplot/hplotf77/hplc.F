*
* $Id: hplc.F,v 1.2 1998/07/08 13:18:03 couet Exp $
*
* $Log: hplc.F,v $
* Revision 1.2  1998/07/08 13:18:03  couet
* - in case of 2D hsitograms (prox, proy etc ..) the minimum and maximum should
* be takne in KMIN2 and KMAX2 adresses.
*
* Revision 1.1.1.1  1996/01/19 10:49:58  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.18/05 15/12/93  19.20.55  by  O.Couet
*-- Author :
      SUBROUTINE HPLC(IERROR)
*.==========>
*.        fills /HPL9/ with HBOOK information
*.        determines YMINI and YMAXI , smallest
*.        and largest ordinates in histogram
*.        coordinates
*.        used for histogram 1D only
*..=========>
#include "hbook/hcbits.inc"
#include "hbook/hcprin.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl9.inc"
#include "hplot/hpl13.inc"
*._____________________________
      FACTOR = 1.0
      IERROR = 0
      NOLD   = 4
      I34    = NOENT
*----
*     I11=1 means error bars are to be calculated
*----
      IF(I11.EQ.1)I34=1
*----
*     abcissa limits : XMINI,XMAXI
*     XX0, XXSIZE for equidistant bins
*     ICMAX=no. of channels of the whole histogram (no zoom)
*----
      ICMAX=NCX
      IFIRST=1
      IF(LOZOOM)IFIRST=NCMIN
      ILAST=ICMAX
      IF(LOZOOM)ILAST=NCMAX
      IF(I6.EQ.0)THEN
         XMINI=XX0+(IFIRST-1)*XXSIZE
         XMAXI=XX0+ILAST*XXSIZE
      ELSE
*----
*             Non_equidistant bins
*----
         LBINS=LQ(LCID-2)
         XMINI=Q(LBINS+IFIRST)
         XMAXI=Q(LBINS+ILAST+1)
      ENDIF
*----
*     for log scales, histogram coordinates are :
*     LOG10(XMINI) and LOG10(XMAXI)
*----
      IF(LOGXFL)THEN
         IF(XMINI.LE.0)GOTO 60
         XMINI=LOG10(XMINI)
         IF(XMAXI.LE.0)GOTO 60
         XMAXI=LOG10(XMAXI)
      ENDIF
*----
*     ordinates limits : YMINI,YMAXI
*----
      YMAXI=-BIGP
      YMINI=BIGP
      ALLCHA=0.
      DO 10  I=IFIRST,ILAST
         C1=HCX(I,1)
         YMAXI=MAX(YMAXI,C1)
*----
*     for log scales, YMINI is the smallest positive value. With
*     option SAME, the previous minimum (YWMINI) is used.
*----
         IF(LOGYFL)THEN
            IF(C1.GT.0.)THEN
               YMINI=MIN(YMINI,C1)
            ELSE
               IF(LOSAME)YMINI=10**YWMINI
            ENDIF
         ELSE
            YMINI=MIN(YMINI,C1)
         ENDIF
         ALLCHA=ALLCHA+C1
   10 CONTINUE
*----
*     take into account HMAXIM,HMINIM,HCOMPA
*----
      XMIN=YMINI
      IF(I20.EQ.1)THEN
         IF(I1.NE.0)THEN
            YMAXI=Q(LCID+KMAX1)
         ELSE
            YMAXI=Q(LCID+KMAX2)
         ENDIF
      ENDIF
      IF(I21.EQ.1)THEN
         IF(I1.NE.0)THEN
            XMIN =Q(LCID+KMIN1)
         ELSE
            XMIN =Q(LCID+KMIN2)
         ENDIF
      ENDIF
      IF(LOGYFL.AND.XMIN.LE.0.)THEN
         CALL HBUG('Log scale requested with zero or negative argument:'
     +   //' HMINIM (HCOMPA) ignored' ,'HPLOT ',ID)
         GOTO 20
      ENDIF
      YMINI=XMIN
   20 CONTINUE
      IF((YMINI.GE.YMAXI).AND.(IOPTP.EQ.0))THEN
         IF(LOGYFL)THEN
            IF(YMAXI.GT.0.)THEN
               YMINI=0.001*YMAXI
            ELSE
               GOTO 50
            ENDIF
         ELSE
            YMAXI=YMINI+1.
            YMINI=YMINI-1.
         ENDIF
      ENDIF
*----
*     take into account HNORMA
*     FACTOR=scaling factor for normalisation (HPLOT and HBOOK)
*----
      FACTOR=ALLCHA
      IF(I18.EQ.1)FACTOR=Q(LCID+KNORM)
      IF(ALLCHA.NE.0.)FACTOR=FACTOR/ALLCHA
      IF(FACTOR.EQ.0.)FACTOR=1.
      YMAXI=YMAXI*FACTOR
      YMINI=YMINI*FACTOR
      C1=YMAXI
      IF(ABS(YMINI).GT.ABS(C1))C1=YMINI
*
      C2=YMAXI
      IF(I20.EQ.0)THEN
         IF(I12.NE.0)THEN
            C2=HCX(IFIRST,3)
            DO 30  I=IFIRST+1,ILAST
               F1=HCX(I,3)*FACTOR
               C2=MAX(C2,F1)
   30       CONTINUE
         ENDIF
         IF(I34.NE.0)THEN
            IF(I11.EQ.0)THEN
               DO 40  I=IFIRST,ILAST
                  E1=(HCX(I,1) + HCX(I,2))*FACTOR
                  YMAXI=MAX(E1,YMAXI)
   40          CONTINUE
            ELSE
               YMAXI=YMAXI+SQRT(YMAXI)
            ENDIF
         ENDIF
         YMAXI=MAX(C2,YMAXI)
      ENDIF
*----
*     For log scales, histogram coordinates are LOG10(YMINI) and
*     LOG10(YMAXI). Final adjustment (if not option "Same" or "Update"
*     or "+" for YMAXI) of YMAXI and YMINI for logarithmic scale, if
*     HMAXIM and HMINIM have not been called.
*----
      IF(LOGYFL)THEN
         IF(YMINI.LE.0)GOTO 60
         YMINI=LOG10(YMINI)
         IF(I21.EQ.0) YMINI=YMINI+LOG10(0.5)
         IF(YMAXI.LE.0)GOTO 60
         YMAXI=LOG10(YMAXI)
         HSCAL=SCMAX
         IF((IOPTK.NE.0).AND.(.NOT.LOMAX))HSCAL=3.*SCMAX
         IF((I20.EQ.0).AND.(IOPTU.EQ.0).AND.(IOPTP.EQ.0)) YMAXI=YMAXI+
     +   LOG10(2.*(0.9/HSCAL))
         GOTO 999
      ENDIF
*----
*     final adjustment of YMAXI for linear scale (if not option "Same"
*     or "Update"):
*     decrease histogram height to MAX% of allowed height if HMAXIM
*     has not been called.
*     MAX% is the value in percent which has been set in HPLSET
*     (default is 90%).
*----
      IF((I20.EQ.0.).AND.(IOPTU.EQ.0).AND.(IOPTP.EQ.0))THEN
         HSCAL=SCMAX
         IF((IOPTK.NE.0).AND.(.NOT.LOMAX))HSCAL=SCMAX/3.
         IF(YMAXI.GT.0.)THEN
            YMAXI=YMAXI*(1./HSCAL)
         ELSE
            YMAXI=YMAXI*HSCAL
         ENDIF
      ENDIF
*-----
*     final adjustment of YMINI for linear scale.
*     if HMINIM has not been called set YMINI to zero if >0
*     or to YMINI*SCMAX if <0.
*-----
      IF(I21.EQ.0.)THEN
         IF(YMINI.GE.0.)THEN
            YMINI=0.
         ELSE
            YMINI=YMINI*(1./SCMAX)
         ENDIF
      ENDIF
      GOTO 999
*
   50 CALL HBUG('Log scale is requested but maximum is less or equal 0'
     +,'HPLOT ',ID)
      IERROR = 1
      GOTO 999
*
   60 CALL HBUG('Routine called with zero or negative argument with log
     +scale','HPLOT ',ID)
*
  999 END
