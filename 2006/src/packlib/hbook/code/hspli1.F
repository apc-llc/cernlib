*
* $Id: hspli1.F,v 1.1.1.1 1996/01/16 17:07:48 mclareni Exp $
*
* $Log: hspli1.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:48  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :
      SUBROUTINE HSPLI1(IDD,ISUPIM,IN,IK,CHISQ)
*.==========>
*.           THIS ROUTINE SMOOTHS THE 1-DIM HISTOGRAM IDD BY THE
*.           METHOD OF NORMALIZED B-SPLINES .
*..=========> ( R.Brun ,D.Leborgne)
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcprin.inc"
      COMMON/HCGARB /IDIMEN,IKNOTS,ICOEIN,ISPLI1,ICALCU,ICOMPT,NCHAN ,
     +        BWID  ,XMIN  ,XMAX  ,XMIN1 ,XMAX1 ,IFUNC ,NARG  ,SPLINE,
     +        ICASE ,DIST
*.___________________________________________
      NARG=5
      CALL NOARG(NARG)
*
      CALL HFIND(IDD,'HSPLI1')
      IF(LCID.EQ.0)GO TO 999
      CALL HDCOFL
      IF(I1.EQ.0)THEN
         CALL HBUG('Not a 1-DIM histogram','HSPLI1',IDD)
         GO TO 999
      ENDIF
*
      ICASE=1
      IF(NARG.NE.1)ICASE=ISUPIM
      IF(ICASE.LE.0)ICASE=2
      NV=1
      IF(ICASE.EQ.2)CALL HFUNC(IDD,SQRT)
      NV=2
*
      K=3
      IF(NARG.GE.4)K=IK
      IF(K.LE.1)K=3
*
      N=13
      IF(NARG.GE.3)N=IN
      IF(N.LE.0)N=13
*
      NCHAN=IQ(LCID+KNCX)
      IDIMEN=NCHAN+2*N+4*K
      CALL HWORK(IDIMEN,IKNOTS,'HSPLI1')
      IF(IKNOTS.EQ.0)GO TO 999
*
      ICOEIN=IKNOTS+N+2*K+1
      ISPLI1=ICOEIN+N+K
      ICALCU=ISPLI1+NCHAN
*
      BWID=(Q(LCID+KXMAX)-Q(LCID+KXMIN))/FLOAT(IQ(LCID+KNCX))
      XMIN=Q(LCID+KXMIN)+0.5*BWID
      XMAX=Q(LCID+KXMAX)-0.5*BWID
*
*             CALCULATION OF THE POINTS XMIN1 AND XMAX1 .
*             WE HAVE THE FUNCTION , WHICH REPRESENTS THE ORIGINAL
*             HISTOGRAM IDD , ZERO WHEN X.LT.XMIN1 OR X.GT.XMAX1 .
*
      DO 20 I=1,NCHAN
         IF(HCX(I,1).EQ.0.)GO TO 20
         XMIN1=XMIN+FLOAT(I-1)*BWID
         GO TO 30
   20 CONTINUE
*
   30 CONTINUE
*
      DO 40 I=1,NCHAN
         J=NCHAN-I+1
         IF(HCX(J,1).EQ.0.)GO TO 40
         XMAX1=XMAX-FLOAT(I-1)*BWID
         GO TO 50
   40 CONTINUE
*
   50 M=N+2*K+1
      NPK=N+K
      KP1=K+1
      KP2=K+2
      KM1=K-1
      MMKP1=M-KP1
      MMKP2=M-KP2
      ICOMPT=0
*
      DIST=(XMAX1-XMIN1)/FLOAT(M-2*K-1)
*
*             WE CALCULATE THE POSITIONS OF THE KNOTS .
*
      CALL VFILL(Q(IKNOTS),KP1,XMIN1)
      CALL VFILL(Q(ICOEIN-KP1),KP1,XMAX1)
*
      DO 70 I=KP2,MMKP1
   70 Q(IKNOTS+I-1)=XMIN1+DIST*FLOAT(I-KP1)
*
*             LET US CALCULATE NOW THE COEFFICIENTS OF THE INTEGRAL
*             OF THE SPLINE FUNCTION S(X) .
*
*             WE HAVE ICOMPT TO DIFFERENCIATE THE CALCULATION OF THE
*             FIRST SPLINE AND THE SPLINE ON THE DIFFERENCE BETWEEN
*             THE ORIGINAL HISTOGRAM IDD AND THE FIRST SPLINE .
*
   60 ICOMPT=ICOMPT+1
*
      DO 90 I=1,NPK
         XI=VSUM(Q(IKNOTS+I),K)/FLOAT(K)
         E1=0.
         DO 80 L=1,NCHAN
            X=XMIN+BWID*FLOAT(L-1)
            IF(XI.LE.X)GO TO 85
            Z=HCX(L,1)
            IF(ICOMPT.NE.1)THEN
               Z=Z-Q(ISPLI1+L-1)
            ENDIF
            E1=E1+Z*BWID
   80    CONTINUE
   85    Q(ICOEIN+I-1)=E1
   90 CONTINUE
*
*             WE CALCULATE NOW THE COEFFICIENTS OF THE SPLINE S(X) .
*
      DO 100 I=1,MMKP2
         Q(ICOEIN+I-1)=FLOAT(K)*(Q(ICOEIN+I)-Q(ICOEIN+I-1))
         Q(ICOEIN+I-1)=Q(ICOEIN+I-1)/(Q(IKNOTS+I+K)-Q(IKNOTS+I))
  100 CONTINUE
*
*             WE CALCULATE NOW THE VALUES OF THE SPLINE FUNCTION AT
*             THE POINTS SITUATED AT THE CENTRE OF THE CHANNELS OF IDD .
*
      DO 190 I=1,NCHAN
         SPLINE=0.
         X=XMIN+BWID*FLOAT(I-1)
*
*             FIND INTERVAL WHERE , XN(KK-1).LE.X.AND.X.LT.XN(KK)
*
         IF(X.LT.Q(IKNOTS+K))GO TO 170
         IF(X.GT.Q(ICOEIN-1))GO TO 170
*
         DO 110 L=KP1,MMKP1
            KK=L+1
            IF(Q(IKNOTS+L-1).LE.X.AND.X.LT.Q(IKNOTS+L))GO TO 120
  110    CONTINUE
*
  120    KKM1=KK-1
         KKM2=KK-2
         E1=X-Q(IKNOTS+KKM2)
         E2=Q(IKNOTS+KKM1)-X
         Q(ICALCU)=1./(Q(IKNOTS+KKM1)-Q(IKNOTS+KKM2))
*
         DO 130 J=2,K
            Q(ICALCU+J-1)=E1*Q(ICALCU+J-2)
            Q(ICALCU+J-1)=Q(ICALCU+J-1)/(Q(IKNOTS+KKM2+J)-Q(IKNOTS+
     +      KKM2))
  130    CONTINUE
*
         DO 150 J=1,KM1
            E3=X-Q(IKNOTS+KKM2-J)
            Q(ICALCU)=E2*(Q(ICALCU))/(Q(IKNOTS+KKM1)-Q(IKNOTS+KKM2-J))
            KMJ=K-J
            IF(KMJ.LT.2)GO TO 150
*
            DO 140 L=2,KMJ
               A1=E3*Q(ICALCU+L-2)
               A2=(Q(IKNOTS+KKM2+L)-X)*Q(ICALCU+L-1)
               Q(ICALCU+L-1)=(A1+A2)/(Q(IKNOTS+KKM2+L)-Q(IKNOTS+KKM2-J))
  140       CONTINUE
  150    CONTINUE
*
         KKMKM2=KK-K-2
*
         DO 160 J=1,K
            II=KKM1+J
            L=KKMKM2+J
            E1=(Q(IKNOTS+II-1)-Q(IKNOTS+II-KP1))*Q(ICALCU+J-1)
     +          *Q(ICOEIN+L-1)
            SPLINE=SPLINE+E1
  160    CONTINUE
*
  170    IF(ICOMPT.EQ.2)GO TO 180
         Q(ISPLI1+I-1)=SPLINE
         GO TO 190
  180    Q(ISPLI1+I-1)=SPLINE+Q(ISPLI1+I-1)
*
  190 CONTINUE
*
      IF(ICOMPT.NE.2)GO TO 60
*
*             WE HAVE FINISHED THE CALCULATION OF THE TWO SPLINES ,
*             AND WE HAVE MADE THE SUM OF THESE . THIS SUM IS THE
*             FINAL RESULT .
*
*             WE HAVE TO PRINT NOW THE RESULT ACCORDING TO THE VALUE
*             OF ISUPIM AND TO LOOK IF WE MUST CALCULATE THE CHISQUARE .
*
  200 CONTINUE
*
      IF(NARG.LT.5)GO TO 230
*
      CHISQ=0.
      DO 220 I=1,NCHAN
         Z=HCX(I,1)
         IF(Z.EQ.0.)GO TO 220
         E1=Z-Q(ISPLI1+I-1)
         CHISQ=CHISQ+(E1*E1)/ABS(Z)
  220 CONTINUE
*
*             LET'S SEE NOW IF WE MUST OR NO SUPERIMPOSE
*             THE SPLINE FUNCTION .
*
  230 IF(ICASE.NE.2)GO TO 240
      LFUNC=LQ(LCONT-1)
      IF(LFUNC.NE.0)CALL UCOPY(Q(ISPLI1),Q(LFUNC+3),IQ(LFUNC-1)-2)
      CALL SBIT0(IQ(LCID),5)
      GO TO 999
*
  240 NOENT=IQ(LCONT+KNOENT)
      CALL HPAK(IDD,Q(ISPLI1))
      IQ(LCONT+KNOENT)=NOENT
*
  999 RETURN
      END
