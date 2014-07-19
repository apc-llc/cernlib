*
* $Id: hspfun.F,v 1.1.1.1 1996/01/16 17:07:48 mclareni Exp $
*
* $Log: hspfun.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:48  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :
      FUNCTION HSPFUN(IDD,X,IN,IK)
*.==========>
*.           RETURN THE INTERPOLATED VALUE AT ABSCISSA X OBTAINED
*.           WITH SPLINES
*..=========> ( D.Leborgne )
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
      COMMON/HCGARB /IDIMEN,IKNOTS,ICOEIN,ISPLIN,ISPLI1,ISPLI2,ICALCU,
     +        ICOMPT,NCHAN ,BWID  ,XMIN  ,XMAX  ,XMIN1 ,XMAX1 ,NARG  ,
     +        DIST  ,SPLINE
*.___________________________________________
      HSPFUN=0.
*
      NARG=4
      CALL NOARG(NARG)
      IF(NARG.LT.4)THEN
         CALL HBUG('Wrong number of parameters','HSPFUN',IDD)
         GO TO 999
      ENDIF
*
      K=IK
      IF(K.LE.1)K=3
      N=IN
      IF(N.LE.0)N=13
*
      CALL HFIND(IDD,'HSPFUN')
      IF(LCID.EQ.0)GO TO 999
      CALL HDCOFL
      IF(I1.EQ.0.AND.LFIX.EQ.0)THEN
         CALL HBUG('Not a 1-DIM histogram','HSPFUN',IDD)
         GO TO 999
      ENDIF
*
      NCHAN=IQ(LCID+KNCX)
      IDIMEN=NCHAN+2*N+4*K+3
      CALL HWORK(IDIMEN,IKNOTS,'HSPFUN')
      IF(IKNOTS.EQ.0)GO TO 999
*
      ICOEIN=IKNOTS+N+2*K+1
      ISPLIN=ICOEIN+N+K
      ISPLI1=ISPLIN+NCHAN
      ISPLI2=ISPLI1+1
      ICALCU=ISPLI2+1
*
      BWID=(Q(LCID+KXMAX)-Q(LCID+KXMIN))/FLOAT(IQ(LCID+KNCX))
      XMIN=Q(LCID+KXMIN)+0.5*BWID
      XMAX=Q(LCID+KXMAX)-0.5*BWID
*
*             CALCULATION OF THE POINTS XMIN1 AND XMAX1 .
*             WE HAVE THE FUNCTION , WHICH REPRESENTS THE ORIGINAL
*             HISTOGRAM IDD , EQUALS ZERO WHEN X.LT.XMIN1.OR.X.GT.XMAX1
*
      DO 40 I=1,NCHAN
         IF(HCX(I,1).EQ.0.)GO TO 40
         XMIN1=XMIN+BWID*FLOAT(I-1)
         GO TO 50
   40 CONTINUE
*
   50 CONTINUE
*
      DO 60 I=1,NCHAN
         J=NCHAN-I+1
         IF(HCX(J,1).EQ.0.)GO TO 60
         XMAX1=XMAX-BWID*FLOAT(I-1)
         GO TO 70
   60 CONTINUE
*
   70 M=N+2*K+1
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
*             WE CALCULATE NOW THE POSITIONS OF THE KNOTS .
*
      CALL VFILL(Q(IKNOTS),KP1,XMIN1)
      CALL VFILL(Q(ICOEIN-KP1),KP1,XMAX1)
*
      DO 90 I=KP2,MMKP1
   90 Q(IKNOTS+I-1)=XMIN1+DIST*FLOAT(I-KP1)
*
*             LET'S CALCULATE NOW THE COEFFICIENTS OF THE INTEGRAL
*             OF THE SPLINE FUNCTION S(X) .
*
*             WE USE ICOMPT TO DIFFERENCIATE THE CALCULATION OF THE
*             FIRST SPLINE (WE NEED THE VALUES OF THIS AT THE POINTS
*             SITUATED AT THE CENTRES OF THE CHANNELS OF IDD , TO
*             HAVE THE COEFFICIENTS OF THE FIRST DERIVATIVE OF S(X)
*             AND TO CALCULATE THE VALUE OF THE SECOND SPLINE IN X) ,
*             AND THE CALCULATION OF THE SPLINE ON THE DIFFERENCE
*             BETWEEN IDD AND THE FIRST SPLINE .
*
   80 ICOMPT=ICOMPT+1
*
      DO 110 I=1,NPK
         XI=VSUM(Q(IKNOTS+I),K)/FLOAT(K)
         E1=0.
         DO 100 L=1,NCHAN
            XL=XMIN+BWID*FLOAT(L-1)
            IF(XI.LE.XL)GO TO 105
            Z=HCX(L,1)
            IF(ICOMPT.NE.1)THEN
               Z=Z-Q(ISPLIN+L-1)
            ENDIF
            E1=E1+Z*BWID
  100    CONTINUE
*
  105    Q(ICOEIN+I-1)=E1
  110 CONTINUE
*
*              WE CALCULATE NOW THE COEFFICIENTS OF S(X) .
*
      DO 120 I=1,MMKP2
         Q(ICOEIN+I-1)=FLOAT(K)*(Q(ICOEIN+I)-Q(ICOEIN+I-1))
         Q(ICOEIN+I-1)=Q(ICOEIN+I-1)/(Q(IKNOTS+I+K)-Q(IKNOTS+I))
  120 CONTINUE
*
*             LET'S CALCULATE THE VALUES OF THE FIRST SPLINE AT X AND
*             AT ALL THE ABSCISAS WHICH ARE AT THE CENTERS OF THE
*             CHANNELS OF IDD , AND THE VALUE OF THE SECOND SPLINE IN X.
*
      NCHAP1=NCHAN+1
*
      DO 250 I=1,NCHAP1
         SPLINE=0.
         IF(ICOMPT.EQ.2.AND.I.NE.NCHAP1)GO TO 250
         IF(I.NE.NCHAP1)THEN
            XI=XMIN+BWID*FLOAT(I-1)
         ELSE
            XI=X
         ENDIF
*
*              FIND INTERVAL WHERE , XN(KK-1).LE.XI.AND.XI.LT.XN(KK)
*
         IF(XI.LT.Q(IKNOTS+K))GO TO 220
         IF(XI.GT.Q(ICOEIN-1))GO TO 220
*
         DO 160 L=KP1,MMKP1
            KK=L+1
            IF(Q(IKNOTS+L-1).LE.XI.AND.XI.LT.Q(IKNOTS+L))GO TO 170
  160    CONTINUE
*
  170    KKM1=KK-1
         KKM2=KK-2
         E1=XI-Q(IKNOTS+KKM2)
         E2=Q(IKNOTS+KKM1)-XI
         Q(ICALCU)=1./(Q(IKNOTS+KKM1)-Q(IKNOTS+KKM2))
*
         DO 180 J=2,K
            Q(ICALCU+J-1)=E1*Q(ICALCU+J-2)
            Q(ICALCU+J-1)=Q(ICALCU+J-1)/(Q(IKNOTS+KKM2+J)-Q(IKNOTS+
     +      KKM2))
  180    CONTINUE
*
         DO 200 J=1,KM1
            E3=XI-Q(IKNOTS+KKM2-J)
            Q(ICALCU)=E2*Q(ICALCU)/(Q(IKNOTS+KKM1)-Q(IKNOTS+KKM2-J))
            KMJ=K-J
            IF(KMJ.LT.2)GO TO 200
            DO 190 L=2,KMJ
               A1=E3*Q(ICALCU+L-2)
               A2=(Q(IKNOTS+KKM2+L)-XI)*Q(ICALCU+L-1)
               Q(ICALCU+L-1)=(A1+A2)/(Q(IKNOTS+KKM2+L)-Q(IKNOTS+KKM2-J)
     +         )
  190       CONTINUE
  200    CONTINUE
*
         KKMKM2=KK-K-2
*
         DO 210 J=1,K
            II=KKM1+J
            L=KKMKM2+J
            E1=(Q(IKNOTS+II-1)-Q(IKNOTS+II-KP1))*Q(ICALCU+J-1)*
     +      Q(ICOEIN+L-1)
            SPLINE=SPLINE+E1
  210    CONTINUE
*
  220    IF(ICOMPT.EQ.2)GO TO 240
         IF(I.EQ.NCHAP1)GO TO 230
         Q(ISPLIN+I-1)=SPLINE
         GO TO 250
*
  230    Q(ISPLI1)=SPLINE
         GO TO 250
*
  240    Q(ISPLI2)=SPLINE
*
  250 CONTINUE
*
*             WE HAVE FINISHED , LET'S GIVE NOW THE RESULT .
*
      IF(ICOMPT.EQ.2)GO TO 260
      GO TO 80
*
  260 HSPFUN=Q(ISPLI1)+Q(ISPLI2)
*
  999 RETURN
      END
