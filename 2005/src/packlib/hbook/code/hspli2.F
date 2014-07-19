*
* $Id: hspli2.F,v 1.1.1.1 1996/01/16 17:07:48 mclareni Exp $
*
* $Log: hspli2.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:48  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  09.52.17  by  Julian Bunn
*-- Author :
      SUBROUTINE HSPLI2(IDD,INX,INY,IKX,IKY)
*.==========>
*.           THIS ROUTINE GIVES THE RESULT OF THE INTERPOLATION
*.           MADE ON THE 2-DIM HISTOGRAM IDD . WE USE FOR THIS
*.           INTERPOLATION THE ALGORITHM OF B-SPLINES .
*.
*.           PARAMETERS :
*.             - INX AND INY ARE CONNECTED RESPECTIVELY AT THE
*.               NUMBER OF KNOTS WE PLACE ON THE AXIS X AND Y .
*.             - IKX AND IKY ARE RESPECTIVELY THE DEGREES OF THE
*.               SPLINE FUNCTION IN X AND Y .
*.
*.           REMARKS :
*.             - WHEN INX.LE.0 , WE TAKE INX=13 .
*.             - WHEN INY.LE.0 , WE PUT INY AT 13 .
*.             - WHEN IKX.LE.0 , WE TAKE IKX=3 .
*.             - WHEN IKY.LE.0 , WE PUT IKY AT 3 .
*.             - AN ERROR MESSAGE IS GIVEN IF IKX OR IKY ARE PUT
*.               AT 1 BY THE USER .
*..=========> ( R.Brun ,D.Leborgne)
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcprin.inc"
      COMMON/HCGARB /ISPACE,IKNOTX,IKNOTY,ICOEFF,ICALCX,ICALCY,ICOMPT,
     +        NCX   ,NCY   ,BWIDX ,BWIDY ,XMIN  ,XMAX  ,YMIN  ,YMAX  ,
     +        NARG  ,SPLINE
*.___________________________________________
      NARG=5
      CALL NOARG(NARG)
*
      CALL HFIND(IDD,'HSPLI2')
      IF(LCID.EQ.0)GO TO 999
      CALL HDCOFL
      IF(I230.EQ.0)THEN
         CALL HBUG('Not a 2-DIM histogram','HSPLI2',IDD)
         GO TO 999
      ENDIF
*
      KX=3
      KY=3
      NX=13
      NY=13
*
      IF(NARG.LT.5)GO TO 20
      KY=IKY
      IF(KY.LE.1)KY=3
*
   20 IF(NARG.LT.4)GO TO 30
      KX=IKX
      IF(KX.LE.1)KX=3
*
   30 IF(NARG.LT.3)GO TO 40
      NY=INY
      IF(NY.LE.0)NY=13
*
   40 IF(NARG.LT.2)GO TO 50
      NX=INX
      IF(NX.LE.0)NX=13
*
   50 NCX=IQ(LCID+KNCX)
      NCY=IQ(LCID+KNCY)
      BWIDX=(Q(LCID+KXMAX)-Q(LCID+KXMIN))/FLOAT(IQ(LCID+KNCX))
      BWIDY=(Q(LCID+KYMAX)-Q(LCID+KYMIN))/FLOAT(IQ(LCID+KNCY))
      XMIN=Q(LCID+KXMIN)+0.5*BWIDX
      XMAX=Q(LCID+KXMAX)-0.5*BWIDX
      YMIN=Q(LCID+KYMIN)+0.5*BWIDY
      YMAX=Q(LCID+KYMAX)-0.5*BWIDY
*
      ISPACE=NX+NY+3*KX+3*KY+2+(NX+KX)*(NY+KY)
*
      CALL HWORK(ISPACE,IKNOTX,'HSPLI2')
      IF(IKNOTX.EQ.0)GO TO 999
*
      CALL SBIT0(IQ(LCID),5)
      IKNOTY=IKNOTX+NX+2*KX+1
      ICOEFF=IKNOTY+NY+2*KY+1
      ICALCX=ICOEFF+(NX+KX)*(NY+KY)
      ICALCY=ICALCX+KX
*
*             LET US CALCULATE THE POINTS XMIN1,XMAX1,YMIN1,YMAX1 .
*             FROM THESE ABSCISSES , THE VALUES OF THE HISTOGRAM IDD
*             BECOME DIFFERENT OF ZERO .
*
      DO 70 I=1,NCX
         DO 60 J=1,NCY
            IF(HCXY(I,J,1).EQ.0.)GO TO 60
            XMIN1=XMIN+BWIDX*FLOAT(I-1)
            IF(I.EQ.1)XMIN1=XMIN
            GO TO 80
   60    CONTINUE
   70 CONTINUE
*
   80 CONTINUE
      DO 100 I=1,NCX
         II=NCX-I+1
         DO 90 J=1,NCY
            IF(HCXY(II,J,1).EQ.0.)GO TO 90
            XMAX1=XMAX-BWIDX*FLOAT(I-1)
            GO TO 110
   90    CONTINUE
  100 CONTINUE
*
  110 CONTINUE
*
      DO 130 I=1,NCY
         DO 120 J=1,NCX
            IF(HCXY(J,I,1).EQ.0.)GO TO 120
            YMIN1=YMIN+BWIDY*FLOAT(I-1)
            IF(I.EQ.1)YMIN1=YMIN
            GO TO 140
  120    CONTINUE
  130 CONTINUE
*
  140 CONTINUE
*
      DO 160 I=1,NCY
         II=NCY-I+1
         DO 150 J=1,NCX
            IF(HCXY(J,II,1).EQ.0.)GO TO 150
            YMAX1=YMAX-BWIDY*FLOAT(I-1)
            GO TO 170
  150    CONTINUE
  160 CONTINUE
*
  170 MX=NX+2*KX+1
      MY=NY+2*KY+1
      KXP1=KX+1
      KXP2=KX+2
      KXM1=KX-1
      MXMKP1=MX-KXP1
      MXMKP2=MX-KXP2
      NXPK=NX+KX
      KYP1=KY+1
      KYP2=KY+2
      KYM1=KY-1
      MYMKP1=MY-KYP1
      MYMKP2=MY-KYP2
      NYPK=NY+KY
*
      DISTX=(XMAX1-XMIN1)/FLOAT(MX-2*KX-1)
      DISTY=(YMAX1-YMIN1)/FLOAT(MY-2*KY-1)
*
*             LET'S CALCULATE NOW THE POSITIONS OF THE KNOTS ON THE
*             AXIS X AND Y .
*
      CALL VFILL(Q(IKNOTX),KXP1,XMIN1)
      CALL VFILL(Q(IKNOTY-KXP1),KXP1,XMAX1)
      CALL VFILL(Q(IKNOTY),KYP1,YMIN1)
      CALL VFILL(Q(ICOEFF-KYP1),KYP1,YMAX1)
*
      DO 180 I=KXP2,MXMKP1
  180 Q(IKNOTX+I-1)=XMIN1+DISTX*FLOAT(I-KXP1)
*
      DO 190 I=KYP2,MYMKP1
  190 Q(IKNOTY+I-1)=YMIN1+DISTY*FLOAT(I-KYP1)
*
*             LET US CALCULATE NOW THE COEFFICIENTS OF THE SPLINE
*             FUNCTION S(X) . THESE COEFFICIENTS ARE THE SAME THAN
*             THE COEFFICIENTS C(I) OF THE THEORY . BUT HERE WE ARE
*             IN DIMENSION 2 .
*
      ICOMPT=0
*
      DO 280 I=1,NXPK
         XI=VSUM(Q(IKNOTX+I),KX)/FLOAT(KX)
*
         DO 200 JX=1,NCX
            XJ=XMIN+BWIDX*FLOAT(JX-1)
            IF(XI.GT.XJ)GO TO 200
            ILIMX=JX
            GO TO 210
  200    CONTINUE
*
  210    CONTINUE
*
         DO 270 J=1,NYPK
            ICOMPT=ICOMPT+1
            YI=VSUM(Q(IKNOTY+J),KY)/FLOAT(KY)
            IF(J.EQ.1)THEN
               YLIM=YMIN
               ILIM=1
            ENDIF
            DO 230 JY=ILIM,NCY
               YJ=YLIM+BWIDY*FLOAT(JY-ILIM)
               IF(YI.GT.YJ)GO TO 230
               ILIM2=JY
               YLIM=YJ
               GO TO 240
  230       CONTINUE
  240       SUM=0.
*
            DO 250 IJ=1,ILIMX
               DO 250 JJ=ILIM,ILIM2
  250       SUM=SUM+HCXY(IJ,JJ,1)*BWIDX*BWIDY
*
            ILIM=ILIM2
            IF(J.EQ.1)THEN
               Q(ICOEFF+ICOMPT-1)=0.
            ELSE
               Q(ICOEFF+ICOMPT-1)=Q(ICOEFF+ICOMPT-2)+SUM
            ENDIF
  270    CONTINUE
*
  280 CONTINUE
*
*             LET US CALCULATE NOW THE COEFFICIENTS WHICH CORRESPOND
*             AT THE FIRST DERIVATIVE OF THE SPLINE FUNCTION S(X) .
*             AS BEFORE , WE HAVE TAKEN FOR THE CALCULATION OF THE
*             COEFFICIENTS ("C(I)") THE PRIMITIVE OF THE HISTOGRAM
*             IDD , THE COEFFICIENTS WE CALCULATE NOW CORRESPOND AT
*             THE VALUES OF THE SPLINE FUNCTION AT THE NODES . (SEE
*             THE EXPLANATION IN THE DOCUMENTATION) .
*
      DO 290 J=1,NYPK
         DO 285 I=1,MXMKP2
            IJK=ICOEFF+(I-1)*NYPK+J-1
            A=FLOAT(KX)*(Q(IJK+NYPK)-Q(IJK))
            Q(IJK)=A/(Q(IKNOTX+I+KX)-Q(IKNOTX+I))
  285    CONTINUE
  290 CONTINUE
*
      DO 300 I=1,NXPK
         DO 300 J=1,MYMKP2
            IJK=ICOEFF+(I-1)*NYPK+J-1
            A=FLOAT(KY)*(Q(IJK+1)-Q(IJK))
            Q(IJK)=A/(Q(IKNOTY+J+KY)-Q(IKNOTY+J))
  300 CONTINUE
*
*              WE HAVE TO CALCULATE NOW THE VALUES OF THE SPLINE FUNC-
*              TION AT ALL THE POINTS SITUATED AT THE CENTRE OF THE
*              CELLS (I,J) OF IDD . WE REPLACE THE VALUES OF THE ORI-
*              GINAL FUNCTION WHICH REPRESENTS IDD BY THE VALUES OF
*              THE SPLINE FUNCTION S(X) .
*
      NEOLD=IQ(LCONT+KNOENT)
      CALL VZERO(IQ(LCONT+KNOENT),IQ(LCONT-1)-1)
      DO 440 IJK=1,NCX
         X=XMIN+BWIDX*FLOAT(IJK-1)
*
         DO 430 JKL=1,NCY
            Y=YMIN+BWIDY*FLOAT(JKL-1)
*
            SPLINE=0.
*
            IF(X.LT.Q(IKNOTX+KX))GO TO 420
            IF(X.GT.Q(IKNOTY-1))GO TO 420
            IF(Y.LT.Q(IKNOTY+KY))GO TO 420
            IF(Y.GT.Q(ICOEFF-1))GO TO 420
*
            DO 310 L=KXP1,MXMKP1
               KKX=L+1
               IF(Q(IKNOTX+L-1).LE.X.AND.X.LT.Q(IKNOTX+L))GO TO 320
  310       CONTINUE
*
  320       CONTINUE
*
            DO 330 L=KYP1,MYMKP1
               KKY=L+1
               IF(Q(IKNOTY+L-1).LE.Y.AND.Y.LT.Q(IKNOTY+L))GO TO 340
  330       CONTINUE
*
  340       KKXM1=KKX-1
            KKXM2=KKX-2
            KKYM1=KKY-1
            KKYM2=KKY-2
            EX1=X-Q(IKNOTX+KKXM2)
            EY1=Y-Q(IKNOTY+KKYM2)
            EX2=Q(IKNOTX+KKXM1)-X
            EY2=Q(IKNOTY+KKYM1)-Y
*
            Q(ICALCX)=1./(Q(IKNOTX+KKXM1)-Q(IKNOTX+KKXM2))
            Q(ICALCY)=1./(Q(IKNOTY+KKYM1)-Q(IKNOTY+KKYM2))
*
            DO 350 J=2,KX
               A=EX1*Q(ICALCX+J-2)
               Q(ICALCX+J-1)=A/(Q(IKNOTX+KKXM2+J)-Q(IKNOTX+KKXM2))
  350       CONTINUE
*
            DO 360 J=2,KY
               A=EY1*Q(ICALCY+J-2)
               Q(ICALCY+J-1)=A/(Q(IKNOTY+KKYM2+J)-Q(IKNOTY+KKYM2))
  360       CONTINUE
*
            DO 380 J=1,KXM1
               EX3=X-Q(IKNOTX+KKXM2-J)
               A4=Q(IKNOTX+KKXM1)-Q(IKNOTX+KKXM2-J)
               Q(ICALCX)=EX2*Q(ICALCX)/A4
               KXMJ=KX-J
               IF(KXMJ.LT.2)GO TO 380
               DO 370 L=2,KXMJ
                  A1=EX3*Q(ICALCX+L-2)
                  A2=(Q(IKNOTX+KKXM2+L)-X)*Q(ICALCX+L-1)
                  A3=Q(IKNOTX+KKXM2+L)-Q(IKNOTX+KKXM2-J)
                  Q(ICALCX+L-1)=(A1+A2)/A3
  370          CONTINUE
  380       CONTINUE
*
            DO 400 J=1,KYM1
               EY3=Y-Q(IKNOTY+KKYM2-J)
               A4=Q(IKNOTY+KKYM1)-Q(IKNOTY+KKYM2-J)
               Q(ICALCY)=EY2*Q(ICALCY)/A4
               KYMJ=KY-J
               IF(KYMJ.LT.2)GO TO 400
               DO 390 L=2,KYMJ
                  A1=EY3*Q(ICALCY+L-2)
                  A2=(Q(IKNOTY+KKYM2+L)-Y)*Q(ICALCY+L-1)
                  A3=Q(IKNOTY+KKYM2+L)-Q(IKNOTY+KKYM2-J)
                  Q(ICALCY+L-1)=(A1+A2)/A3
  390          CONTINUE
  400       CONTINUE
*
            KKXMK2=KKX-KX-2
            KKYMK2=KKY-KY-2
*
            DO 415 I=1,KX
               IX=KKXM1+I
               LX=KKXMK2+I
               DO 410 J=1,KY
                  JY=KKYM1+J
                  LY=KKYMK2+J
                  A1=Q(IKNOTX+IX-1)-Q(IKNOTX+IX-KXP1)
                  A2=Q(IKNOTY+JY-1)-Q(IKNOTY+JY-KYP1)
                  A3=Q(ICALCX+I-1)*Q(ICALCY+J-1)
                  A4=Q(ICOEFF+(LX-1)*NYPK+LY-1)
                  SPLINE=SPLINE+A1*A2*A3*A4
  410          CONTINUE
  415       CONTINUE
*
            IF(SPLINE.EQ.0.)GO TO 420
*
            IF(SPLINE.LT.0..AND.NB.LT.32)SPLINE=0.
*
  420       CONTINUE
*
            CALL HFCXY(IJK,JKL,SPLINE)
*
  430    CONTINUE
  440 CONTINUE
      IQ(LCONT+KNOENT)=NEOLD
*
  999 RETURN
      END
