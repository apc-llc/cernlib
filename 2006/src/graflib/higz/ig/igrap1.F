*
* $Id: igrap1.F,v 1.1.1.1 1996/02/14 13:10:38 mclareni Exp $
*
* $Log: igrap1.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:38  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/08 21/07/94  12.19.27  by  O.Couet
*-- Author :
      SUBROUTINE IGRAP1(X,Y,N,IITYP)
*.===========>
*.
*.   Underlaying routine for IGHIST  Based on the CERN GD3 routine TVIPTE
*.
*.     Author - Marlow etc.   Modified by - P. Ward     Date -  3.10.1973
*.
*.   This routine draws a smooth tangentially continuous curve through
*. the sequence of data points P(I) I=1,N where P(I)=(X(I),Y(I))
*. the curve is approximated by a polygonal arc of short vectors .
*. the data points can represent open curves, P(1).NE.P(N) or closed
*. curves P(2).EQ.P(N) . If a tangential discontinuity at P(I) is
*. required , then set P(I)=P(I+1) . loops are also allowed .
*.
*. Reference Marlow and Powell,Harwell report No.R.7092.1972
*. MCCONALOGUE,Computer Journal VOL.13,NO4,NOV1970PP392 6
*.
*. _Input parameters:
*.
*.  INTEGER N : Number of data points.
*.  REAL X(N) : Abscissa
*.  REAL Y(N) : Ordinate
*.
*.
*. XDELT is the accuracy required in constructing the curve.
*. if it is zero then the routine calculates a value other-
*. wise it uses this value. (default is 0.0)
*.
*.     JIPTE=0   No coordinate transformation
*.          =1   Transformation for graph in  X  direction
*.          =2   Transformation for graph in  Y  direction
*.          =3   Transformation for contour plotting  (make square)
*.     KIPTE .LT. 0  Set JIPTE as GD3 routines wish
*.           .GE. 0  Set JIPTE=KIPTE  (set by user)
*.
*. ITYP has different value according to CHOPT in IGHIST:
*.
*.     CHOPT='S'     ITYP=1
*.     CHOPT='SA'    ITYP=2
*.     CHOPT='SA1'   ITYP=3
*.     CHOPT='RS'    ITYP=11
*.     CHOPT='RSA'   ITYP=12
*.     CHOPT='RSA1'  ITYP=13
*.
*..==========>
#include "higz/hipaw.inc"
#include "higz/hiatt.inc"
      DIMENSION X(N),Y(N)
      LOGICAL FLGIC,FLGIS,LOPTX
      EQUIVALENCE (LX,LSTACK(9)),(LY,LSTACK(10))
*.______________________________________
*
      NPMAX  = N*10
      N2     = NPMAX-2
      IBKSIZ = N2
      CALL MZNEED(IXHIGZ,2*NPMAX+25,'G')
      IF (IQUEST(11).LT.0) THEN
         CALL IGERR('IGRAP1','Not enough space in memory')
         RETURN
      ENDIF
      IF(LX.NE.0)CALL MZDROP(IXHIGZ,LX,' ')
      IF(LY.NE.0)CALL MZDROP(IXHIGZ,LY,' ')
      CALL MZBOOK(IXHIGZ,LX,LX,1,'TMPX',0,0,NPMAX,3,0)
      CALL MZBOOK(IXHIGZ,LY,LY,1,'TMPY',0,0,NPMAX,3,0)
*
*     Decode the type of curve according to
*     CHOPT of IGHIST.
*     ('S', 'SA', 'SA1' ,'XS', 'XSA', or 'XSA1')
*
      ITYP  = IITYP
      IF(ITYP.GE.1000)ITYP=ITYP-1000
      LOPTX = .FALSE.
      JTYP  = ITYP-10
      IF(JTYP.GT.0)THEN
         KTYP  = JTYP
         LOPTX = .TRUE.
      ELSE
         KTYP = ITYP
      ENDIF
      IF(KTYP.EQ.3)THEN
         XORG = RWXMIN
         YORG = RWYMIN
      ELSE
         XORG = MAX(0.,RWXMIN)
         YORG = MAX(0.,RWYMIN)
      ENDIF
*
      MAXIT = 20
      DELT  = 0.00055
*
*          Scale data to the range 0-RATSIG in X, 0-1 in Y
*          where RATSIG is the ratio between the number of changes
*          of sign in Y divided by the number of changes of sign in X
*
      SXMIN = X(1)
      SXMAX = X(1)
      SYMIN = Y(1)
      SYMAX = Y(1)
      SIX   = 1.
      SIY   = 1.
      DO 10 I=2,N
         IF(I.GT.2)THEN
            IF((X(I)-X(I-1))*(X(I-1)-X(I-2)).LT.0.)SIX=SIX+1.
            IF((Y(I)-Y(I-1))*(Y(I-1)-Y(I-2)).LT.0.)SIY=SIY+1.
         ENDIF
         IF(X(I).LT.SXMIN)SXMIN=X(I)
         IF(X(I).GT.SXMAX)SXMAX=X(I)
         IF(Y(I).LT.SYMIN)SYMIN=Y(I)
         IF(Y(I).GT.SYMAX)SYMAX=Y(I)
   10 CONTINUE
      ICLOSE = 0
      DX1N   = ABS(X(N)-X(1))
      DY1N   = ABS(Y(N)-Y(1))
      CLO    = 0.01
      IF(DX1N.LT.CLO*(SXMAX-SXMIN).AND.DY1N.LT.CLO*(SYMAX-SYMIN))
     +        ICLOSE = 1
      IF(SXMIN.EQ.SXMAX)THEN
         XRAT = 1.
      ELSE
         IF(SIX.GT.1.)THEN
            RATSIG = SIY/SIX
         ELSE
            RATSIG = 20.
         ENDIF
         XRAT = RATSIG/(SXMAX-SXMIN)
      ENDIF
      IF(SYMIN.EQ.SYMAX)THEN
         YRAT = 1.
      ELSE
         YRAT = 1./(SYMAX-SYMIN)
      ENDIF
*
      Q(LX+1) = X(1)
      Q(LY+1) = Y(1)
      DO 20 I=1,N
         X(I) = (X(I)-SXMIN)*XRAT
         Y(I) = (Y(I)-SYMIN)*YRAT
   20 CONTINUE
*
*              IFIN is minus one if we must draw a straight line from P(K-1)
*              to P(K). IFIN is one if the last call to IPL has .LT. N2
*              points. IFIN is zero otherwise. NPT counts the X and Y
*              coordinates in work . When NPT=N2 a call to IPL is made.
*
      IFIN = 0
      NPT  = 1
      K    = 1
*
*              Convert coordinates back to original system
*
*
*              Separate the set of data points into arcs P(K-1),P(K).
*              Calculate the direction cosines. first consider whether
*              there is a continuous tangent at the endpoints.
*
      IF(ICLOSE.NE.0)GO TO 30
      IF(X(1).NE.X(N).OR.Y(1).NE.Y(N))GOTO 40
      IF(X(N-1).EQ.X(N).AND.Y(N-1).EQ.Y(N))GOTO 40
      IF(X(1).EQ.X(2).AND.Y(1).EQ.Y(2))GOTO 40
   30 FLGIC = .FALSE.
      FLGIS = .TRUE.
*
*              FLGIC is true if the curve is open and false if it is closed.
*              FLGIS is true in the main loop, but is false if there is
*              a deviation from the main loop.
*
      KM=N-1
*
*              Calculate direction cosines at P(1) using P(N-1),P(1),P(2).
*
      GOTO 100
   40 FLGIC = .TRUE.
      FLGIS = .FALSE.
*
*              Skip excessive consecutive equal points.
*
   50 IF(K.GE.N)GOTO 380
      K=K+1
      IF(X(K).EQ.X(K-1).AND.Y(K).EQ.Y(K-1))GOTO 50
   60 KM=K-1
      IF(K-N)90,70,380
   70 IF(FLGIC)GOTO 80
*
*              Calculate direction cosines at P(n) using P(N-1),P(N),P(2).
*
      KP=2
      GOTO 130
   80 IF(FLGIS)GOTO 150
*
*              Draw a straight line from P(K-1) to P(K).
*
      IFIN=-1
      GOTO 170
*
*              Test whether P(K) is a cusp.
*
   90 IF(X(K).EQ.X(K+1).AND.Y(K).EQ.Y(K+1))GOTO 80
  100 KP=K+1
      GOTO 130
*
*              Branch if the next section of the curve begins at a cusp.
*
  110 IF(.NOT.FLGIS)GOTO 50
*
*              Carry forward the direction cosines from the previous arc.
*
  120 CO=CT
      SO=ST
      K=K+1
      GOTO 60
*
*              Calculate the direction cosines at P(K).  If K=1 then
*              N-1 is used for K-1. If K=N then 2 is used for K+1.
*              direction cosines at P(K) obtained from P(K-1),P(K),P(K+1).
*
  130 DX1 = X(K)-X(KM)
      DY1 = Y(K)-Y(KM)
      DK1 = DX1**2+DY1**2
      DX2 = X(KP)-X(K)
      DY2 = Y(KP)-Y(K)
      DK2 = DX2**2+DY2**2
      CTU = DX1*DK2+DX2*DK1
      STU = DY1*DK2+DY2*DK1
      XNT = CTU**2+STU**2
      IF(XNT.GT.1.E-25)GOTO 140
*
*              If both ctu and stu are zero,then default.This can
*              occur when P(K)=P(K+1). I.E. A loop.
*
      CTU = DY1
      STU =-DX1
      XNT = DK1
*
*              Normalise direction cosines.
*
  140 CT = CTU/SQRT(XNT)
      ST = STU/SQRT(XNT)
      IF(FLGIS)GOTO 160
*
*              Direction cosines at P(K-1) obtained from P(K-1),P(K),P(K+1).
*
      W3    = 2.*(DX1*DY2-DX2*DY1)
      CO    = CTU+W3*DY1
      SO    = STU-W3*DX1
      XNT   = 1./SQRT(CO**2+SO**2)
      CO    = CO*XNT
      SO    = SO*XNT
      FLGIS = .TRUE.
      GOTO 170
*
*              Direction cosines at P(K) obtained from P(K-2),P(K-1),P(K).
*
  150 W3    = 2.*(DX1*DY2-DX2*DY1)
      CT    = CTU-W3*DY2
      ST    = STU+W3*DX2
      XNT   = 1./SQRT(CT**2+ST**2)
      CT    = CT*XNT
      ST    = ST*XNT
      FLGIS = .FALSE.
      GOTO 170
  160 IF(K.LE.1)GOTO 120
*
*              For the arc between P(K-1) and P(K) with direction cosines CO,
*              SO and CT,ST respectively, calculate the coefficients of the
*              parametric cubic represented by X(T) and Y(T) where
*              X(T)=XA*T**3 + XB*T**2 + CO*T + XO
*              Y(T)=YA*T**3 + YB*T**2 + SO*T + YO
*
  170 XO = X(K-1)
      YO = Y(K-1)
      DX = X(K)-XO
      DY = Y(K)-YO
*
*              Initialise the values of X(TI),Y(TI) in XT and YT respectively.
*
      XT = XO
      YT = YO
      IF(IFIN.LT.0)GOTO 350
      C  = DX**2+DY**2
      A  = CO+CT
      B  = SO+ST
      R  = DX*A+DY*B
      T  = C*6./(SQRT(R**2+2.*(7.-CO*CT-SO*ST)*C)+R)
      XA = (A*T-2.*DX)/T**3
      XB = (3.*DX-(CO+A)*T)/T**2
      YA = (B*T-2.*DY)/T**3
      YB = (3.*DY-(SO+B)*T)/T**2
*
*              If the curve is close to a straight line then use a straight
*              line between (XO,YO) and (XT,YT).
*
      IF(.75*MAX(ABS(DX*SO-DY*CO),ABS(DX*ST-DY*CT)).LE.DELT)GOTO 340
*
*              Calculate a set of values 0.EQ.T(0).LTCT(1).LT. ... .LT.T(M)=TC
*              such that polygonal arc joining X(T(J)),Y(T(J)) (J=0,1,..M)
*              is within the required accuracy of the curve
*
      TJ = 0.
      U1 = YA*XB-YB*XA
      U2 = YB*CO-XB*SO
      U3 = SO*XA-YA*CO
*
*              Given T(J), calculate T(J+1). The values of X(T(J)),
*              Y(T(J)) T(J) are contained in XT,YT and TJ respectively.
*
  180 S  = T-TJ
      IW = -2
*
*              Define IW here later.
*
      P1 = (2.*U1)*TJ-U3
      P2 = (U1*TJ-U3)*3.*TJ+U2
      P3 = 3.*TJ*YA+YB
      P4 = (P3+YB)*TJ+SO
      P5 = 3.*TJ*XA+XB
      P6 = (P5+XB)*TJ+CO
*
*              Test D(TJ,THETA). A is set to (Y(TJ+S)-Y(TJ))/S.B is
*              set to (X(TJ+S)-X(TJ))/S.
*
      CC  = 0.8209285
      ERR = 0.1209835
  190 IW  = IW-2
  200 A   = (S*YA+P3)*S+P4
      B   = (S*XA+P5)*S+P6
*
*              Set Z to PSI(D/DELTA)-CC.
*
      W1 = -S*(S*U1+P1)
      W2 = S**2*U1-P2
      W3 = 1.5*W1+W2
*
*              Set the estimate of (THETA-TJ)/S.Then set the numerator
*              of the expression (EQUATION 4.4)/S. Then set the square
*              of D(TJ,TJ+S)/DELT. Then replace Z by PSI(D/DELT)-CC.
*
      STH = 0.5+SIGN(W1,W3)/(3.4*ABS(W1)+5.2*ABS(W3))
      Z   = S*STH*(S-S*STH)*(W1*STH+W1+W2)
      Z   = Z**2/((A**2+B**2)*(DELT**2))
      Z   = (Z+2.642937)*Z/((.3715652*Z+3.063444)*Z+.2441889)-CC
*
*              Branch if Z has been calculated
*
      IF(IW.GT.0)GOTO 250
      IF(Z.GT.ERR)GOTO 240
      GOTO 220
  210 IW=IW-2
  220 IF(IW+2)230,190,290
*
*              Last part of arc.
*
  230 XT = X(K)
      YT = Y(K)
      S  = 0.
      GOTO 300
*
*              Z(S). find a value of S where 0.LE.S.LE.SB such that
*              ABS(Z(S)).LT.ERR
*
  240 KP=0
      C=Z
      SB=S
  250 CALL IGRAP2(KP,0.,SB,ERR,S,Z,MAXIT)
      IF(KP-2)260,210,370
  260 IF(IW)270,280,200
*
*              Set Z=Z(S) for S=0.
*
  270 Z=-CC
      IW=0
      GOTO 250
*
*              Set Z=Z(S) for S=SB.
*
  280 Z=C
      IW=1
      GOTO 250
*
*              Update TJ,XT and YT.
*
  290 XT=XT+S*B
      YT=YT+S*A
      TJ=S+TJ
  300 NPT=NPT+1
*
*              Convert coordinates to original system
*
      Q(LX+NPT)=SXMIN+XT/XRAT
      Q(LY+NPT)=SYMIN+YT/YRAT
*
*              If a fill area must be drawn and if the banks LX and
*              LY are too small they are enlarged in order to draw
*              the filled area in one go.
*
      IF(NPT.LT.IBKSIZ)THEN
         GOTO 320
      ELSE
         IF(IITYP.GE.1000)THEN
            CALL MZPUSH(IXHIGZ,LX,0,N2,' ')
            CALL MZPUSH(IXHIGZ,LY,0,N2,' ')
            IBKSIZ = IBKSIZ+N2
            GOTO 320
         ELSE
            IF(KTYP.GT.1)THEN
               CALL MZPUSH(IXHIGZ,LX,0,N2,' ')
               CALL MZPUSH(IXHIGZ,LY,0,N2,' ')
               IBKSIZ = IBKSIZ+N2
               GOTO 320
            ENDIF
         ENDIF
      ENDIF
*
*              Draw the graph
*
  310 CONTINUE
      IF(IITYP.GE.1000)THEN
         CALL IFA(NPT,Q(LX+1),Q(LY+1))
         IF(IBORD.NE.0)CALL IPL(NPT,Q(LX+1),Q(LY+1))
      ELSE
         IF(KTYP.GT.1)THEN
            IF(.NOT.LOPTX)THEN
               Q(LX+NPT+1)=Q(LX+NPT)
               Q(LX+NPT+2)=Q(LX+1)
               Q(LY+NPT+1)=YORG
               Q(LY+NPT+2)=YORG
            ELSE
               Q(LX+NPT+1)=XORG
               Q(LX+NPT+2)=XORG
               Q(LY+NPT+1)=Q(LY+NPT)
               Q(LY+NPT+2)=Q(LY+1)
            ENDIF
            CALL IFA(NPT+2,Q(LX+1),Q(LY+1))
         ENDIF
         CALL IPL(NPT,Q(LX+1),Q(LY+1))
      ENDIF
      NPT=1
      Q(LX+NPT)=SXMIN+XT/XRAT
      Q(LY+NPT)=SYMIN+YT/YRAT
  320 IF(IFIN)360,330,390
  330 IF(S)110,110,180
*
*              Draw a straight line between (XO,YO) and (XT,YT)
*
  340 IFIN=-1
  350 XT=XT+DX
      YT=YT+DY
      GOTO 300
  360 IFIN=0
      GOTO 110
  370 CALL IGERR('Attempt to plot outside plot limits'
     +,'IGRAPH')
      GOTO 230
*
*              Prepare to clear out remaining short vectors before returning
*
  380 IFIN=1
      IF(NPT.GT.1)GOTO 310
*
*              Convert coordinates back to original system
*
  390 DO 400 I=1,N
         X(I)=SXMIN+X(I)/XRAT
         Y(I)=SYMIN+Y(I)/YRAT
  400 CONTINUE
*
      CALL MZDROP(IXHIGZ,LX,' ')
      CALL MZDROP(IXHIGZ,LY,' ')
      LX = 0
      LY = 0
*
      END
