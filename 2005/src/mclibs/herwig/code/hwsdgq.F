CDECK  ID>, HWSDGQ.
*CMZ :-        -26/04/91  13.04.45  by  Federico Carminati
*-- Author :    Drees, Grassie, Charchula, modified by Bryan Webber
C --------------------------------------
C  QUARK PART OF THE PHOTON SF
C --------------------------------------
      FUNCTION HWSDGQ(X,Q2,NFL,NCH)
      IMPLICIT REAL (A-H,P-Z)
      INTEGER NFL,NCH
      DIMENSION A(5,4,2,3),AT(5,2),XQPOM(2),E(2)
      COMMON/DG/F2
C SQUARE OF LAMBDA=0.4 GEV
      ALAM2=0.160
      T=LOG(Q2/ALAM2)
C
C  CHECK WHETHER NFL AND NCH HAVE RIGHT VALUES
C
      IF(.NOT.((NFL.EQ.3).OR.(NFL.EQ.4).OR.(NFL.EQ.5))) THEN
 110   WRITE(6,111)
 111   FORMAT('NUMBER OF FLAVOURS (NFL) HAS NOT BEEN SET TO: 3,4 OR 5'/
     *'          NFL=3 IS ASSUMED')
       NFL=3
      ELSEIF (T.LE.0) THEN
       WRITE(6,132)
 132   FORMAT(' HWSDGQ CALLED WITH SCALE < LAMBDA. RETURNING ZERO.')
       HWSDGQ=0
       RETURN
      ENDIF
      IF (.NOT.((NCH.EQ.1).OR.(NCH.EQ.2))) THEN
 120     WRITE(6,121)
 121     FORMAT(' QUARK CHARGE NUMBER (NCH) HAS NOT BEEN SET',
     *'           TO 1 OR 2;'/
     *'           NCH=1 IS ASSUMED')
         NCH=1
      ENDIF
C ------ INITIALIZATION ------
      DATA(((A(I,J,K,1),I=1,5),J=1,4),K=1,2)/
     + 2.28500,  6.07300, -0.42020,-0.08080, 0.05530,
     +-0.01530, -0.81320,  0.01780, 0.63460, 1.13600,
     + 1.3300E3,-41.3100,   0.92160, 1.20800, 0.95120,
     + 4.21900,  3.16500,  0.18000, 0.20300, 0.01160,
     +16.6900,   0.17600, -0.02080,-0.01680,-0.19860,
     +-0.79160,  0.04790,  0.3386E-2,1.35300, 1.10000,
     + 1.0990E3,  1.04700,  4.85300, 1.42600, 1.13600,
     + 4.42800,  0.02500,  0.84040, 1.23900,-0.27790/
        DATA(((A(I,J,K,2),I=1,5),J=1,4),K=1,2)/
     +-0.37110,-0.17170, 0.087660,-0.89150,-0.18160,
     + 1.06100, 0.78150, 0.021970, 0.28570, 0.58660,
     + 4.75800, 1.53500, 0.109600, 2.97300, 2.42100,
     +-0.01500, 0.7067E-2,0.204000, 0.11850, 0.40590,
     +-0.12070,25.00000,-0.012300,-0.09190, 0.020150,
     + 1.07100,-1.64800, 1.162000, 0.79120, 0.98690,
     + 1.97700,-0.015630,0.482400, 0.63970,-0.070360,
     +-0.8625E-2,6.43800,-0.011000, 2.32700, 0.016940/
        DATA(((A(I,J,K,3),I=1,5),J=1,4),K=1,2)/
     +15.80,     2.7420,  0.029170,-0.03420, -0.023020,
     +-0.94640, -0.73320, 0.046570, 0.71960,  0.92290,
     +-0.50,     0.71480, 0.17850,  0.73380,  0.58730,
     +-0.21180,  3.2870,  0.048110, 0.081390,-0.79E-4,
     + 6.7340,  59.880,  -0.3226E-2,-0.03321,   0.10590,
     +-1.0080,  -2.9830,  0.84320,  0.94750,  0.69540,
     +-0.085940, 4.480,   0.36160, -0.31980, -0.66630,
     + 0.076250, 0.96860, 0.1383E-2, 0.021320, 0.36830/
      CF=10.0
C ------- EVALUATION OF PARAMETERS IN Q2 ---------
      E(1)=1.0
      IF (NFL.EQ.3) THEN
        E(2)=9.0
        LF=1
      ELSEIF (NFL.EQ.4) THEN
        E(2)=10.0
        LF=2
      ELSEIF (NFL.EQ.5) THEN
        E(2)=55.0/6.0
        LF=3
      ENDIF
      DO 10 J=1,2
        DO 20 I=1,5
           ATP=A(I,1,J,LF)*T**A(I,2,J,LF)
           AT(I,J)=ATP+A(I,3,J,LF)*T**(-A(I,4,J,LF))
 20     CONTINUE
 10   CONTINUE
      DO 30 J=1,2
       POM1=X*(X*X+(1.0-X)**2)/(AT(1,J)-AT(2,J)*ALOG(1.0-X))
       POM2=AT(3,J)*X**AT(4,J)*(1.0-X)**AT(5,J)
       XQPOM(J)=E(J)*POM1+POM2
 30   CONTINUE
C -------  QUARK DISTRIBUTIONS ----------
      IF (NFL.EQ.3) THEN
         IF (NCH.EQ.2) THEN
           HWSDGQ=1.0/6.0*(XQPOM(2)+9.0*XQPOM(1))
         ELSEIF(NCH.EQ.1) THEN
           HWSDGQ=1.0/6.0*(XQPOM(2)-9.0/2.0*XQPOM(1))
         ENDIF
        F2=2.0/9.0*XQPOM(2)+XQPOM(1)
      ELSEIF (NFL.EQ.4) THEN
         IF (NCH.EQ.2) THEN
           HWSDGQ=1.0/8.0*(XQPOM(2)+6.0*XQPOM(1))
         ELSEIF(NCH.EQ.1) THEN
           HWSDGQ=1.0/8.0*(XQPOM(2)-6.0*XQPOM(1))
         ENDIF
        F2=5.0/18.0*XQPOM(2)+XQPOM(1)
      ELSEIF (NFL.EQ.5) THEN
         IF (NCH.EQ.2) THEN
           HWSDGQ=1.0/10.0*(XQPOM(2)+15.0/2.0*XQPOM(1))
         ELSEIF(NCH.EQ.1) THEN
           HWSDGQ=1.0/10.0*(XQPOM(2)-5.0*XQPOM(1))
         ENDIF
        F2=11.0/45.0*XQPOM(2)+XQPOM(1)
      ENDIF
      HWSDGQ=HWSDGQ/137.
      RETURN
      END
