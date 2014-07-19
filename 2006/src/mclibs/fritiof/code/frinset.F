*
* $Id: frinset.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: frinset.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C******************************** END FRMXGPT ********************************
 
C******************************** FRINSET ********************************
 
      SUBROUTINE FRINSET(NA,N1,N2,NOG,IQ)
 
C.......To place a gluon specified by NA on FRATLE1 block onto LUJETS
C.......between N1,N2, where N1 is assumed to be quark N2G could be
C.......the gluon kink or the end diquark.
C.......The placing is based on rapidity
C.......ordering, and if more than one rapidity-ordered spots are
C.......found then the one giving maximum invariant mass is used.
C.......If no such place is found then NA is placed near N1.
C....... IQ:=0 the actual insertion will not take place, N1,N2,N unchanged;
C........   =1 the insertion takes place, and the entire LUJETS N>NOG
C........      is shifted by 1.
C....... output NOG: the actual place NA was placed;
C....... The inserted gluon has a code: K(NOG,5)=100.
C....... The gluon will not be placed at the string ends.  So in case of
C....... q-qbar pairs from gluon splitting in Ariadne, the gluon
C....... will not be placed between the pair.
 
 
      PARAMETER (KSZJ=4000,KSZ2=300)
      COMMON/LUJETS/N,K(KSZJ,5),P(KSZJ,5),V(KSZJ,5)
      COMMON/FRATLE1/NA1,KA1(KSZ2,5),PA1(KSZ2,5)
      DIMENSION Y(2)
      SAVE /LUJETS/,/FRATLE1/
 
      NOG = 0
      IF(N2.LE.N1+1) GOTO 250
 
      YGL = PA1(NA,4) - PA1(NA,3)
      YGU = PA1(NA,4) + PA1(NA,3)
      IF(YGU.LE.0.) THEN
      YG = -.9E10
      ELSEIF(YGL.LE.0.) THEN
      YG = +.9E10
      ELSE
      YG = .5* LOG(YGU/YGL)
      ENDIF
 
      VM2X = 0.
      DO 200 I = N1, N2-1
      IF(K(I,5).EQ.222.AND.K(I,2).EQ.21) GOTO 250
      NUM=2
      IF(I.NE.N1) THEN
      NUM=1
      Y(1) = Y(2)
      ENDIF
       DO 100 II=1,NUM
       IR = I+2-II
       YL = P(IR,4) - P(IR,3)
       YU = P(IR,4) + P(IR,3)
CC       IF(YU.LE.0.AND.YL.LE.0.) CALL FRMGOUT(0,1,' 0 MOMENTA!',
CC     >         FLOAT(IR),P(IR,1),P(IR,2),P(IR,3),P(IR,4))
       IF(YU.LE.0.) THEN
       YR = -1.E10
       ELSEIF(YL.LE.0.) THEN
       YR = +1.E10
       ELSE
       YR = .5* LOG(YU/YL)
       ENDIF
       Y(3-II) = YR
100    CONTINUE
 
      IF( K(I,1).EQ.2 .AND.
     >  (YG.GE.MIN(Y(1),Y(2)).AND.YG.LE.MAX(Y(1),Y(2))) ) THEN
      VM2=(PA1(NA,4)+P(I,4)+P(I+1,4))**2-(PA1(NA,3)
     >     +P(I,3)+P(I+1,3))**2-(PA1(NA,2)+P(I,2)+P(I+1,2))**2-
     >     (PA1(NA,1)+P(I,1)+P(I+1,1))**2
       IF(VM2.GT.VM2X) THEN
       VM2X = VM2
       NOG = I+1
       ENDIF
      ENDIF
200   CONTINUE
 
250    NOG = MAX(N1+1, NOG)
 
      IF (IQ.GE.1) THEN
      DO 150 I = N, NOG, -1
      DO 150 LO = 1,5
      K(I+1,LO) = K(I,LO)
150   P(I+1,LO) = P(I,LO)
 
      DO 160 LO = 1,5
      K(NOG,LO) = KA1(NA,LO)
160   P(NOG,LO) = PA1(NA,LO)
      P(NOG,4) = SQRT(P(NOG,1)**2+P(NOG,2)**2+P(NOG,3)**2)
      P(NOG,5) = 0.0
      K(NOG,3) = 0
      K(NOG,4) = 0
      K(NOG,5) = 100
      N = N+1
      ENDIF
 
      RETURN
      END
