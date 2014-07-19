*
* $Id: frsaven.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: frsaven.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C********************************* END FRINSKK ***************************
 
C*************************** FRSAVEN ********************************
 
 
      SUBROUTINE FRSAVEN(N1,IQ)
 
C..IQ=0: To save a lujets configuration (with 3 partons) N1 and N temporarily
C..IQ=\=0: restore the configuration at N1 and N1+1=N
 
      PARAMETER (KSZJ=4000)
      COMMON/LUJETS/N,K(KSZJ,5),P(KSZJ,5),V(KSZJ,5)
      DIMENSION KM(3,5),PM(3,5)
      SAVE KM, PM, NUM
      SAVE /LUJETS/
 
      IF(IQ.EQ.0) THEN
      NUM = N-N1+1
      IF(NUM.GT.3) CALL FRMGOUT(0,0,'More than 3 partons in
     > FRSAVEN',float(n1),float(N),0.,0.,0.)
 
      DO 100 I=N1,N
      IM = I-N1+1
      DO 100 LO=1,5
      PM(IM,LO) = P(I,LO)
100   KM(IM,LO) = K(I,LO)
 
      ELSE
 
      DO 200 I=1,NUM
      NI = N1+I-1
      DO 200 LO=1,5
      P(NI,LO) = PM(I,LO)
200   K(NI,LO) = KM(I,LO)
 
      N = N1+NUM-1
 
      ENDIF
 
      RETURN
      END
