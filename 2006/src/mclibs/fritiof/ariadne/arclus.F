*
* $Id: arclus.F,v 1.1.1.1 1996/01/11 14:05:16 mclareni Exp $
*
* $Log: arclus.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:16  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: arclus.F,v 1.1.1.1 1996/01/11 14:05:16 mclareni Exp $
 
      SUBROUTINE ARCLUS(NJET)
 
C...ARiadne subroutine jet CLUStering
 
C...Clusters particle in the /LUJETS/ common block into jets according
C...the dipole clustering algorithm.
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
 
      COMMON /ARJETX/ N,K(300,5),P(300,5),V(300,5)
      SAVE /ARJETX/
 
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
 
 
C...Reset error flag.
      MSTA(13)=0
 
C...Copy all particle to be considered as jet-initiators to the end
C...of the event record.
      IF(MSTU(48).EQ.0) CALL ARCOPJ
 
C...The total number of jetinitiators = current number of jets.
      NJET=MSTU(3)
      I1=0
      I3=0
 
C...Loop over all possible three-jets to find the three jets with
C...smallest invariant p_t^2
100   IF(NJET.LE.MAX(MSTU(47),2)) THEN
        CALL ARORDJ
        RETURN
      ENDIF
 
      J1=0
      J2=0
      J3=0
      PT2MIN=PARA(31)
 
      DO 110 I2=N+1,N+MSTU(3)
        IF(K(I2,5).LT.0) GOTO 110
        CALL ARUPDJ(I2,I1,I3)
        IF(V(I2,5).LT.PT2MIN) THEN
          J1=K(I2,3)
          J2=I2
          J3=K(I2,4)
          PT2MIN=V(I2,5)
        ENDIF
110   CONTINUE
 
C...Exit if smallest p_t^2 is above cutoff
      IF(J1.EQ.0) THEN
        CALL ARORDJ
        RETURN
      ENDIF
 
C...Else join the three jets into two and redo the procedure.
      CALL ARJOIN(J1,J2,J3)
      K(J2,5)=-1
      I1=J1
      I3=J3
      NJET=NJET-1
 
      GOTO 100
 
C**** END OF ARCLUS ****************************************************
      END
