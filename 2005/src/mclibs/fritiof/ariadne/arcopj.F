*
* $Id: arcopj.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
*
* $Log: arcopj.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:17  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: arcopj.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
 
      SUBROUTINE ARCOPJ
 
C...ARiadne subroutine COPy Jet
 
C...Copies particles into jet initiators in /LUCLUS/
 
 
      COMMON /ARJETX/ N,K(300,5),P(300,5),V(300,5)
      SAVE /ARJETX/
 
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
 
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      SAVE /LUDAT2/
 
 
C...Reset jet counter
      MSTU(3)=0
 
C...Loop over all particles in the event record
      DO 100 I=1,N
 
C...Disregard all decayed particles and unknown entries
        IF(K(I,1).LE.0.OR.K(I,1).GE.10) GOTO 100
 
C...Disregard neutrinos and neutral particles according to MSTU(41)
        IF(MSTU(41).GE.2) THEN
          KC=LUCOMP(K(I,2))
          IF(KC.EQ.0.OR.KC.EQ.12.OR.KC.EQ.14.OR.KC.EQ.16.OR.KC.EQ.18)
     $      GOTO 100
          IF(MSTU(41).GE.3.AND.KCHG(KC,2).EQ.0.AND.LUCHGE(K(I,2)).EQ.0)
     $      GOTO 100
        ENDIF
 
        IF(N+MSTU(3)+1.GT.MSTU(4)) THEN
          CALL LUERRM(11,'(ARCLUS:) no more memory left in LUJETS')
          MSTU(3)=-1
          RETURN
        ENDIF
 
C...Tag found jet-initiator
        MSTU(3)=MSTU(3)+1
        IJ=N+MSTU(3)
        DO 200 J=1,5
          P(IJ,J)=P(I,J)
 200    CONTINUE
        K(IJ,1)=31
        K(IJ,2)=97
        K(IJ,3)=0
        K(IJ,4)=0
        K(IJ,5)=0
        V(IJ,1)=P(I,4)**2-P(I,3)**2-P(I,2)**2-P(I,1)*2
        V(IJ,5)=0
 
 100  CONTINUE
 
      RETURN
 
C**** END OF ARCOPJ ****************************************************
      END
