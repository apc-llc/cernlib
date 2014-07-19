*
* $Id: ardump.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
*
* $Log: ardump.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:17  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: ardump.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
 
      SUBROUTINE ARDUMP
 
C...ARiadne subroutine DUMP
 
C...Dumps the entries in /ARPART/ into /LUJETS/
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),IEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),IPART
      SAVE /ARPART/
 
      COMMON /ARDIPS/ BX1(MAXDIP),BX3(MAXDIP),PT2IN(MAXDIP),
     $                SDIP(MAXDIP),IP1(MAXDIP),IP3(MAXDIP),
     $                AEX1(MAXDIP),AEX3(MAXDIP),QDONE(MAXDIP),
     $                QEM(MAXDIP),IRAD(MAXDIP),ISTR(MAXDIP),IDIPS
      SAVE /ARDIPS/
 
      COMMON /ARSTRS/ IPF(MAXSTR),IPL(MAXSTR),IFLOW(MAXSTR),
     $                PT2LST,IMF,IML,IO,QDUMP,ISTRS
      SAVE /ARSTRS/
 
      COMMON /ARJETX/ N,K(300,5),P(300,5),V(300,5)
      SAVE /ARJETX/
 
CCPH:..............................................................
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
CCPH:^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 
      INXT(I)=IP3(IDO(I))
 
 
C...Tag particles in old string with pointers to cascaded string
      DO 100 I=IMF,IML
        K(I,1)=K(I,1)+10
        K(I,4)=N+1
        K(I,5)=N+IPART
 100  CONTINUE
 
C...Loop over all strings in dipole record
      DO 200 IS=1,ISTRS
 
C...Loop over all particles in each string
        I=IPF(IS)
 210    N=N+1
 
CCPH:..............................................................
        IF(N.GT.300) THEN
        WRITE(MSTA(8),*) '**',N,'  Extend ARJETX in Ariadne **'
        STOP
        ENDIF
CCPH:^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 
        DO 220 J=1,5
          P(N,J)=BP(I,J)
          V(N,J)=V(IMF,J)
 220    CONTINUE
        K(N,2)=IFL(I)
        K(N,3)=IMF
        K(N,4)=IEX(I)
        K(N,5)=INO(I)
        IF(I.EQ.IPL(IS)) THEN
          K(N,1)=1
        ELSE
          K(N,1)=2
          I=INXT(I)
          GOTO 210
        ENDIF
 200  CONTINUE
 
C...Set pointers to cascaded string
      IMF=N+1-IPART
      IML=N
      QDUMP=.TRUE.
 
      RETURN
 
C**** END OF ARDUMP ****************************************************
      END
