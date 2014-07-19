*
* $Id: ardump.F,v 1.1.1.1 1996/03/08 16:51:01 mclareni Exp $
*
* $Log: ardump.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:01  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: ardump.F,v 1.1.1.1 1996/03/08 16:51:01 mclareni Exp $
 
      SUBROUTINE ARDUMP
 
C...ARiadne subroutine DUMP
 
C...Dumps the entries in /ARPART/ into /LUJETS/
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),QEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),INQ(MAXPAR),
     $                XPMU(MAXPAR),XPA(MAXPAR),PT2GG(MAXPAR),IPART
      SAVE /ARPART/
      COMMON /ARDIPS/ BX1(MAXDIP),BX3(MAXDIP),PT2IN(MAXDIP),
     $                SDIP(MAXDIP),IP1(MAXDIP),IP3(MAXDIP),
     $                AEX1(MAXDIP),AEX3(MAXDIP),QDONE(MAXDIP),
     $                QEM(MAXDIP),IRAD(MAXDIP),ISTR(MAXDIP),
     $                ICOLI(MAXDIP),IDIPS
      SAVE /ARDIPS/
      COMMON /ARSTRS/ IPF(MAXSTR),IPL(MAXSTR),IFLOW(MAXSTR),
     $                PT2LST,PT2MAX,IMF,IML,IO,QDUMP,ISTRS
      SAVE /ARSTRS/
      COMMON /LUJETS/ N,K(4000,5),P(4000,5),V(4000,5)
      SAVE /LUJETS/
 
      INXT(I)=IP3(IDO(I))
 
 
C...Loop over all strings in dipole record
      DO 200 IS=1,ISTRS
 
C...Loop over all particles in each string
        I=IPF(IS)
 210    N=N+1
        DO 220 J=1,5
          P(N,J)=BP(I,J)
          V(N,J)=V(IMF,J)
 220    CONTINUE
        K(N,2)=IFL(I)
        K(N,3)=IMF
        K(N,4)=-I
        K(N,5)=INO(I)
        IF (I.EQ.IPL(IS).AND.IFLOW(IS).NE.2) THEN
          K(N,1)=1
        ELSEIF (IFLOW(IS).EQ.2.AND.INXT(I).EQ.IPF(IS)) THEN
          K(N,1)=1
        ELSE
          K(N,1)=2
          I=INXT(I)
          GOTO 210
        ENDIF
 200  CONTINUE
 
      IEXTRA=0
      DO 250 I=MAXPAR-4,MAXPAR-3
        IF (.NOT.QQ(I)) GOTO 250
        IEXTRA=IEXTRA+1
        N=N+1
        DO 260 J=1,5
          P(N,J)=BP(I,J)
          V(N,J)=V(IMF,J)
 260    CONTINUE
        K(N,1)=1
        K(N,2)=IFL(I)
        K(N,3)=IMF
        K(N,4)=-I
        K(N,5)=0
 250  CONTINUE
 
C...Set pointers to cascaded string and fix complex remnant
      IMFNEW=N+1-IPART-IEXTRA
      IMLNEW=N
 
C...Tag particles in old string with pointers to cascaded string
      DO 100 I=1,IML
        IF (K(I,1).LT.10.AND.K(I,4).LT.0) THEN
          K(I,1)=K(I,1)+10
          K(I,4)=IMFNEW
          K(I,5)=IMLNEW
        ENDIF
 100  CONTINUE
 
      IMF=IMFNEW
      IML=IMLNEW
      QDUMP=.TRUE.
 
C...Check if Drell-Yan particle is present
      IF (QQ(MAXPAR-2)) THEN
        IDY=IDI(MAXPAR-2)
        DO 300 J=1,5
          P(IDY,J)=BP(MAXPAR-2,J)
 300    CONTINUE
      ENDIF
 
      RETURN
 
C**** END OF ARDUMP ****************************************************
      END
