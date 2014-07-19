C***********************************************************************
C $Id: ardump.F,v 1.2 1996/04/10 12:33:08 mclareni Exp $

      SUBROUTINE ARDUMP

C...ARiadne subroutine DUMP 

C...Dumps the entries in /ARPART/ into /LUJETS/

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "lujets.f"

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
