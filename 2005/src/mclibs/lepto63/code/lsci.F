C **********************************************************************

      SUBROUTINE LSCI(PROB)
C--                                                                   --C
C--   Created:        950319                                          --C
C--   Last update:    960730                                          --C
C--   Purpose:        to generate random switches of parton           --C
C--                   colours in the partonic final state             --C
C--                   Two versions: LST(34).EQ.1 original             --C
C--                                 LST(34).NE.1 no switch between    --C
C--                                              perturbative partons --C

      IMPLICIT NONE

C--       global variables
      COMMON /LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      INTEGER N,K
      REAL P,V
      COMMON/LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST

C--       functions
      REAL RLU
C--       local variables
      INTEGER I,J,LUCOMP,NS,NEXT,THIS,INIT
      LOGICAL QUARK,QUARK1,QUARK2,AQUARK1,AQUARK2,GLUON1,GLUON2,FIRST
      REAL PROB

C--       Assign colour and anticolour pointers to all partons. Colour
C--       pointers are in K(I,4) and anticolour pointers are in K(I,5).
C--       The pointer points to the row where the respective anticolour
C--       and colour is.

      FIRST=.TRUE.
      DO 10 I=5,N
         IF (K(I,1).LT.10 .AND. K(I,1).GT.0) THEN
C--       check if parton is a quark, antiquark or diquark
            IF (ABS(K(I,2)).LT.10 .OR. LUCOMP(K(I,2)).EQ.90) THEN
               IF (K(I,2).LT.10 .AND. K(I,2).GT.0 .OR.
     &             K(I,2).LT.-1000) THEN
        	  QUARK=.TRUE.
               ELSE
        	  QUARK=.FALSE.
               ENDIF
C--       reset pointers
              K(I,4)=0
              K(I,5)=0
C--       the first quark, antiquark or diquark in a string points
C--       to the parton in the next line
               IF (FIRST) THEN
        	  IF (QUARK) THEN
                     K(I,4)=(I+1)
        	  ELSE
                     K(I,5)=(I+1)
        	  ENDIF
        	  FIRST=.FALSE.
C--       the last quark, antiquark or diquark in a string points
C--       to the parton in the previous line
               ELSE
        	  IF (QUARK) THEN
                     K(I,4)=(I-1)
        	  ELSE
                     K(I,5)=(I-1)
        	  ENDIF
        	  FIRST=.TRUE.
               ENDIF
               K(I,1)=3
C--       check if parton gluon 
            ELSEIF (K(I,2).EQ.21) THEN
C--       if the previous colour points to this gluon then its anticolour
C--       should point back and its colour should point to the next line 
               IF(K(I-1,4).EQ.I) THEN
        	  K(I,4)=(I+1)
        	  K(I,5)=(I-1)
               ELSE
        	  K(I,4)=(I-1)
        	  K(I,5)=(I+1)
               ENDIF
               K(I,1)=3
            ENDIF
         ENDIF
10    CONTINUE

C--       find first parton in colour switch
      DO 20 I=5,N
         QUARK1=.FALSE.
         AQUARK1=.FALSE.
         GLUON1=.FALSE.
         IF (K(I,1).EQ.3) THEN 
C--       check if parton quark or antidiquark
            IF (K(I,4).NE.0 .AND. K(I,5).EQ.0) THEN
               QUARK1=.TRUE.
C--       check if parton antiquark or diquark
            ELSEIF (K(I,4).EQ.0 .AND. K(I,5).NE.0) THEN
               AQUARK1=.TRUE.
C--       check if parton gluon 
            ELSEIF (K(I,2).EQ.21) THEN
               GLUON1=.TRUE.
            ENDIF
C--       find second parton in colour switch
            DO 30 J=I+1,N
               QUARK2=.FALSE.
               AQUARK2=.FALSE.
               GLUON2=.FALSE.
               IF (K(J,1).EQ.3 .AND.
C--       at least one remnant parton if LST(34).NE.1
     &            (K(J,3).EQ.2 .OR. K(I,3).EQ.2 .OR. LST(34).EQ.1))THEN
C--       check if second parton quark or antidiquark
                  IF (K(J,4).NE.0 .AND. K(J,5).EQ.0) THEN
                     QUARK2=.TRUE.
C--       check if second parton antquark or diquark
                  ELSEIF (K(J,4).EQ.0 .AND. K(J,5).NE.0) THEN
                     AQUARK2=.TRUE.
C--       check if second parton gluon 
                  ELSEIF (K(J,2).EQ.21) THEN
        	     GLUON2=.TRUE.
                  ENDIF
C--       switch colour pointers
                  IF (QUARK1.AND.QUARK2) THEN
                     IF (RLU(0).LT.PROB) CALL LECSWI(I,J)
                  ELSEIF (K(I,4).NE.J .AND. K(J,4).NE.I .AND. 
     &            (QUARK1.AND.GLUON2 .OR. GLUON1.AND.QUARK2)) THEN
                     IF (RLU(0).LT.PROB) CALL LECSWI(I,J)
                  ELSEIF (AQUARK1.AND.AQUARK2) THEN
                     IF (RLU(0).LT.PROB) CALL LEASWI(I,J)
                  ELSEIF (K(I,5).NE.J .AND. K(J,5).NE.I .AND. 
     &            (AQUARK1.AND.GLUON2 .OR. GLUON1.AND.AQUARK2)) THEN
                     IF (RLU(0).LT.PROB) CALL LEASWI(I,J)
                  ELSEIF (K(I,4).NE.J .AND. K(J,4).NE.I .AND.
     &                 GLUON1.AND.GLUON2) THEN
                     IF (RLU(0).LT.PROB) CALL LECSWI(I,J)
                     IF (RLU(0).LT.PROB) CALL LEASWI(I,J)
                  ENDIF
               ENDIF
30          CONTINUE
         ENDIF
20    CONTINUE

C--       restore colour order in strings ready for hadronisation
      NS=N
      DO 40 I=5,NS
C--       find first quark (or anti diquark) string end
         IF (K(I,1).EQ.3 .AND. K(I,4).NE.0 .AND. K(I,5).EQ.0 )THEN
            NEXT=I
50          CONTINUE
            N=N+1
            IF(N.GT.4000) THEN 
              IF(LST(3).GE.1) WRITE(6,*) 'LSCI: N>4000!'
              LST(21)=101
              RETURN
            ENDIF
            THIS=NEXT
C--       copy to last row in event-record and update K-vector
            DO 60 J=1,5
               P(N,J)=P(THIS,J)
               V(N,J)=V(THIS,J)
               K(N,J)=K(THIS,J)
60          CONTINUE
            K(THIS,1)=13
            K(N,1)=2
            K(N,3)=THIS
            K(N,4)=0
            K(N,5)=0
C--       find next parton in string in row K(THIS,4)
            NEXT=K(THIS,4)
            IF (NEXT.NE.0) GOTO 50
C--       this is the last parton in string
            K(N,1)=1    
         ENDIF
40    CONTINUE
      DO 70 I=5,NS
C--       find first gluon string end
         IF (K(I,1).EQ.3 .AND. K(I,2).EQ.21) THEN
            INIT=I
            NEXT=I
80          CONTINUE
            N=N+1
            IF(N.GT.4000) THEN 
              IF(LST(3).GE.1) WRITE(6,*) 'LSCI: N>4000!'
              LST(21)=101
              RETURN
            ENDIF
            THIS=NEXT
C--       copy to last row in event-record and update K-vector
            DO 90 J=1,5
               P(N,J)=P(THIS,J)
               V(N,J)=V(THIS,J)
               K(N,J)=K(THIS,J)
90          CONTINUE
            K(THIS,1)=13
            K(N,1)=2
            K(N,3)=THIS
            K(N,4)=0
            K(N,5)=0
C--       find next parton in string in row K(THIS,4)
            NEXT=K(THIS,4)
            IF (NEXT.NE.INIT) GOTO 80
C--       this is the last parton in string
            K(N,1)=1    
         ENDIF
70    CONTINUE

      END

