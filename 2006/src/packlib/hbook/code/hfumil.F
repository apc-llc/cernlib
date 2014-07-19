*
* $Id: hfumil.F,v 1.1.1.1 1996/01/16 17:07:38 mclareni Exp $
*
* $Log: hfumil.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:38  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 19/04/93  10.27.31  by  Rene Brun
*-- Author :
      SUBROUTINE HFUMIL(TFUNC,S,M,IT,MC,Z,
     +Z0,G   ,A   ,DF   ,PL0   ,SIGMA   ,PL   ,R   ,DA,AMX,AMN,EXDA)
*.==========>
*.           HFUMIL IS SPECIAL VERSION OF THE FUMILI
*.           (D510 CERN LIBRARY)
*.           THIS VERSION ALLOWS TO USE DYNAMIC MEMORY MANAGEMENT
*.           AND FITTING FUNCTIONS AS FOLLOWS:
*.           IFLFUN=1-EXTERNAL FUNCTION TO BE  WRITTEN BY USER
*.                 =2-INTERNAL HBOOK FUNCTION I.E. HGAUSS HPOLY HEXP.
*.           (THESE FUNCTIONS ARE USED INSTEAD OF THE HARITH)
*..=========> ( I.Ivanchenko )
      DIMENSION Z(1),Z0(1),
     +G(1),A(1),DF(1),PL0(1),SIGMA(1),PL(1),R(1),DA(1),AMX(1),AMN(1),
     +EXDA(1)
#include "hbook/hcfit1.inc"
#include "hbook/hcfit2.inc"
#include "hbook/hcunit.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcrlf.inc"
*     10.*MAXIMUM RELATIVE PRECISION
*
      DATA RP/1.E-14/
*.___________________________________________
      N1=2
      N2=1
      N3=113
      EPS=0.1
      INDFLG(3)=0
      IF(IT.GE.0)WRITE(LOUT,84)CRLF, NAMFUN,ID
      NN2=0
      N=M
      FIXFLG=0.
      ENDFLG=0.
      INDFLG(2)=0
      IFIX1=0.
      FI=0.
      NN3=0
      DO 2 I=1,N
         R(I)=0.
         IF (EPS.GT.0.) SIGMA(I)=0.
         PL(I)=PL0(I)
   2  CONTINUE
*
*             START NEW ITERATION
*
   3  NN1=1
      T1=1.
*
*             REPEAT ITERATION WITH SMALLER STEP
*
   4  S=0.
      N0=0
      DO 7 I=1,N
         G(I)=0.
         IF (PL0(I).LE.0.)GO TO 7
         N0=N0+1
         IF (PL(I).GT.0.)THEN
            PL0(I)=PL(I)
         ENDIF
   7  CONTINUE
      NN0=N0*(N0+1)/2
      IF (NN0.LT.1) GO TO 9
      DO 8 I=1,NN0
         Z(I)=0.
   8  CONTINUE
   9  NA=M
      INDFLG(1)=0
*
*             CALCULATE OBJECTIVE FUNCTION
*
      CALL HSGZ (TFUNC,M,S,Z,G,A,DF,EXDA,PL0,AMX,AMN)
      SP=RP*ABS(S)
      IF (NN0.GE.1)THEN
         DO 10 I=1,NN0
            Z0(I)=Z(I)
  10     CONTINUE
      ENDIF
      IF (NN3.LE.0)GO TO 19
      IF (NN1-N1.GT.0)GO TO 19
      T=2.*(S-OLDS-GT)
      IF (INDFLG(1).NE.0)GO TO 16
      IF (ABS(S-OLDS).LE.SP.AND.-GT.LE.SP) GO TO 19
      IF (0.59*T+GT.LT.0.)GO TO 19
      T=-GT/T
      IF (T-0.25) 16,17,17
  16  T=0.25
  17  GT=GT*T
      T1=T1*T
      NN2=0
      DO 18 I=1,N
         IF (PL(I).LE.0.) GO TO 18
         A(I)=A(I)-DA(I)
         PL(I)=PL(I)*T
         DA(I)=DA(I)*T
         A(I)=A(I)+DA(I)
  18  CONTINUE
      NN1=NN1+1
      GO TO 4
*
*             REMOVE CONTRIBUTION OF FIXED PARAMETERS FROM Z
*
  19  IF (INDFLG(1).EQ.0) GO TO 20
      ENDFLG=-4.
      GO TO 85
  20  K1=1
      K2=1
      I1=1
      DO 30 I=1,N
         IF (PL0(I).LE.0.)GO TO 30
         IF (PL(I).EQ.0.) PL(I)=PL0(I)
         IF (PL(I)) 23,23,24
  22     PL(I)=0.
  23     K1=K1+I1
         GO TO 29
  24     IF (A(I).GE.AMX(I).AND.G(I).LT.0.) GO TO 22
         IF (A(I).LE.AMN(I).AND.G(I).GT.0.) GO TO 22
         DO 28 J=1,I
            IF (PL0(J).LE.0)GO TO 28
            IF (PL(J).GT.0.)THEN
               Z(K2)=Z0(K1)
               K2=K2+1
            ENDIF
            K1=K1+1
  28     CONTINUE
  29     I1=I1+1
  30  CONTINUE
*
*             INVERT Z
*
      I1=1
      L=I1
      DO 32 I=1,N
         IF (PL(I).LE.0.)GO TO 32
         R(I)=Z(L)
         I1=I1+1
         L=L+I1
  32  CONTINUE
      N0=I1-1
      CALL HMCONV (N0,Z,PL,R)
      IF (INDFLG(1).NE.0)THEN
         INDFLG(1)=0
         INDFLG(2)=1
         GO TO 49
      ENDIF
*
*              CALCULATE THEORETICAL STEP TO MINIMUM
      I1=1
      DO 41 I=1,N
         DA(I)=0.
         IF (PL(I).LE.0.)GO TO 41
         L1=1
         DO 40 L=1,N
            IF (PL(L).LE.0.)GO TO 40
            IF (I1-L1.LE.0)THEN
               K=L1*(L1-1)/2+I1
            ELSE
               K=I1*(I1-1)/2+L1
            ENDIF
            DA(I)=DA(I)-G(L)*Z(K)
            L1=L1+1
  40     CONTINUE
         I1=I1+1
  41  CONTINUE
*
*             CHECK FOR PARAMETERS ON BOUNDARY
*
      AFIX=0.
      IFIX=0
      I1=1
      L=I1
      DO 47 I=1,N
         IF (PL(I).LE.0.)GO TO 47
         SIGI=SQRT(ABS(Z(L)))
         R(I)=R(I)*Z(L)
         IF (EPS.GT.0.)THEN
            SIGMA(I)=SIGI
         ENDIF
         IF ((A(I).LT.AMX(I).OR.DA(I).LE.0.).AND.
     +    (A(I).GT.AMN(I).OR.DA(I).GE.0.)) GO TO 46
         AKAP=ABS(DA(I)/SIGI)
         IF (AKAP-AFIX.GT.0.)THEN
            AFIX=AKAP
            IFIX=I
            IFIX1=I
         ENDIF
  46     I1=I1+1
         L=L+I1
  47  CONTINUE
      IF (IFIX.EQ.0)GO TO 50
      PL(IFIX)=-1.
  49  FIXFLG=FIXFLG+1.
      FI=0.
*
*             REPEAT CALCULATION OF THEORETICAL STEP
*             AFTER FIXING EACH PARAMETER
*
      GO TO 19
*
*             CALCULATE STEP CORRECTION FACTOR
*
  50  ALAMBD=1.
      AKAPPA=0.
      IMAX=0
      DO 60 I=1,N
         IF (PL(I).LE.0.)GO TO 60
         BM=AMX(I)-A(I)
         ABI=A(I)+PL(I)
         ABM=AMX(I)
         IF (DA(I).LE.0.)THEN
            BM=A(I)-AMN(I)
            ABI=A(I)-PL(I)
            ABM=AMN(I)
         ENDIF
         BI=PL(I)
         IF (BI-BM.GT.0.)THEN
            BI=BM
            ABI=ABM
         ENDIF
         IF (ABS(DA(I))-BI.GT.0.)THEN
            AL=ABS(BI/DA(I))
            IF (ALAMBD-AL.GT.0.)THEN
               IMAX=I
               AIMAX=ABI
               ALAMBD=AL
            ENDIF
         ENDIF
         AKAP=ABS(DA(I)/SIGMA(I))
         IF (AKAP-AKAPPA.LE.0.)GO TO 60
         AKAPPA=AKAP
  60  CONTINUE
*
*             CALCULATE NEW CORRECTED STEP
*
      GT=0.
      AMB=1.E+18
      IF (ALAMBD.GT.0)THEN
         AMB=0.25/ALAMBD
      ENDIF
      DO 67 I=1,N
         IF (PL(I).LE.0.)GO TO 67
         IF (NN2-N2.GT.0)THEN
            IF (ABS(DA(I)/PL(I))-AMB.GE.0.)THEN
               PL(I)=4.*PL(I)
               T1=4.
            ENDIF
         ENDIF
         DA(I)=DA(I)*ALAMBD
         GT=GT+DA(I)*G(I)
  67  CONTINUE
*
*             CHECK IF MINIMUM ATTAINED AND SET EXIT MODE
*
      IF (-GT.GT.SP.OR.T1.GE.1..OR.ALAMBD.GE.1.) GO TO 68
      ENDFLG=-1.
  68  IF (ENDFLG.LT.0)GO TO 85
      IF (AKAPPA-ABS(EPS).GE.0.)GO TO 75
      IF (FIXFLG.EQ.0)THEN
         ENDFLG=1.
         GO TO 85
      ENDIF
      IF (ENDFLG) 85,77,73
  73  IF (IFIX1) 85,85,76
  74  IF (FI-FIXFLG) 76,76,77
  75  IF (FIXFLG.NE.0)GO TO 74
  76  FI=FI+1.
      ENDFLG=0.
  85  IF(ENDFLG.EQ.0..AND.NN3.GE.N3) ENDFLG=-3.
      IF(ENDFLG.GT.0..AND.INDFLG(2).GT.0) ENDFLG=-2.
      CALL HMONIT(S,M,NN3,IT,GT,AKAPPA,ALAMBD,A,SIGMA,R,PL,PL0)
      IF (ENDFLG) 83,79,83
*
*             CHECK IF FIXING ON BOUND IS CORRECT
*
 77   ENDFLG=1.
      FIXFLG=0.
      IFIX1=0
      DO 78 I=1,M
  78  PL(I)=PL0(I)
      INDFLG(2)=0
      GO TO 19
*
*             NEXT ITERATION
*
  79  ENDFLG=0.
      DO 80 I=1,N
         A(I)=A(I)+DA(I)
  80  CONTINUE
      IF (IMAX.GT.0)THEN
         A(IMAX)=AIMAX
      ENDIF
      OLDS=S
      NN2=NN2+1
      NN3=NN3+1
      GO TO 3
  83  MC=ENDFLG
*
  84  FORMAT(A,
     +/'     ********************************************************',
     +/'     *                                                      *',/
     +5X,'* FUNCTION MINIMIZATION BY SUBROUTINE HFIT',A2,'  ID=',I6,'*'
     +/'     *                                                      *',
     +/'     ********************************************************',
     +/'0     S     = VALUE OF OBJECTIVE FUNCTION                    ',
     +/'      2S    = CHISQUARE                                      ',
     +/'      EC    = EXPECTED CHANGE IN S DURING THE NEXT ITERATION ',
     +/'      KAPPA = ESTIMATED DISTANCE TO MINIMUM                  ',
     +/'      LAMBDA= STEP LENGTH MODIFIER                         '//)
      END
