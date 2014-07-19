*
* $Id: isa_pjets.F,v 1.8 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: isa_pjets.F,v $
* Revision 1.8  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE ISA_PJETS(DRCUT,ETCUT,NPJ,PJ_PT,PJ_PHI,PJ_ETA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : COMBINES PARTONS INTO PARTON JETS
C-                         based on PJCONE
C-   Inputs
C-             DRCUT  - dR=sqrt(dETA**2+dPHI**2) cut around Leading Partons.
C-             ETCUT  - Transverse Energy cut ( minimum for defining a JET ).
C-
C-   Outputs : 
C-        NPJ        = No. of Parton Jets found.
C-        PJ_PT(NPJ) = pt of partons
C-        PJ_PHI(NPJ)= phi "
C-        PJ_ETA(NPJ)= eta "
C-        
C-   created  16-APR-1996   Serban D. Protopopescu   
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
C
#include "pjets.inc"
#include "jetset.inc"
C
      INTEGER NPJ
      REAL    DRCUT
      REAL    PJ_PHI(*), PJ_ETA(*)
      REAL    PJ_PT(*)
      INTEGER NP, JP, J, JO, JOP1, JOP2, JP1, JP2, ISKP, IP
      REAL    X1, Y1, PHI1, PHI2,TH
      REAL    DETA, DPHI, DR,  ETCUT
      INTEGER NPMAX
      PARAMETER (NPMAX=50)
      INTEGER JIORD(NPMAX), JDORD(NPMAX), JCNN(NPMAX,NPMAX)
      INTEGER JSKP(NPMAX)
      INTEGER I, JJ
      REAL    P_JIN(4,NPMAX), P_PHI(NPMAX), P_ETA(NPMAX)
      REAL    P_PT(NPMAX),PDM_PT(NPMAX)
      REAL    PJ(4,NPMAX)
      REAL    EPS
      DOUBLE PRECISION PI, TWOPI, HALFPI, RADIAN
C
C last significant (correctly rounded) decimal place on VAX:
C                                                |
C                                                V
        PARAMETER (PI=        3.1415 92653 58979 32384 6 D0)
        PARAMETER (TWOPI=     6.2831 85307 17958 64769 3 D0)
        PARAMETER (HALFPI=    1.5707 96326 79489 66192 3 D0)
        PARAMETER (RADIAN= 0.0174532 92519 94329 5769237 D0)

C
C
      PARAMETER( EPS = 1.0E-5 )
C----------------------------------------------------------------------
C
      NP=0
      DO I=1,NJSET
        IF(JDCAY(I).EQ.0.AND.IABS(JTYPE(I)).LT.10) THEN
          NP = NP + 1
          CALL UCOPY(PJSET(1,I),P_JIN(1,NP),4)
          P_PT(NP)  = SQRT( P_JIN(1,NP)**2+P_JIN(2,NP)**2 )
          P_PHI(NP) = ATAN2 (P_JIN(2,NP),P_JIN(1,NP)+EPS)
          IF(P_PHI(NP).LT.0.)P_PHI(NP)=P_PHI(NP)+TWOPI
          TH = ATAN2 (P_PT(NP),P_JIN(3,NP)+EPS)
          P_ETA(NP) = -ALOG ( ABS(TAN(TH/2.)) + EPS )
          IF(NP.GE.NPMAX) GOTO 35
        ENDIF
      ENDDO
   35 CONTINUE    ! jump here if more than NPMAX partons
C
C ... ORDER PARTONS IN PT
C
      DO 100 JP = 1 , NP
        JIORD(JP) = JP
  100 PDM_PT(JP)=P_PT(JP)
      CALL ISASRT(PDM_PT(1),NP,JIORD)
      DO 105 JP = 1 , NP
  105 JDORD(JP) = JIORD(NP-JP+1)
C
C ... COMBINE PARTONS CLOSE IN R SPACE
C
      DO 110 J = 1 , NP
        JO=JDORD(J)
  110 JCNN(JO,1)=0
      ISKP=0
      DO 120 JP1 = 1 , NP-1
        JOP1=JDORD(JP1)
C ... CHECK IF PARTON ALREADY CONNECTED TO OTHER ONE
        IF ( JCNN(JOP1,1).EQ.-1 ) GOTO 120
        DO 130 JP2 = JP1+1 , NP
          JOP2=JDORD(JP2)
C ... CHECK IF PARTON ALREADY CONNECTED TO OTHER ONE
          IF ( JCNN(JOP2,1).EQ.-1 ) GOTO 130
          DETA = P_ETA(JOP1) - P_ETA(JOP2)
          PHI1 = P_PHI(JOP1)
          PHI2 = P_PHI(JOP2)
          X1 = COS(PHI2-PHI1)
          Y1 = SIN(PHI2-PHI1)
          IF(X1.EQ.0.0) THEN
            DPHI = HALFPI
          ELSE
            DPHI = ATAN2(Y1,X1)
          END IF
          DR = SQRT(DETA**2+DPHI**2)
C --- CRITERION FOR COMBINING PARTONS
          IF ( DR.LT.DRCUT ) THEN
            JCNN(JOP1,1)=JCNN(JOP1,1)+1
            JCNN(JOP2,1)=-1
            JCNN(JOP1,JCNN(JOP1,1)+1)=JOP2
            ISKP=ISKP+JCNN(JOP1,1)
            JSKP(ISKP)=JOP2
          ELSE
            GOTO 130
          ENDIF
  130   CONTINUE
  120 CONTINUE
C
C ... BOOKKEEPING FOR PARTON JETS
C
      DO 150 IP = 1 , NPJ
        PJ_PHI(IP)=0.
        PJ_ETA(IP)=0.
        PJ_PT(IP) =0.
  150 CONTINUE
      NPJ=0
      DO 200 JP1 = 1 , NP
        JOP1=JDORD(JP1)
C ... ALREADY CONNECTED, SINGLE PARTON JET, OR HAS OTHERS TO CONNECT TO
        IF ( JCNN(JOP1,1).GE.0 ) THEN
          NPJ=NPJ+1
          CALL UCOPY(P_JIN(1,JOP1),PJ(1,NPJ),4)
          PJ_PHI(NPJ) = P_PHI(JOP1)
          PJ_ETA(NPJ) = P_ETA(JOP1)
          PJ_PT(NPJ) = P_PT(JOP1)
          IF ( JCNN(JOP1,1).EQ.0 ) GOTO 205
          DO 210 JJ = 1 , JCNN(JOP1,1)
            PJ(1,NPJ) = PJ(1,NPJ) + P_JIN(1,JCNN(JOP1,JJ+1))
            PJ(2,NPJ) = PJ(2,NPJ) + P_JIN(2,JCNN(JOP1,JJ+1))
            PJ(3,NPJ) = PJ(3,NPJ) + P_JIN(3,JCNN(JOP1,JJ+1))
            PJ(4,NPJ) = PJ(4,NPJ) + P_JIN(4,JCNN(JOP1,JJ+1))
  210     CONTINUE
          PJ_PT(NPJ) = SQRT( PJ(1,NPJ)**2 + PJ(2,NPJ)**2 )
          PJ_PHI(NPJ) = ATAN2 (PJ(2,NPJ),PJ(1,NPJ)+EPS)
          IF(PJ_PHI(NPJ).LT.0.)PJ_PHI(NPJ)=PJ_PHI(NPJ)+TWOPI
          TH = ATAN2 (PJ_PT(NPJ),PJ(3,NPJ)+EPS)
          PJ_ETA(NPJ) = -ALOG ( ABS(TAN(TH/2.)) + EPS )
C --- CRITERION FOR DROPPING A PARTON JET ( ET < ETCUT )
  205     IF ( PJ_PT(NPJ).GT.ETCUT ) GOTO 200
          NPJ=NPJ-1
        ENDIF
  200 CONTINUE
C
C ... ORDER PJETS IN PT
C
      DO 300 JP = 1 , NPJ
        JIORD(JP) = JP
  300 PDM_PT(JP)=PJ_PT(JP)
      CALL ISASRT(PDM_PT(1),NPJ,JIORD)
      DO 305 JP = 1 , NPJ
        P_PT(JP)=PJ_PT(JP)
        P_ETA(JP)=PJ_ETA(JP)
        P_PHI(JP)=PJ_PHI(JP)
  305 JDORD(JP) = JIORD(NPJ-JP+1)
      DO 306 JP = 1 , NPJ
        PJ_PT(JP)=P_PT(JDORD(JP))
        PJ_ETA(JP)=P_ETA(JDORD(JP))
        PJ_PHI(JP)=P_PHI(JDORD(JP))
  306 CONTINUE
C-
  999 RETURN
      END
