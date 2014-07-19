*
* $Id: ranecu.s,v 1.1.1.1 1996/02/15 17:53:05 mclareni Exp $
*
* $Log: ranecu.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:05  mclareni
* Kernlib
*
*
RANECU   CSECT
*
* CERN PROGLIB# V114    RANECU          .VERSION KERNIBM  2.34  930209
* ORIG. 29/05/89  C.Guerin, ****  IBM corr.18/12/92 MRO/CG
*
#if defined(CERNLIB_QMIBMXA)
RANECU   AMODE ANY
RANECU   RMODE ANY
#endif
         EXTRN RANEC1
#if defined(CERNLIB_SZ256)
SZ       EQU   256           SECTION SIZE EQUATE
#endif
#if !defined(CERNLIB_SZ256)
SZ       EQU   128           SECTION SIZE EQUATE
#endif
         USING *,15
         STM   14,12,12(13)  SAVE REG
****************************************
* SEE AT THE END FOR FORTRAN ALGORITHM *
****************************************
         L     G11,ARANEC1   ADCON FOR COMMON
         LA    G5,SZ         G5 = SECTION SIZE
         LM    G2,G4,0(G1)   GET ADDRESSES
         L     G3,0(G3)      GET LEN
         ST    G3,RESCNT     STORE TOTAL CNT = RES CNT
         L     G9,0(G4)      VALUE OF KSEQ
         LTR   G9,G9         IF(KESQ.GT.0)
         BC    12,ELSE       BRANCH IF NOT GT THAN ZERO
         ST    G9,0(G11)     JSEQ(1)=KSEQ
         B     SHIFT         ISEQ IN G9
*        B     ENDIF
ELSE     L     G9,0(G11)     ISEQ=JSEQ(1)
SHIFT    SLL   G9,4          correct. 18/12/92 SLL G9,3 to SLL G9,4
         S     G9,K8         added    18/12/92
         AR    G9,G11        (1,ISEQ) ADDR IN G9
         LD    F4,0(G9)      F4 = ISEED1 = (ISEED(1,ISEQ)
         LD    F6,8(G9)      F6 = ISEED2 = (ISEED(2,ISEQ)
         LA    G12,8         SET G12 = 8
         LR    G4,G3         SET G4 FOR VLVCU
* INIT ** LOOP   ***** DO 100 I=1,LEN
INIT     EQU   *             BEGIN LOOPS
         SR    G11,G11       ZERO INDEX
         L     G3,RESCNT     GET RESID CNT
         SR    G3,G5         SUBTRACT SIZE
         BM    LESS          IF MINUS LESS THAN SECT SIZE (LAST)
         ST    G3,RESCNT     SAVE RES CNT
         LR    G3,G5         IF GT SET TO SECT SIZE
* LOOP ** ISEED1 ***** DO 100 I=1,LEN
SCALARP  EQU   *
         LD    F0,KC1        KC1 = 1/53668  K=ISEED1/53668
         MDR   F0,F4         F4  = ISEED1
         AD    F0,K4F08      CONVERT TO
         LE    F0,D4E        INTEGER THEN
         SDR   F2,F2         TO FLOATING
         ADR   F2,F0         POINT AGAIN
         LDR   F0,F2         COPY F2/F0
         MD    F0,K44D4M     MULT BY 53668 NEG   -K*53668 IN F0
         ADR   F0,F4         ADD ISEED1(I-1)     +ISEED1  IN F0
         MD    F0,K449C      MULT BY 40014       40014*(ISEED1-K*53668)
         MD    F2,K442F      MULT BY 12211        K*12211
         SDR   F0,F2         F0=ISEED1=40014*(ISEED1-K*53668)-K*12211
         BP    EXITA         IF GT ZERO STORE
         AD    F0,K487F      IF LT ZERO ADD 2147483563
EXITA    EQU   *
         STD   F0,ISEED1(G11)     ISEED1 IN F0
         LDR   F4,F0              COPY IN F4
* SCALAR PART ** ISEED2 *****
         LD    F0,KC2        KC2 = 1/52774  K=ISEED2/52774
         MDR   F0,F6         F6  = ISEED2
         AD    F0,K4F08      CONVERT TO
         LE    F0,D4E        INTEGER THEN
         SDR   F2,F2         TO FLOATING
         ADR   F2,F0         POINT AGAIN
         LDR   F0,F2         COPY F2/F0
         MD    F0,K44CEM     MULT BY 52774 NEG   -K*52774 IN F0
         ADR   F0,F6         ADD ISEED2(I-1)     +ISEED2  IN F0
         MD    F0,K449E      MULT BY 40692       40692*(ISEED2-K*52774)
         MD    F2,K43EC      MULT BY 3791         K*3791
         SDR   F0,F2         F0=ISEED2=40692*(ISEED2-K*52774)-K*3791
         BP    EXITB         IF GT ZERO STORE
         AD    F0,K487FB     IF LT ZERO ADD 2147483399
EXITB    EQU   *
         STD   F0,ISEED2(G11)     ISEED2 IN F0
         LDR   F6,F0         COPY IN F6
****     LA    G11,8(G11)    SET ISEED INDEX +8
         AR    G11,G12
         BCT   G3,SCALARP    LOOP
* VECTOR LOOP ****************
         LM    G6,G7,AISEED1 GET VECT ADDR ISEED1 ISEED2
         LD    F0,KF1        F0 = 1.0
         LD    F2,K4656      F2 = 4.6566128 E-10
         VLVCU G4
         VLD   V0,G6         V0 = ISEED1 ADDR IN G6
         VSD   V0,V0,G7      V0 = ISEED1-ISEED2 ISEED2 ADDR IN G7
         VCDQ  2,F0,V0       V0 = IZ .LT. F0 (F0 = 1.0)
         LD    F0,K21562     F0 = 2147485362
         VSVMM 1             IF IZ .LT. 1.
         VADQ  V0,F0,V0      IZ = IZ + 2147485362
         VSVMM 0
         VMDQ  V0,F2,V0      MULT IZ * 4.6566128 E-10 (F2)
         VSTE  V0,G2         STORE IN RVEC(I)
         LTR   G4,G4
         BC    2,INIT
* 100 CONTINUE **************
**       STM   G0,G1,0(G9)   ISEED(1,ISEQ)=ISEED1;ISEED(2,ISEQ)=ISEED2
         STD   F4,0(G9)
         STD   F6,8(G9)
* RETURN *********************
         LM    2,12,28(13)   *
         BR    14            *
* END SUBROUTINE *************
LESS     EQU   *             LAST PASS SCALAR THEN VECTOR
         AR    G3,G5         RESET COUNT
         C     G3,K30        LAST VECTOR LONGER THAN 30
         BH    SCALARP       GO BACK SCAL THEN VECT
ALLSCA   EQU   *             IF LESS GO ALL SCALAR
         LD    F0,KC1        KC1 = 1/53668  K=ISEED1/53668 MODIF
         MDR   F0,F4         F4  = ISEED1
         AD    F0,K4F08      CONVERT TO
         LE    F0,D4E        INTEGER THEN
         SDR   F2,F2         TO FLOATING
         ADR   F2,F0         POINT AGAIN
         LDR   F0,F2         COPY F2/F0
         MD    F0,K44D4M     MULT BY 53668 NEG   -K*53668 IN F0
         ADR   F0,F4         ADD ISEED1(I-1)     +ISEED1  IN F0
         MD    F0,K449C      MULT BY 40014       40014*(ISEED1-K*53668)
         MD    F2,K442F      MULT BY 12211        K*12211
         SDR   F0,F2         F0=ISEED1=40014*(ISEED1-K*53668)-K*12211
         BP    EXIT3         IF GT ZERO STORE
         AD    F0,K487F      IF LT ZERO ADD 2147483563
EXIT3    EQU   *
         LDR   F4,F0              COPY IN F4
* SCALAR PART ** ISEED2 *****
         LD    F0,KC2        KC2 = 1/52774  K=ISEED2/52774 MODIF
         MDR   F0,F6         F6  = ISEED2
         AD    F0,K4F08      CONVERT TO
         LE    F0,D4E        INTEGER THEN
         SDR   F2,F2         TO FLOATING
         ADR   F2,F0         POINT AGAIN
         LDR   F0,F2         COPY F2/F0
         MD    F0,K44CEM     MULT BY 52774 NEG   -K*52774 IN F0
         ADR   F0,F6         ADD ISEED2(I-1)     +ISEED2  IN F0
         MD    F0,K449E      MULT BY 40692       40692*(ISEED2-K*52774)
         MD    F2,K43EC      MULT BY 3791         K*3791
         SDR   F0,F2         F0=ISEED2=40692*(ISEED2-K*52774)-K*3791
         BP    EXITB3        IF GT ZERO STORE
         AD    F0,K487FB     IF LT ZERO ADD 2147483399
EXITB3   EQU   *
         LDR   F6,F0              COPY IN F6
* SUITE SCALAIRE ************
         LDR   F0,F4         FO = ISEED1  F4 = ISEED2
         SDR   F0,F6         FO = IZ = ISEED1 - ISEED2
         CD    F0,KF1        IF IZ(I) .GE.1 GO TO MULT
         BNL   MULT          IF IZ(I) .LT.1 IZ(I)=IZ(I)+2147483562
         AD    F0,K21562
MULT     EQU   *
         MD    F0,K4656      IZ(I)=IZ(I)*4.6566128 E -10
         STE   F0,0(G2)      STORE IN RVEC(I)
         LA    G2,4(G2)      SET RVEC INDEX +4
         BCT   G3,ALLSCA     LOOP SCALAIRE
* 100 CONTINUE **************
**       STM   G0,G1,0(G9)   ISEED(1,ISEQ)=ISEED1;ISEED(2,ISEQ)=ISEED2
         STD   F4,0(G9)
         STD   F6,8(G9)
* RETURN *********************
         LM    2,12,28(13)   *
         BR    14            *
* END SUBROUTINE *************
* CONSTANTS *****************
K8       DC    F'8'
         DS    0D
DB       DC    D'0'
D4E      DC    X'4E000000'
         DC    X'00000000'
KC1      DC    X'3D1389C7'   KC1    = 1./53668 OLD VERSION
         DC    X'54E4331C'   KC1    = 1.D0+1.D-11/53668  NEW
K4F08    DC    X'4F080000'
         DC    X'00000000'
K44D4M   DC    X'C4D1A400'   K44D4M = - 53668.
         DC    X'00000000'
K449C    DC    X'449C4E00'   K449C  =   40014.
         DC    X'00000000'
K442F    DC    X'442FB300'   K442F  =   12211.
         DC    X'00000000'
ISEED1   DC    (SZ)D'0'
K487F    DC    X'487FFFFF'   K487F  =   2147483563.
         DC    X'AB000000'
KC2      DC    X'3D13DE82'   KC2    =   1./52774.     OLD VERSION
         DC    X'731FCFC7'   KC2    =   1.D0+1.D-11/52774  NEW
K44CEM   DC    X'C4CE2600'   K44CEM = - 52774.
         DC    X'00000000'
K449E    DC    X'449EF400'   K449E  =   40692.
         DC    X'00000000'
K43EC    DC    X'43ECF000'   K43EC  =   3791.
         DC    X'00000000'
K487FB   DC    X'487FFFFF'   K487FB =   2147483399.
         DC    X'07000000'
KF1      DC    X'41100000'   KF1    =   1.
         DC    X'00000000'
K21562   DC    X'487FFFFF'   K21562 =   2147485362.
         DC    X'00000000'
K4656    DC    X'39200000'   K4656  =   4.6566128 E-10
         DC    X'00000000'
ARANEC1  DC    A(RANEC1)
AISEED1  DC    A(ISEED1)
AISEED2  DC    A(ISEED2)
K30      DC    F'30'
RESCNT   DC    F'0'
ISEED2   DC    (SZ)D'0'
*********************************************************************
*******  ORIGINAL FORTRAN ALGORITHM *********************************
*********************************************************************
***   SUBROUTINE RANECU(RVEC,LEN,ISEQ)
* COMMENT OUT PRINT OFF STATEMENT TO GET FORTRAN PROGRAM ************
*        PRINT OFF
***   DIMENSION RVEC(*)
***   COMMON / ECUSED / ISEED(2,1)
*C
***   ISEED1 = ISEED(1,ISEQ)
***   ISEED2 = ISEED(2,ISEQ)
***   DO 100 I= 1, LEN
***     K = ISEED1/53668
***     ISEED1 = 40014*(ISEED1 - K*53668) - K*12211
***     IF (ISEED1 .LT. 0) ISEED1=ISEED1+2147483563
*C
***     K = ISEED2/52774
***     ISEED2 = 40692*(ISEED2 - K*52774) - K* 3791
***     IF (ISEED2 .LT. 0) ISEED2=ISEED2+2147483399
***
***     IZ = ISEED1 - ISEED2
***     IF (IZ .LT. 1)  IZ = IZ + 2147483562
*C
***     RVEC(I) = IZ * 4.6566128E-10
* 100 CONTINUE
***   ISEED(1,ISEQ) = ISEED1
***   ISEED(2,ISEQ) = ISEED2
***   RETURN
*C
***   ENTRY RECUIN(IS1,IS2,ISEQ)
***   ISEED(1,ISEQ) = IS1
***   ISEED(2,ISEQ) = IS2
***   ENTRY RECUUT(IS1,IS2,ISEQ)
***   IS1    = ISEED(1,ISEQ)
***   IS2    = ISEED(2,ISEQ)
***   END
***************** SECOND ALGORITHM ****************
***   SUBROUTINE RANECF(RVEC,LEN,KSEQ)
*
***   DOUBLE PRECISION ISEED, ISEED1, ISEED2, K, C1, C2, IZ
***   PARAMETER (C1=(1.D0+1.D-11)/53668.D0,C2=(1.D0+1.D-11)/52774.D0)
***   PARAMETER  (MAXSEQ=100)
***   DIMENSION RVEC(*)
***   COMMON / RANEC1 / JSEQ(2), ISEED(2,MAXSEQ)
***   SAVE /RANEC1/
*C
***   IF(KSEQ.GT.0) THEN
***     JSEQ(1) = KSEQ
***     ISEQ    = KSEQ
***   ELSE
***     ISEQ   = JSEQ(1)
***   ENDIF
***   ISEED1 = ISEED(1,ISEQ)
***   ISEED2 = ISEED(2,ISEQ)
***   DO 100 I= 1, LEN
***     K = INT (ISEED1*C1)
***     ISEED1 = 40014*(ISEED1 - K*53668) - K*12211
***     IF (ISEED1 .LT. 0) ISEED1=ISEED1+2147483563
*C
***     K = INT(ISEED2*C2)
***     ISEED2 = 40692*(ISEED2 - K*52774) - K* 3791
***     IF (ISEED2 .LT. 0) ISEED2=ISEED2+2147483399
*C
***     IZ = ISEED1 - ISEED2
***     IF (IZ .LE. 0)  IZ = IZ + 2147483562
*C
***     RVEC(I) = IZ * 4.6566128E-10
* 100 CONTINUE
***   ISEED(1,ISEQ) = ISEED1
***   ISEED(2,ISEQ) = ISEED2
***   END
G0       EQU   0
G1       EQU   1
G2       EQU   2
G3       EQU   3
G4       EQU   4
G5       EQU   5
G6       EQU   6
G7       EQU   7
G8       EQU   8
G9       EQU   9
G10      EQU   10
G11      EQU   11
G12      EQU   12
G13      EQU   13
G14      EQU   14
G15      EQU   15
F0       EQU   0
F2       EQU   2
F4       EQU   4
F6       EQU   6
V0       EQU   0
V1       EQU   1
V2       EQU   2
V3       EQU   3
V4       EQU   4
V5       EQU   5
V6       EQU   6
V7       EQU   7
V8       EQU   8
V9       EQU   9
V10      EQU   10
V11      EQU   11
V12      EQU   12
V13      EQU   13
V14      EQU   14
V15      EQU   15
         END
#ifdef CERNLIB_TCGEN_RANECU
#undef CERNLIB_TCGEN_RANECU
#endif
