*
* $Id: lvsimx.s,v 1.1.1.1 1996/02/15 17:53:04 mclareni Exp $
*
* $Log: lvsimx.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:04  mclareni
* Kernlib
*
*
LVSIMX   CSECT
* CERN PROGLIB# F121    LVSIMX          .VERSION KERNIBM  2.27  890622
* ORIG. 29/05/89  C.Guerin, IBM
*
#if defined(CERNLIB_QMIBMXA)
LVSIMX   AMODE ANY
LVSIMX   RMODE ANY
#endif
         USING *,15          FIND MAXIMUM IN SCATTERED VECTOR, INTEG
         STM   14,05,12(13)
*
         LM    G1,G3,0(G1)   GET ADDRESSES
         L     G2,0(G2)      GET LEN
         LTR   G2,G2         TEST IF LEN = 0
         BZ    ZERO          IF LEN = 0 GO TO ZERO
         L     G3,0(G3)      GET STRIDE
         LE    F2,KF         GET MAX NEGATIVE
         LA    G0,1          GET 1
         SR    G4,G4         ZERO G4 FOR WORK
         SR    G5,G5         ZERO G5 FOR WORK
         LD    F0,C          GET C FOR CONVERSION
VLOOP    EQU   *
         VLVCU G2            SET LOOP
         VLDQ  V0,F0         SET V0 WITH C
         VX    V1,V1,G1(G3)  EXCL OR IN V1 WITH A AND STRIDE
         VSDQ  V0,F0,V0      SUB C IN V0/V1 CONVERT I TO F
         VMXSD V0,F2,G4      FIND MAXIMUM IN G6
         BC    2,VLOOP       LOOP
         MR    G2,G4         MULT BY STRIDE
         AR    G0,G3         ADD 1
* END SUBROUTINE *
RETURN   EQU   *
         LM    2,05,28(13)
         BR    14
ZERO     EQU   *
         SR    G0,G0
         B     RETURN
* CONSTANTS *
         DS    0D
KF       DC    X'FFFFFFFF'
         DC    X'FFFFFFFF'
C        DC    X'CE000000'
         DC    X'80000000'
V0       EQU   0
V1       EQU   1
G0       EQU   0
G1       EQU   1
G2       EQU   2
G3       EQU   3
G4       EQU   4
G5       EQU   5
F0       EQU   0
F2       EQU   2
         END
#ifdef CERNLIB_TCGEN_LVSIMX
#undef CERNLIB_TCGEN_LVSIMX
#endif
