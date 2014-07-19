*
* $Id: lvsimx.s,v 1.1.1.1 1996/02/15 17:54:32 mclareni Exp $
*
* $Log: lvsimx.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:32  mclareni
* Kernlib
*
*
LVSIMX   CSECT
* CERN PROGLIB# F121    LVSIMX          .VERSION KERNIBX  1.01  900525
* ORIG. 29/05/89  C.Guerin, IBM
* Modified for AIX, April 1990, Roger Howard
*
         USING *,15
         ENTRY _lvsimx_
_lvsimx_ STM   2,5,16(13)
*
         L     G3,0(,G2)     AIX: value of arg3
         L     G2,0(,G1)     AIX: value of arg2
         LR    G1,G0         AIX: address of arg1
         LTR   G2,G2         TEST IF LEN = 0
         BZ    ZERO          IF LEN = 0 GO TO ZERO
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
         LM    2,5,16(13)
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
