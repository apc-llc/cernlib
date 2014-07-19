*
* $Id: lvsdmx.s,v 1.1.1.1 1996/02/15 17:53:04 mclareni Exp $
*
* $Log: lvsdmx.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:04  mclareni
* Kernlib
*
*
LVSDMX   CSECT
* CERN PROGLIB# F121    LVSDMX          .VERSION KERNIBM  2.27  890622
* ORIG. 29/05/89  C.Guerin, IBM
*
#if defined(CERNLIB_QMIBMXA)
LVSDMX   AMODE ANY
LVSDMX   RMODE ANY
#endif
         USING *,15          FIND MAXIMUM IN SCATTERED VECTOR, DBL PR
         STM   14,05,12(13)
*
         LM    G1,G3,0(G1)   GET ADDRESSES
         L     G2,0(G2)      GET LEN
         LTR   G2,G2         TEST IF LEN = 0
         BZ    ZERO          IF LEN = 0 GO TO ZERO
         L     G3,0(G3)      GET STRIDE
         LD    F0,KF         GET MAX NEGATIVE
         LA    G0,1          GET 1
         SR    G4,G4         ZERO G4 FOR WORK
         SR    G5,G5         ZERO G5 FOR WORK
VLOOP    EQU   *
         VLVCU G2            SET LOOP
         VLD   V0,G1(G3)     LOAD V0 WITH A AND STRIDE
         VMXSD V0,F0,G4      FIND MAXIMUM IN G6
         BC    2,VLOOP       LOOP
         MR    G2,G4         MULT BY STRIDE
         AR    G0,G3         ADD 1
* END SUBROUTINE *
RETURN   EQU   *
         LM    2,05,28(13)   RETURN
         BR    14
ZERO     EQU   *
         SR    G0,G0
         B     RETURN
* CONSTANTS *
         DS    0D
KF       DC    X'FFFFFFFF'
         DC    X'FFFFFFFF'
V0       EQU   0
G0       EQU   0
G1       EQU   1
G2       EQU   2
G3       EQU   3
G4       EQU   4
G5       EQU   5
F0       EQU   0
         END
#ifdef CERNLIB_TCGEN_LVSDMX
#undef CERNLIB_TCGEN_LVSDMX
#endif
