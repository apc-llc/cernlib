*
* $Id: lvmaxa.s,v 1.1.1.1 1996/02/15 17:53:04 mclareni Exp $
*
* $Log: lvmaxa.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:04  mclareni
* Kernlib
*
*
LVMAXA   CSECT
* CERN PROGLIB# F121    LVMAXA          .VERSION KERNIBM  2.27  890622
* ORIG. 29/05/89  C.Guerin, IBM
*
#if defined(CERNLIB_QMIBMXA)
LVMAXA   AMODE ANY
LVMAXA   RMODE ANY
#endif
         USING *,15
         STM   14,05,12(13)
*
         LM    G4,G5,0(G1)   GET ADDRESSES
         L     G5,0(G5)      GET LEN
         LTR   G5,G5         TEST IF LEN = 0
         BZ    ZERO          IF LEN = 0 GO TO ZERO
         LE    F0,KF         GET MAX NEGATIVE
         LA    G0,1          GET 1
         SR    G2,G2         ZERO G2 FOR WORK
         SR    G3,G3         ZERO G3 FOR WORK
VLOOP    EQU   *
         VLVCU G5            SET LOOP
         VLE   V0,G4         LOAD V0 WITH A VECTOR
         VMXAE V0,F0,G2      FIND MAXIMUM ABSOLUTE IN G2
         BC    2,VLOOP       LOOP
         AR    G0,G2         ADD 1
* END SUBROUTINE *
RETURN   EQU   *
         LM    2,05,28(13)   RETURN
         BR    14
ZERO     EQU   *
         SR    G0,G0
         B     RETURN
* CONSTANTS *
         DS    0D
KF       DC    X'00000000'
V0       EQU   0
G0       EQU   0
G1       EQU   1
G2       EQU   2
G3       EQU   3
G4       EQU   4
G5       EQU   5
F0       EQU   0
         END
#ifdef CERNLIB_TCGEN_LVMAXA
#undef CERNLIB_TCGEN_LVMAXA
#endif
