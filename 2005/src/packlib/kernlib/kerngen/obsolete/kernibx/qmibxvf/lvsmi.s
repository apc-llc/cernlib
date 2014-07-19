*
* $Id: lvsmi.s,v 1.1.1.1 1996/02/15 17:54:31 mclareni Exp $
*
* $Log: lvsmi.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:31  mclareni
* Kernlib
*
*
LVSMI   CSECT
* CERN PROGLIB# F121    LVSMI           .VERSION KERNIBX  1.01  900525
* ORIG. 29/05/89  C.Guerin, IBM
* Modified for AIX, April 1990, Roger Howard
*
         USING *,15
         ENTRY _lvsmi_
_lvsmi_  STM   2,5,16(13)
*
         L     G3,0(,G2)     AIX: value of arg3
         L     G2,0(,G1)     AIX: value of arg2
         LR    G1,G0         AIX: address of arg1
         LTR   G2,G2         TEST IF LEN = 0
         BZ    ZERO          IF LEN = 0 GO TO ZERO
         LE    F0,KF         GET MAX POSITIVE
         LA    G0,1          GET 1
         SR    G4,G4         ZERO G4 FOR WORK
         SR    G5,G5         ZERO G5 FOR WORK
VLOOP    EQU   *
         VLVCU G2            SET LOOP
         VLE   V0,G1(G3)     LOAD V0 WITH A AND STRIDE
         VMNSE V0,F0,G4      FIND MINIMUM IN G6
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
KF       DC    X'7FFFFFFF'
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
#ifdef CERNLIB_TCGEN_LVSMI
#undef CERNLIB_TCGEN_LVSMI
#endif
