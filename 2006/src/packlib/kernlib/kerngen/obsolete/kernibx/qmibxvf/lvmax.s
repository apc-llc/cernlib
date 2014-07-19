*
* $Id: lvmax.s,v 1.1.1.1 1996/02/15 17:54:31 mclareni Exp $
*
* $Log: lvmax.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:31  mclareni
* Kernlib
*
*
LVMAX   CSECT
* CERN PROGLIB# F121    LVMAX           .VERSION KERNIBX  1.01  900525
* ORIG. 29/05/89  C.Guerin, IBM
* Modified for AIX, April 1990, Roger Howard
*
         USING *,15
         ENTRY _lvmax_
_lvmax_  STM   2,5,16(13)
*
         LR    G4,G0         AIX: address of arg1
         L     G5,0(,G1)     AIX: value of arg2
         LTR   G5,G5         TEST IF LEN = 0
         BZ    ZERO          IF LEN = 0 GO TO ZERO
         LE    F0,KF         GET MAX NEGATIVE
         LA    G0,1          GET 1
         SR    G2,G2         ZERO G2 FOR WORK
         SR    G3,G3         ZERO G3 FOR WORK
VLOOP    EQU   *
         VLVCU G5            SET LOOP
         VLE   V0,G4         LOAD V0 WITH A VECTOR
         VMXSE V0,F0,G2      FIND MAXIMUM IN G2
         BC    2,VLOOP       LOOP
         AR    G0,G2         ADD 1
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
V0       EQU   0
G0       EQU   0
G1       EQU   1
G2       EQU   2
G3       EQU   3
G4       EQU   4
G5       EQU   5
F0       EQU   0
         END
#ifdef CERNLIB_TCGEN_LVMAX
#undef CERNLIB_TCGEN_LVMAX
#endif
