*
* $Id: qnexte.s,v 1.1.1.1 1996/02/15 17:54:46 mclareni Exp $
*
* $Log: qnexte.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:46  mclareni
* Kernlib
*
*
QNEXTE   CSECT
*
* CERN PROGLIB# Z041    QNEXTE          .VERSION KERNIBX  1.01  900524
*
* FUNCTION:     ON FIRST ENTRY, STANDARD REGISTER SAVING AND STACK
*               CHAINING ARE PERFORMED AND ROUTINE QNEXT IS CALLED.
*               ON RE-ENTRY, QNEXT IS CALLED AFTER THE ORIGINAL
*               STACK FRAME HAS BEEN RESTORED. WHEN CONTROL IS
*               RETURNED BY QNEXT, A STANDARD RETURN TO MAIN IS MADE.
*               AT ALL TIMES IT THEREFORE APPEARS, (E.G. IN A FORTRAN
*               TRACEBACK), AS THOUGH THE CHAIN OF CALLS WAS MAIN,
*               QNEXTE, QNEXT ... , AND A CHAIN OF RETURNS LEADS BACK
*               TO MAIN.
*
* CALLING SEQUENCE:     CALL QNEXTE
*
* ORIGINAL VERSION : R. MATTHEWS, CERN/DD, OCTOBER 1981.
*
         USING *,15
         ENTRY _qnexte_
_qnexte_ STM   2,1,16(13)
         LR    4,13               caller's stack pointer
         S     13,=F'144'         set up stack frame
         ST    4,4(0,13)          chain to previous frame
         BALR  12,0               GR12 is base register
         DROP  15
         USING *,12
         L     11,=A(DAREA)       GR11 addresses data
         USING DAREA,11
         CLI   EFLAG,1            first entry?
         BNE   CALL               no: branch
         ST    4,ANCHOR           save original caller's stack frame
         MVI   EFLAG,0            turn off first entry flag
CALL     L     0,=F'-1'           no subroutine arguments
         L     15,=V(_qnext_)     GR15 -> QNEXT
         BALR  14,15              branch to QNEXT
         L     13,ANCHOR          restore original caller's stack frame
         LM    2,1,16(13)         ... then restore registers
         BR    14                 ... and return
         LTORG                    literals must be in CSECT
DAREA    PSECT                    data segment
ANCHOR   DC    F'0'               stack frame address
EFLAG    DC    X'01'              first entry flag
         END
#ifdef CERNLIB_CCGEN_QNEXTE
#undef CERNLIB_CCGEN_QNEXTE
#endif
#ifdef CERNLIB_TCGEN_QNEXTE
#undef CERNLIB_TCGEN_QNEXTE
#endif
