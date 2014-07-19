*
* $Id: jumpad.s,v 1.1.1.1 1996/02/15 17:54:44 mclareni Exp $
*
* $Log: jumpad.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:44  mclareni
* Kernlib
*
*
JUMPAD   CSECT
*
* CERN PROGLIB# Z042    JUMPAD          .VERSION KERNIBX  1.01  900524
* ORIG.  March 90, Roger Howard, Vancouver
*
*     INTEGER FUNCTION JUMPAD(TARGET)
*     Return address of routine TARGET
*
         ENTRY _jumpad_
_jumpad_ ST    2,16(,13)
         LR    2,0         Address of function argument
         L     0,0(,2)     Return value of argument
         L     2,16(,13)
         BCR   15,14
*
*     SUBROUTINE JUMPST(IAD)
*     Set the transfer address for calls to JUMPXn
*
         USING *,15
         ENTRY _jumpst_
_jumpst_ L     15,=A(DATA)
         ST    2,16(,13)
         LR    2,0         Address of argument
         L     2,0(,2)     Value of argument
         USING DATA,15
         ST    2,ROUTINE   Save argument in data segment
         L     2,16(,13)
         BCR   15,14
*
*     SUBROUTINE JUMPXn(args)
*     Execute transfer to routine whose address is set
*
*     In assembler the transfer can be made transparent by
*     simply changing the transfer address in GR15 and
*     branching to it; any arguments in GR0 through GR3 or
*     on the stack are then automatically arguments to the
*     transfer routine.
*
         USING *,15
         ENTRY _jumpx0_
         ENTRY _jumpx1_
         ENTRY _jumpx2_
_jumpx0_ DS    0H
_jumpx1_ DS    0H
_jumpx2_ L     15,=A(DATA)
         USING DATA,15
         L     15,ROUTINE   Retrieve the transfer address
         BCR   15,15        and branch to it
         LTORG
DATA     PSECT
ROUTINE  DS    F
         END
#ifdef CERNLIB_CCGEN_JUMPXN
#undef CERNLIB_CCGEN_JUMPXN
#endif
#ifdef CERNLIB_TCGEN_JUMPXN
#undef CERNLIB_TCGEN_JUMPXN
#endif
