*
* $Id: jumpxn.s,v 1.1.1.1 1996/02/15 17:50:56 mclareni Exp $
*
* $Log: jumpxn.s,v $
* Revision 1.1.1.1  1996/02/15 17:50:56  mclareni
* Kernlib
*
*
|--> IDENT: jumpxn
| CERN PROGLIB# Z042    JUMPXN          .VERSION KERNALT  1.08  891207
| ORIG. 28/06/88 JZ
| MOD.  12/09/89 JZ, add entries JUMPX1, JUMPX0
 
        .text
        .globl  _jumpxn_, _jumpx1_, _jumpx2_
        | CALL JUMPXN (PAR1,PAR2,...)   transfer
_jumpxn_:
_jumpx0_:
_jumpx1_:
_jumpx2_:
        movl    jxq.jpoint, a1  | a1 = jump address
        jmp     a1@             | jump
 
        .globl  _jumpst_
        | CALL JUMPST (JTRANS)          set transfer address
_jumpst_:
        movl    a0@, a1         | a1 = adr(JTRANS)
        movl    a1@, jxq.jpoint | jpoint = JTRANS
        rts
 
        .globl  _jumpad_
        | JTRANS = JUMPAD (TARGET)      get transfer address
_jumpad_:
        movl    a0@, d0         | value = adr(TARGET)
        rts
 
        .data
jxq.jpoint: .long   0
#ifdef CERNLIB_CCGEN_JUMPXN
#undef CERNLIB_CCGEN_JUMPXN
#endif
#ifdef CERNLIB_TCGEN_JUMPXN
#undef CERNLIB_TCGEN_JUMPXN
#endif
