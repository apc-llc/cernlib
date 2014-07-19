*
* $Id: qnexte.s,v 1.1.1.1 1996/02/15 17:50:56 mclareni Exp $
*
* $Log: qnexte.s,v $
* Revision 1.1.1.1  1996/02/15 17:50:56  mclareni
* Kernlib
*
*
|--> IDENT: qnexte
| CERN PROGLIB# Z041    QNEXTE          .VERSION KERNALT  1.03  880701
| ORIG. 28/06/88  JZ
 
        .text
        .globl  _qnexte_
        .globl  _qnext_
_qnexte_:
        tstl    qn.jpoint       | test original stack-pointer set
        bne     qn.reent
 
 | First entry
        link    a6,#-8
        movl    a6,qn.jpoint    | save stack pointer on first entry
        bra     qn.godown
 
 | Re-entry
qn.reent:
        movl    qn.jpoint,a6    | get original stack pointer
 
qn.godown:
        unlk    a6
        lea     _qnext_,a1
        movl    #0, a0
 
        link    a6, #-8
        movl    a0, a6@(-4)
        jsr     _qnext_
        unlk a6
        rts
 
        .data
qn.jpoint:  .long  0
#ifdef CERNLIB_CCGEN_QNEXTE
#undef CERNLIB_CCGEN_QNEXTE
#endif
#ifdef CERNLIB_TCGEN_QNEXTE
#undef CERNLIB_TCGEN_QNEXTE
#endif
