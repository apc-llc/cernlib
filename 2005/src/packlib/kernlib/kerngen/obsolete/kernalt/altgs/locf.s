*
* $Id: locf.s,v 1.1.1.1 1996/02/15 17:50:55 mclareni Exp $
*
* $Log: locf.s,v $
* Revision 1.1.1.1  1996/02/15 17:50:55  mclareni
* Kernlib
*
*
|--> IDENT: locf
| CERN PROGLIB# N100    LOCF            .VERSION KERNALT  1.03  880701
| ORIG. 28/06/88  JZ
 
        .text
        .globl  _locf_
        | JAD = LOCF (VAR)              lword adr of VAR
_locf_:
        movl    a0@, d0         | d0   = lad   byte adr
        asrl    #2, d0          | ladr = lad / 4
        rts
#ifdef CERNLIB_CCGEN_LOCF
#undef CERNLIB_CCGEN_LOCF
#endif
#ifdef CERNLIB_TCGEN_LOCF
#undef CERNLIB_TCGEN_LOCF
#endif
#ifdef CERNLIB_ALTGS_LOCFF
#undef CERNLIB_ALTGS_LOCFF
#endif
