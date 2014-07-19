*
* $Id: jbit.s,v 1.1.1.1 1996/02/15 17:53:42 mclareni Exp $
*
* $Log: jbit.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:42  mclareni
* Kernlib
*
*
        TITLE JBIT (X,LX)
;#
; CERN PROGLIB# M421    JBIT            .VERSION KERNPDP  1.00  750903
;#
        WORD=0
        LSHIFT=1
        SEARCH HELP
#include "reentr.inc"
        HELLO( JBIT)
        MOVN    LSHIFT,@1(16)
        MOVE    WORD,@(16)
        LSH     WORD,1(LSHIFT)
        ANDI    WORD,1
        GOODBY
        PRGEND
#ifdef CERNLIB_TCGEN_JBIT
#undef CERNLIB_TCGEN_JBIT
#endif
