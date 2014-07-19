*
* $Id: noarg.s,v 1.1.1.1 1996/02/15 17:52:19 mclareni Exp $
*
* $Log: noarg.s,v $
* Revision 1.1.1.1  1996/02/15 17:52:19  mclareni
* Kernlib
*
*
;     SUBROUTINE NOARG (NARGS)
;
; CERN PROGLIB# Z029    NOARG           .VERSION KERNCVX  1.10  940228
; ORIG. 01/01/89  JV, CONVEX
;
#if !defined(CERNLIB_QIEEE)
         .fpmode native
#endif
#if defined(CERNLIB_QIEEE)
         .fpmode ieee
#endif

; INITIALIZED DATA

        .data
        .align  8
LI:

; UNINITIALIZED DATA

        .bss
        .align  8
LU:

; INSTRUCTIONS

        .text
        ds.w    0x4010000
        ds.b    "-O\0\0"
        .globl  _noarg_ ;ENTRY
_noarg_:
        sub.w   #Lfs1,sp        ;
        ld.w    8(fp),a1        ;get the previous frame pointer
        ld.w    12(a1),a1       ;get the previous argument pointer
        ld.w    -4(a1),s0       ;get the number of arguments
        mov     s0,s1
        mul.w   #4,s1           ;
loop:
        sub.w   #4,s1
        mov     a1,a2
        add.w   s1,a2
        ld.w    (a2),s2
        leu.w   #0x80000000,s2
        jmps.t   exit
        sub.w     #1,s0
        jmp    loop
exit:
        cvtw.l  s0,s0
        st.l    s0,@0(ap)       ;
        rtn     ; #4
Lfs1 = 8
#ifdef CERNLIB_TCGEN_NOARG
#undef CERNLIB_TCGEN_NOARG
#endif
