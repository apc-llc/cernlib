*
* $Id: vmod.s,v 1.1.1.1 1996/02/15 17:53:40 mclareni Exp $
*
* $Log: vmod.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:40  mclareni
* Kernlib
*
*
        TITLE   VMOD
;#
; CERN PROGLIB# F121    VMOD            .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH HELP
       EXTERN  SQRT
        HELLO (VMOD)
       SETZM
       SKIPG      ,@1(A16)
        GOODBY
       MOVEM   A01,AC01
       MOVEM   A02,AC02
       MOVEI   A01,@0(A16)
       SOS        ,A01
       HRRM    A01,LOOP
       MOVEI   A01,1
LOOP:  MOVE    A02,(A01)
       FMPR    A02,A02
       FADR       ,A02
       CAMGE   A01,@1(A16)
       AOJA    A01,LOOP
       MOVEM      ,TEMP
               ENTER (SQRT)
       JUMP      2,TEMP
       MOVE    A01,AC01
       MOVE    A02,AC02
        GOODBY
AC01:  0
AC02:  0
TEMP:  0
       A01= 1
       A02= 2
       A16=16
      PRGEND
#ifdef CERNLIB_TCGEN_VMOD
#undef CERNLIB_TCGEN_VMOD
#endif
