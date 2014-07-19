*
* $Id: vunit.s,v 1.1.1.1 1996/02/15 17:53:41 mclareni Exp $
*
* $Log: vunit.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:41  mclareni
* Kernlib
*
*
        TITLE   VUNIT
;#
; CERN PROGLIB# F121    VUNIT           .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH HELP
       EXTERN  SQRT
        HELLO (VUNIT)
       SKIPG      ,@2(A16)
        GOODBY
       SETZM
       MOVEM   A01,AC01
       MOVEI   A01,@0(A16)
       SOS        ,A01
       HRRM    A01,LOOP
       HRRM    A01,LOOP1
       MOVEI   A01,@1(A16)
       SOS        ,A01
       HRRM    A01,LOOP1+2
       MOVEI   A01,1
       MOVEM   A02,AC02
LOOP:  MOVE    A02,(A01)
       FMPR    A02,A02
       FADR       ,A02
       CAMGE   A01,@2(A16)
       AOJA    A01,LOOP
       PAGE
       JUMPLE     ,EXIT
       MOVEM      ,TEMP
               ENTER (SQRT)
       JUMP      2,TEMP
       MOVE    A02,EINS
       FDVR    A02,
       MOVEI   A01,1
LOOP1: MOVE       ,(A01)
       FMPR       ,A02
       MOVEM      ,(A01)
       CAMGE   A01,@2(A16)
       AOJA    A01,LOOP1
EXIT:  MOVE    A01,AC01
       MOVE    A02,AC02
        GOODBY
EINS:  EXP     ^D1.0
AC01:  0
AC02:  0
TEMP:  0
       A01= 1
       A02= 2
       A16=16
        PRGEND
#ifdef CERNLIB_TCGEN_VUNIT
#undef CERNLIB_TCGEN_VUNIT
#endif
