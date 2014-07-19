*
* $Id: vexcum.s,v 1.1.1.1 1996/02/15 17:53:39 mclareni Exp $
*
* $Log: vexcum.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:39  mclareni
* Kernlib
*
*
        TITLE   VEXCUM
;#
; CERN PROGLIB# F121    VEXCUM          .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH HELP
        HELLO (VEXCUM)
       SKIPG      ,@2(A16)
        GOODBY
       MOVEM   A01,AC01
       MOVEM   A02,AC02
       MOVEI      ,@0(A16)
       SOS
       HRRM       ,LOOP
       MOVEI   A01,@1(A16)
       MOVEI  A02,1
LOOP:  MOVE       ,(A02)
       CAMGE      ,(A01)
       MOVEM      ,(A01)
       CAMLE      ,1(A01)
       MOVEM      ,1(A01)
       FADRM      ,2(A01)
       CAMGE   A02,@2(A16)
       AOJA    A02,LOOP
       MOVE    A01,AC01
       MOVE    A02,AC02
        GOODBY
AC01:  0
AC02:  0
       A01= 1
       A02= 2
       A16=16
      PRGEND
#ifdef CERNLIB_TCGEN_VEXCUM
#undef CERNLIB_TCGEN_VEXCUM
#endif
