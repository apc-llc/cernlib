*
* $Id: iulast.s,v 1.1.1.1 1996/02/15 17:53:36 mclareni Exp $
*
* $Log: iulast.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:36  mclareni
* Kernlib
*
*
        TITLE   IULAST
;#
; CERN PROGLIB# V305    IULAST          .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH HELP
        HELLO (IULAST)
       MOVEM   A01,AC01                 ;SAVE AC 1
       MOVEI      ,@1(A16)              ;INSERT ADR (IC)
       HRRM       ,LOOP
       MOVE    A01,@2(A16)              ;FETCH WORD COUNT N
       SOSGE      ,A01                  ;SKIP IF N .GT. 0
       JRST       ,KO
LOOP:  MOVE       ,(A01)                ;COMPARE IC WITH ISTUFF
       CAMN       ,@0(A16)
       SOJGE   A01,LOOP
       AOS        ,A01                  ;ADD 1 TO GET FORTRAN WORD COUNT
       MOVE       ,A01                  ;IULAST = J
       MOVE    A01,AC01                 ;RESTORE AC 1, EXIT
        GOODBY
KO:    SETZM                            ;ERROR EXIT
       MOVE    A01,AC01
        GOODBY
       A01= 1
       A16=16
AC01:  0
       PRGEND
#ifdef CERNLIB_TCGEN_IULAST
#undef CERNLIB_TCGEN_IULAST
#endif
