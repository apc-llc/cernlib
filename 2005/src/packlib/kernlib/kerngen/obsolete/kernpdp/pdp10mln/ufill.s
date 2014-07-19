*
* $Id: ufill.s,v 1.1.1.1 1996/02/15 17:53:37 mclareni Exp $
*
* $Log: ufill.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:37  mclareni
* Kernlib
*
*
        TITLE   UFILL
;#
; CERN PROGLIB# V300    UFILL           .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH HELP
        HELLO (UFILL)
       HRRZI      ,@0(A16)              ;SET UP LAST ADR FOR BLT
       ADD        ,@2(A16)
       SOS
       HRRM       ,BLTG
       HRRZI      ,@0(A16)              ;BLT START ADR IN AC 0
       ADD        ,@1(A16)
       SOS
       EXCH    A01,@3(A16)
       MOVEM   A01,@0                   ;SET FIRST WORD OF A TO STUFF
       EXCH    A01,@3(A16)
       HRLI       ,251000               ;IF (J1.GE.J2) RETURN
       CAML       ,BLTG
        GOODBY
       HRL
       AOS
BLTG:  BLT
        GOODBY
       A01= 1
       A16=16
       PRGEND
#ifdef CERNLIB_TCGEN_UFILL
#undef CERNLIB_TCGEN_UFILL
#endif
