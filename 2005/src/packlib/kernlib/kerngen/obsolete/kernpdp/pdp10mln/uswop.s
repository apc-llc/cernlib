*
* $Id: uswop.s,v 1.1.1.1 1996/02/15 17:53:38 mclareni Exp $
*
* $Log: uswop.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:38  mclareni
* Kernlib
*
*
        TITLE   USWOP
;#
; CERN PROGLIB# V303    USWOP           .VERSION KERNPDP  1.00  750903
;#
        SEARCH HELP
        HELLO (USWOP)
       MOVEM   A01,AC01                 ;SAVE AC 1
       MOVE    A01,@2(A16)              ;FETCH N
       SOSGE      ,A01                  ;SKIP IF N .GT. 0
       JRST       ,END
       MOVEI      ,@0(A16)              ;INSERT ADDRESSES
       HRRM       ,USWO+1
       MOVEI      ,@1(A16)
       HRRM       ,USWO+0
       HRRM       ,USWO+2
USWO:  MOVE       ,(A01)                ;EXCHANGE A AND B
       EXCH       ,(A01)
       MOVEM      ,(A01)
       SOJGE   A01,USWO
END:   MOVE    A01,AC01                 ;RESTORE AC 1, EXIT
        GOODBY
AC01:  0
       A01= 1
       A16=16
       PRGEND
#ifdef CERNLIB_TCGEN_USWOP
#undef CERNLIB_TCGEN_USWOP
#endif
