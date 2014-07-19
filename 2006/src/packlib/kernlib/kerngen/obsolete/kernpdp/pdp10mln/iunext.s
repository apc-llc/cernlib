*
* $Id: iunext.s,v 1.1.1.1 1996/02/15 17:53:36 mclareni Exp $
*
* $Log: iunext.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:36  mclareni
* Kernlib
*
*
        TITLE   IUNEXT
;#
; CERN PROGLIB# M410    IUNEXT          .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH HELP
        HELLO (IUNEXT)
       HRRZI      ,ACSAVE               ;SAVE AC'S
       BLT        ,ACSAVE+LOOPX
       MOVEI      ,@0(A16)              ;INSERT ADR (IVEC)
       SOS
       HRRM       ,LOOPB+1
       MOVE       ,[XWD LOOPB+1,LOOP]   ;MOVE LOOP DOWN INTO
       BLT        ,LOOPX                ;FAST ACCUMULATORS
       MOVE    A02,BLANK
       MOVE    A01,@1(A16)
LOOPB: JRST       ,LOOP
       PHASE      3
LOOP:  MOVE       ,(A01)                ;BLANK IN IVEC ?
       CAMN       ,A02
       AOJA    A01,LOOP
LOOPX: JRST       ,END
       DEPHASE
END:   MOVEM   A01,                     ;IUNEXT=J
       MOVE  LOOPX,[XWD ACSAVE+1,1]     ;RESTORE AC'S, EXIT
       BLT   LOOPX,LOOPX
        GOODBY
BLANK: ASCII   /     /
ACSAVE:BLOCK   LOOPX+1
       A01= 1
       A02= 2
       A16=16
       PRGEND
#ifdef CERNLIB_TCGEN_IUNEXT
#undef CERNLIB_TCGEN_IUNEXT
#endif
