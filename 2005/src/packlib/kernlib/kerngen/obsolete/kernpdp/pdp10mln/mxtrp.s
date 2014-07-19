*
* $Id: mxtrp.s,v 1.1.1.1 1996/02/15 17:53:37 mclareni Exp $
*
* $Log: mxtrp.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:37  mclareni
* Kernlib
*
*
        TITLE   MXTRP
;#
; CERN PROGLIB# F110    MXTRP           .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH HELP
        HELLO (MXTRP)
       HRRZI      ,ACSAV                ;SAVE AC'S
       BLT        ,ACSAV+17
       SKIPG   A03,@3(A16)              ;IF (I.EQ.0.OR.J.EQ.0) RETURN
       JRST       ,END
       SKIPG   A04,@2(A16)
       JRST       ,END
       SOS        ,A04                  ;SET INDICES
       MOVN    A04,A04
       HRRM    A04,LOOPB+10
       HRRM    A03,LOOPB+5
       SOS        ,A03
       HRRM    A03,LOOPB+11
      MOVEI      ,@0(A16)              ;SET ADR OF A AND B
       HRRM       ,LOOPB+3
       MOVEI      ,@1(A16)
       HRRM       ,LOOPB+4
       MOVE       ,[XWD LOOPB+1,5]      ;MOVE LOOP DOWN INTO
       BLT        ,17                   ;FAST ACCUMULATORS
       SETZM      ,A03                  ;CLEAR LOOP REGISTERS
       SETZM      ,A02
LOOPB: JRST       ,LOOP
       PHASE   5
LOOP:  MOVEM   A03,A01                  ;IA=K
       AOS        ,A03                  ;K=K+1
LOOP1: MOVE       ,(A01)                ;B(IB)=A(IA)
       MOVEM      ,(A02)
       ADDI    A01,                     ;IA=IA+J
       AOS        ,A02                  ;IB=IB+1
       AOJLE   A04,LOOP1                ;END OF INNER LOOP
       HRREI   A04,
       CAIG    A03,                     ;END OF OUTER LOOP
       JRST       ,LOOP
       JRST       ,END
       DEPHASE
END:   HRLZI      ,ACSAV                ;RESTORE AC'S, EXIT
       BLT        ,17
        GOODBY
ACSAV: BLOCK   20
       A01= 1
       A02= 2
       A03= 3
       A04= 4
       A16= 16
       PRGEND
#ifdef CERNLIB_TCGEN_MXTRP
#undef CERNLIB_TCGEN_MXTRP
#endif
