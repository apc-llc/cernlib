*
* $Id: udicop.s,v 1.1.1.1 1996/02/15 17:53:37 mclareni Exp $
*
* $Log: udicop.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:37  mclareni
* Kernlib
*
*
        TITLE   UDICOP
;#
; CERN PROGLIB# V302    UDICOP          .VERSION KERNPDP  1.00  750903
;#
        SEARCH HELP
        HELLO (UDICOP)
       SKIPG      ,@2(A16)
        GOODBY
       SKIPG      ,@3(A16)
        GOODBY
       HRRZI      ,ACSAVE
       BLT        ,ACSAVE+7
       MOVE    A01,@2(A16)
       SOS        ,A01
       MOVEM   A01,A02
       IMUL    A01,@4(A16)
       IMUL    A02,@5(A16)
       ADD     A01,@3(A16)
       ADD     A02,@3(A16)
       MOVE    A03,@4(A16)
       SUB     A03,@3(A16)
       MOVE    A04,@5(A16)
       SUB     A04,@3(A16)
       MOVEI      ,@0(A16)
       SOS
       HRRM       ,LOOP
       MOVEI      ,@1(A16)
       SOS
       HRRM       ,LOOP+1
       MOVEI   A05,1
       MOVEI   A06,1
LOOP:  MOVE       ,(A01)
       MOVEM      ,(A02)
       SOS        ,A01
       SOS        ,A02
       CAMGE   A06,@3(A16)
       AOJA    A06,LOOP
       MOVEI   A06,1
       SUB     A01,A03
       SUB     A02,A04
       CAMGE   A05,@2(A16)
       AOJA    A05,LOOP
       HRLZI      ,ACSAVE
       BLT        ,7
        GOODBY
ACSAVE:BLOCK   10
       A01= 1
       A02= 2
       A03= 3
       A04= 4
       A05= 5
       A06= 6
       A16=16
      PRGEND
#ifdef CERNLIB_TCGEN_UDICOP
#undef CERNLIB_TCGEN_UDICOP
#endif
