*
* $Id: lorenc.s,v 1.1.1.1 1996/02/15 17:53:46 mclareni Exp $
*
* $Log: lorenc.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:46  mclareni
* Kernlib
*
*
        TITLE   LORENC                   ;P. SCHMITZ  JULI 69
        SEARCH HELP
       EXTERN  SQRT
        HELLO (LORENC)
       HRRZI      ,ACSAVE               ;SAVE AC'S
       BLT        ,ACSAVE+5
       MOVEI   A03,@0(A16)              ;ADR(A)
       MOVEI   A04,@1(A16)              ;ADR(B)
        JSR     ,MUL
       MOVEM      ,A05
       MOVEM   A04,A03                  ;B(1)*B(1)+ ...
       JSR        ,MUL
       FADR       ,[EXP ^D1.0]          ;+1.0
       MOVEM      ,ACSAVE
               ENTER (SQRT)
       JUMP       ,ACSAVE
       FADR       ,[EXP ^D1.0]
       FDVR    A05,
       MOVEI   A03,@0(A16)              ;ADR(A)
       MOVEM   A03,A04
        JSR     ,MUL
       FADR       ,[EXP ^D1.0]          ;+1.0
       MOVEM      ,ACSAVE
               ENTER (SQRT)
       JUMP       ,ACSAVE
       FADR    A05,
END:   MOVEI   A01,@0(A16)              ;ADR(A)
       MOVEI   A02,@1(A16)              ;ADR(B)
       MOVEI   A03,@2(A16)              ;ADR(C)
       MOVEI   A04,2                    ;LOOP COUNT
LOOP:  MOVE       ,(A02)                ; DO 50 I=1,3
       FMPR       ,A05
       FADR       ,(A01)
       MOVEM      ,(A03)
       AOS        ,A01
       AOS        ,A02
       AOS        ,A03
       SOJGE   A04,LOOP
       HRLZI      ,ACSAVE               ;RESTORE AC'S
       BLT        ,5
        GOODBY
MUL:    0
       MOVE    A01,(A03)                ;MULTIPLY VECTORS
       FMPR    A01,(A04)
       MOVE       ,1(A03)
       FMPR       ,1(A04)
       FADR       ,A01
       MOVE    A01,2(A03)
       FMPR    A01,2(A04)
       FADR       ,A01
        JRST    2,@MUL
        HELLO (LOTRAN)
       HRRZI      ,ACSAVE               ;SAVE AC'S
       BLT        ,ACSAVE+5
       MOVEI   A03,@1(A16)              ;ADR(B)
       MOVEM   A03,A04
        JSR     ,MUL
       MOVE    A01,3(A03)
       FMPR    A01,A01
       FSBR    A01,                     ;B(4)**2 - ...
       MOVEM   A01,ACSAVE               ;SQRT (...)
               ENTER (SQRT)
       JUMP       ,ACSAVE
       MOVEM      ,A05
       MOVEI   A04,@0(A16)              ;A(1)*B(1) + ...
        JSR     ,MUL
       MOVE    A02,3(A03)
       FADR    A02,A05                  ;/ (B(4)+AMASS)
       FDVRM      ,A02
       MOVEM   A04,A03                  ;A(1)*A(1) + ...
        JSR     ,MUL
       MOVE    A01,3(A03)
       FMPR    A01,A01
       FADR       ,A01                  ;+ A(4)**2
       MOVEM      ,ACSAVE
               ENTER (SQRT)
       JUMP       ,ACSAVE
       FADR       ,A02
       FDVRM      ,A05                  ;AC 5 CONTAINS NOW X
       MOVEI   A01,@2(A16)
       MOVE       ,3(A03)               ;C(4) = A(4)
       MOVEM      ,3(A01)
       JRST       ,END
        HELLO (LOREN4)
       HRRZI      ,ACSAVE               ;SAVE AC'S
       BLT        ,ACSAVE+5
       MOVEI   A03,@0(A16)              ;ADR(A)
       MOVEM   A03,A04
       JSR        ,MUL                  ;A(1)*A(1) + ...
       MOVEM      ,U
       MOVE       ,3(A03)               ;V = SQRT( ...)
       FMPR
       FSBR       ,U
       MOVEM      ,ACSAVE
               ENTER (SQRT)
       JUMP       ,ACSAVE
       MOVE    A01,[EXP ^D1.0]
       FDVR    A01,
       MOVEM   A01,V
       MOVEI   A04,@1(A16)              ;A(1)*B(1) + ...
       JSR        ,MUL
       MOVEM      ,W
       MOVE       ,3(A03)               ;X = W* ...
       FMPR       ,V
       FSBR       ,[EXP ^D1.0]
       FMPR       ,W
       FDVR       ,U
       MOVE    A05,3(A04)
       FMPR    A05,V
       FSBRM      ,A05
       MOVE       ,3(A04)               ;C(4) = V* ...
       FMPR       ,3(A03)
       FSBR       ,W
       FMPR       ,V
       MOVEI   A03,@2(A16)
       MOVEM      ,3(A03)
       MOVEM   A04,A01
       MOVEI   A02,@0(A16)
       JRST       ,LOOP-1
ACSAVE:BLOCK   6
U:     0
V:     0
W:     0
       A01= 1
       A02= 2
       A03= 3
       A04= 4
       A05= 5
       A16=16
       PRGEND
#ifdef CERNLIB_TCGENR_LORENC
#undef CERNLIB_TCGENR_LORENC
#endif
#ifdef CERNLIB_TCGENR_LOREN4
#undef CERNLIB_TCGENR_LOREN4
#endif
#ifdef CERNLIB_TCGENR_LOTRAN
#undef CERNLIB_TCGENR_LOTRAN
#endif
