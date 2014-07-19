*
* $Id: bytops.s,v 1.1.1.1 1996/02/15 17:53:41 mclareni Exp $
*
* $Log: bytops.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:41  mclareni
* Kernlib
*
*
        TITLE   BYTOPS
;#
; CERN PROGLIB# M421    CBYT ET AL.     .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
#include "reentr.inc"
        SEARCH HELP
        HELLO (CBYT)
       MOVE    A01,@1(A16)
       MOVE       ,@4(A16)
       DPB        ,[POINT 6,LDBTB2-1(A01),11]
       MOVE    A02,@3(A16)
       DPB        ,[POINT 6,LDBTB2-1(A02),11]
       LDB        ,LDBTB2-1(A01)
       ADDI    A16,2
       DPB        ,LDBTB2-1(A02)
       SUBI    A16,2
        GOODBY
        HELLO (JBYTET)
        EXCH   A01,@2(A16)
       MOVE       ,@3(A16)
       DPB        ,[POINT 6,LDBTB2-1(A01),11]
       AOS        ,A16
       LDB        ,LDBTB2-1(A01)
       SOS        ,A16
       AND        ,@0(A16)
       EXCH    A01,@2(A16)
        GOODBY
        HELLO (SBYTOR)
        MOVE   A01,@2(A16)
       MOVE       ,@3(A16)
       DPB        ,[POINT 6,LDBTB2-1(A01),11]
       AOS        ,A16
       LDB        ,LDBTB2-1(A01)
       SOS        ,A16
       IOR        ,@0(A16)
       AOS        ,A16
       DPB        ,LDBTB2-1(A01)
       SOS        ,A16
        GOODBY
        HELLO (JRSBYT)
       AOS        ,A16
       EXCH    A01,@1(A16)
       MOVE       ,@2(A16)
       DPB        ,[POINT 6,LDBTB2-1(A01),11]
       LDB        ,LDBTB2-1(A01)
       SOS        ,A16
       EXCH       ,@0(A16)
       AOS        ,A16
       DPB        ,LDBTB2-1(A01)
       SOS        ,A16
       EXCH       ,@0(A16)
       EXCH    A01,@2(A16)
        GOODBY
        HELLO (LOCBYT)
       MOVEM   A01,AC01
       MOVEM   A02,AC02
       SKIPG      ,@2(A16)
       JRST       ,P19
       MOVEI   A01,1
       MOVE    A02,@4(A16)
       MOVE       ,@5(A16)
       DPB        ,[POINT   6,LDBTB3-1(A02),11]
LOOP:  MOVEI   A10,@1(A16)
       ADD     A10,A01
       SOS        ,A10
       LDB        ,LDBTB3-1(A02)
       CAMN       ,@0(A16)
       JRST       ,P21
       CAML    A01,@2(A16)
       JRST       ,P19
       ADD     A01,@3(A16)
       JRST       ,LOOP
P19:   SETZM
       MOVE    A01,AC01
       MOVE    A02,AC02
        GOODBY
P21:   MOVEM   A01,
       MOVE    A01,AC01
       MOVE    A02,AC02
        GOODBY
        HELLO (JBYTPK)
        PUSHJ  P,JBSETI
       DPB        ,[POINT   6,LDBTB3-1(A03),11]
       MOVEI   A10,@0(A16)
       ADD     A10,A02
       LDB        ,LDBTB3-1(A03)
JBSETX:MOVE    A01,AC01
       MOVE    A02,AC02
       MOVE    A03,AC03
       MOVE    A04,AC04
       MOVE    A10,AC10
        GOODBY
        HELLO (SBYTPK)
       AOS        ,A16
        PUSHJ  P,JBSETI
       SOS        ,A16
       DPB        ,[POINT 6,LDBTB3-1(A03),11]
       MOVE       ,@0(A16)
       MOVEI   A10,@1(A16)
       ADD     A10,A02
       DPB        ,LDBTB3-1(A03)
       JRST       ,JBSETX
JBSETI: MOVEM  A01,AC02
       MOVEM   A02,AC02
       MOVEM   A03,AC03
       MOVEM   A04,AC04
       MOVEM   A10,AC10
       MOVE       ,@2(A16)
       JUMPG      ,P7
       MOVEI   A01,^D36
       MOVEI      ,1
       JRST       ,P11
P7:    MOVEI   A01,@2(A16)
       MOVE    A01,1(A01)
P11:   MOVE    A02,@1(A16)
       SOS        ,A02
       IDIV    A02,A01
       MOVEM   A02,A03
       IMUL    A03,A01
       SUB     A03,@1(A16)
       MOVN    A03,A03
       SOS        ,A03
       IMUL    A03,
       AOS        ,A03
        POPJ   P,
#include "reent1.inc"
LDBTB2:POINT   1,@0(A16),35
       POINT   1,@0(A16),34
       POINT   1,@0(A16),33
       POINT   1,@0(A16),32
       POINT   1,@0(A16),31
       POINT   1,@0(A16),30
       POINT   1,@0(A16),29
       POINT   1,@0(A16),28
       POINT   1,@0(A16),27
       POINT   1,@0(A16),26
       POINT   1,@0(A16),25
       POINT   1,@0(A16),24
       POINT   1,@0(A16),23
       POINT   1,@0(A16),22
       POINT   1,@0(A16),21
       POINT   1,@0(A16),20
       POINT   1,@0(A16),19
       POINT   1,@0(A16),18
       POINT   1,@0(A16),17
       POINT   1,@0(A16),16
       POINT   1,@0(A16),15
       POINT   1,@0(A16),14
       POINT   1,@0(A16),13
       POINT   1,@0(A16),12
       POINT   1,@0(A16),11
       POINT   1,@0(A16),10
       POINT   1,@0(A16),09
       POINT   1,@0(A16),08
       POINT   1,@0(A16),07
       POINT   1,@0(A16),06
       POINT   1,@0(A16),05
       POINT   1,@0(A16),04
       POINT   1,@0(A16),03
       POINT   1,@0(A16),02
       POINT   1,@0(A16),01
       POINT   1,@0(A16),00
LDBTB3:POINT   1,(A10),35
       POINT   1,(A10),34
       POINT   1,(A10),33
       POINT   1,(A10),32
       POINT   1,(A10),31
       POINT   1,(A10),30
       POINT   1,(A10),29
       POINT   1,(A10),28
       POINT   1,(A10),27
       POINT   1,(A10),26
       POINT   1,(A10),25
       POINT   1,(A10),24
       POINT   1,(A10),23
       POINT   1,(A10),22
       POINT   1,(A10),21
       POINT   1,(A10),20
       POINT   1,(A10),19
       POINT   1,(A10),18
       POINT   1,(A10),17
       POINT   1,(A10),16
       POINT   1,(A10),15
       POINT   1,(A10),14
       POINT   1,(A10),13
       POINT   1,(A10),12
       POINT   1,(A10),11
       POINT   1,(A10),10
       POINT   1,(A10),9
       POINT   1,(A10),8
       POINT   1,(A10),7
       POINT   1,(A10),6
       POINT   1,(A10),5
       POINT   1,(A10),4
       POINT   1,(A10),3
       POINT   1,(A10),2
       POINT   1,(A10),1
       POINT   1,(A10),0
        P==17
       A01= 1
       A02= 2
       A03= 3
       A04= 4
       A10=10
       A16=16
AC01:  0
AC02:  0
AC03:  0
AC04:  0
AC10:  0
      PRGEND
#ifdef CERNLIB_TCGEN_CBYT
#undef CERNLIB_TCGEN_CBYT
#endif
#ifdef CERNLIB_TCGEN_JBYTET
#undef CERNLIB_TCGEN_JBYTET
#endif
#ifdef CERNLIB_TCGEN_SBYTOR
#undef CERNLIB_TCGEN_SBYTOR
#endif
#ifdef CERNLIB_TCGEN_JRSBYT
#undef CERNLIB_TCGEN_JRSBYT
#endif
#ifdef CERNLIB_TCGEN_LOCBYT
#undef CERNLIB_TCGEN_LOCBYT
#endif
#ifdef CERNLIB_TCGEN_JBYTPK
#undef CERNLIB_TCGEN_JBYTPK
#endif
#ifdef CERNLIB_TCGEN_SBYTPK
#undef CERNLIB_TCGEN_SBYTPK
#endif
