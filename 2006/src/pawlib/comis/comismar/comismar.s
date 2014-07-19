*
* $Id: comismar.s,v 1.1.1.1 1996/02/26 17:16:54 mclareni Exp $
*
* $Log: comismar.s,v $
* Revision 1.1.1.1  1996/02/26 17:16:54  mclareni
* Comis
*
*
*CMZ :  1.19/00 03/04/95  11.34.02  by  Unknown
*-- Author :
      .TITLE      CASETW
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
CVSETR::
      .ENTRY      CASETW,^M<R2>
      MOVL      @8(AP),R0
      BLEQ      2$
      MOVL      @12(AP),R1
      MOVAL      @4(AP),R2
1$:   MOVL      R1,(R2)+
      SOBGTR      R0,1$
2$:   RET
      .TITLE      CBIS
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      CBIS,^M<R2,R3,R4,R5>
      SUBL2      #17,SP                  ; ALLOCATE SPACE IN STACK
      CVTLP      @4(AP),#10,11(SP)      ; LONG TO PACKED
      CVTPS      #10,11(SP),#10,(SP)      ; PACKED TO STRING
      MOVL      @8(AP),R2
      MOVL      @12(AP),R3
      BLEQ      5$
      CMPB      (SP)+,#^A/-/            ; CHECK THE SIGN
      BNEQ      1$
      MOVB      #^A/-/,(R2)+            ; INSERT "-" TO OUTPUT STRING
      DECL      R3
1$:   SKPC      #^A/0/,#10,(SP)            ; SKIP LEADING ZEROS
      BNEQ      2$
      INCL      R0
      DECL      R1
2$:   CMPL      R0,R3
      BGTR      3$
      SUBL3      R0,R3,@12(AP)
      MOVC3      R0,(R1),(R2)
      BRB      4$
3$:   CLRL      @12(AP)
      MOVC5      #0,(R1),#^A/*/,R3,(R2)  ; FILL THE STRING WITH "*"
4$:   MOVL      R3,@8(AP)
5$:   RET
      .TITLE      CCOPYA
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      CCOPYA,^M<R2,R3,R4,R5>
      ASHL      #2,@12(AP),R0
      MOVC3      R0,@4(AP),@8(AP)
      RET
      .TITLE      COPYAJ
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      COPYAJ,^M<R2,R3,R4,R5>
      ROTL      #2,@4(AP),R0
      ROTL      #2,@8(AP),R1
      ASHL      #2,@12(AP),R2
      MOVC3      R2,(R0),(R1)
      RET
      .TITLE      CCOPYS
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      CCOPYS,^M<R2,R3,R4,R5>
      MOVL      @4(AP),R0
      MOVL      @8(AP),R1
      MOVC3      @12(AP),(R0),(R1)
      RET
      .TITLE      CSETC
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      CSETC,^M<>
      MOVB      @8(AP),@4(AP)
      RET
      .TITLE      CSLBDF
;
;   THIS MODULE IS USED FOR COMMON-BLOCK INITIALIZATION ONLY.
;   ENTRY IS USED TO GAIN THE POSSIBILITY TO INCLUDE THIS
;   MODULE FROM LIBRARY.
;
      $LBRDEF            ; DEFINE LIBRARIAN PARAMETERS
;
;   INITIALIZE THE COMMON-BLOCK:
;
      .PSECT      CSLBRW,PIC,OVR,REL,GBL,SHR,NOEXE,RD,WRT,LONG
;
LIBIND: .BLKL      1            ; FOR LIBRARY INDEX
LIBST:  .BLKL      1            ; FOR OPERATION STATUS
LEOF:   .LONG      RMS$_EOF      ; EOF STATUS
LIBRD:  .LONG      LBR$C_READ      ; FUNC_READ
LIBTXT: .LONG      LBR$C_TYP_TXT      ; TYPE_TEXT
;
      .ENTRY      CSLBDF,^M<>
      RET
      .TITLE      IADTWD
;
;   I=IADTWD(IAD): CONVERT ADDRESS IAD TO
;                  'MJWORD-SENCE' ADDRESS I
;
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      IADTWD,^M<>
      ROTL      #-2,@4(AP),R0
      RET
      .TITLE      MIDENT
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      MIDENT,^M<R2,R3,R4,R5>
      CLRL      R0
      MOVL      @12(AP),R1
      MOVL      @16(AP),R2
      BEQL      4$
      BGTR      2$
1$:   RET
2$:   MOVL      R1,R3
      MOVL      R2,R4
      MOVB      #^A/ /,R0
3$:   MOVB      R0,(R3)+
      SOBGTR      R4,3$
4$:   CLRL      R0
      MOVL      @4(AP),R3
      MOVL      @8(AP),R4
      BLEQ      1$
      MOVB      (R3),R5
      CMPB      R5,#^X41
      BLSS      1$
      BRB      13$
10$:  MOVB      (R3),R5
      CMPB      R5,#^X41
      BLSS      15$
13$:  CMPB      R5,#^X5A
      BLEQ      20$
      CMPB      R5,#^X61
      BLSS      40$
      CMPB      R5,#^X7A
      BGTR      40$
      BICB2      #^X20,R5
      BRB      20$
15$:  CMPB      R5,#^X30
      BLSS      40$
      CMPB      R5,#^X39
      BGTR      40$
20$:  TSTL      R2
      BEQL      25$
      MOVB      R5,(R1)+
      DECL      R2
25$:  INCL      R3
      INCL      R0
      SOBGTR      R4,10$
40$:  MOVL      R3,@4(AP)
      MOVL      R4,@8(AP)
      RET
      .TITLE      MDIGIT
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      MDIGIT,^M<>
      MOVZBL      @4(AP),R0
      SUBL2      #^X30,R0
      BLSS      1$
      CMPL      R0,#9
      BLEQ      2$
1$:   MOVL      #-1,R0
2$:   RET
      .TITLE      MIKCHA
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      MIKCHA,^M<>
      MOVL      @4(AP),R0
      MOVZBL      (R0),R0
      RET
      .TITLE      MIWORD
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
CRWORD::
      .ENTRY      MIWORD,^M<>
      ROTL      #2,@4(AP),R1
      MOVL      (R1),R0
      RET
      .TITLE      MJCHAR
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      MJCHAR,^M<>
      MOVAL      @4(AP),R0
      RET
      .TITLE      MJSCHA
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      MJSCHA,^M<>
      MOVAL      @4(AP),R0
      MOVL      4(R0),R0
      RET
      .TITLE      MJSINS
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      MJSINS,^M<R2,R3>
      MOVL      @4(AP),R0
      MOVL      @12(AP),R1
      MATCHC      @8(AP),(R0),@16(AP),(R1)
      BNEQ      1$
      SUBL3      @8(AP),R3,R0
      RET
1$:   CLRL      R0
      RET
      .TITLE      MJSUBR
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      MJSUBR,^M<>
      MOVAL      @4(AP),R0
      RET
      .TITLE      MJWORD
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      MJWORD,^M<>
      ROTL      #-2,4(AP),R0
      RET
      .TITLE      MKBLAN
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      MKBLAN,^M<R2>
      MOVL      @4(AP),R1
      MOVL      @8(AP),R2
      BLEQ      5$
1$:   MOVZBL      (R1)+,R0
      CMPB      R0,#9
      BEQL      2$
      CMPB      R0,#^X20
      BNEQ      3$
2$:   SOBGTR      R2,1$
      MOVL      #^X20202020,R0
      BRB      4$
3$:   DECL      R1
      BISL2      #^X20202000,R0
4$:   MOVL      R1,@4(AP)
      MOVL      R2,@8(AP)
5$:   RET
      .TITLE      MKCHAR
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      MKCHAR,^M<>
      MOVL      @4(AP),R0
      MOVZBL      (R0),R0
      BISL2      #^X20202000,R0
      RET
      .TITLE      MKDIGI
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      MKDIGI,^M<>
      ADDL3      #^X30,@4(AP),R0
      CMPB      R0,#^A/0/
      BLSS      1$
      CMPB      R0,#^A/9/
      BGTR      1$
      BISL2      #^X20202000,R0
      RET
1$:   MOVL      #^A/*   /,R0
      RET
      .TITLE      MLCEQC
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      MLCEQC,^M<>
      CLRL      R0
      CMPB      @4(AP),@8(AP)
      BNEQ      1$
      INCL      R0
1$:   RET
      .TITLE      MLDIGI
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      MLDIGI,^M<>
      MOVZBL      @4(AP),R1
      CLRL      R0
      CMPB      R1,#^A/0/
      BLSS      2$
      CMPB      R1,#^A/9/
      BGTR      2$
      INCL      R0
2$:   RET
      .TITLE      MLEQS
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      MLEQS,^M<R2,R3,R4>
      CLRL      R4
      MOVL      @4(AP),R1
      MOVL      @8(AP),R2
      CMPC3      @12(AP),(R1),(R2)
      BNEQ      1$
      INCL      R4
1$:   MOVL      R4,R0
      RET
      .TITLE      MLETTE
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      MLETTE,^M<>
      MOVZBL      @4(AP),R1
      CLRL      R0
      CMPB      R1,#^X41
      BLSS      2$
      CMPB      R1,#^X5A
      BLEQ      1$
      CMPB      R1,#^X61
      BLSS      2$
      CMPB      R1,#^X7A
      BGTR      2$
1$:   INCL      R0
2$:   RET
      .TITLE      LHXNMB
;
;   L=LHXNMB(J,N,I): CONVERT HEXADECIMAL STRING
;                    TO NUMBER I
;   L=1 IF CONVERTION IS SUCCESSEFUL
;     0 OTHERWIZE
;
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      LHXNMB,^M<R2,R3,R4>
      CLRL      R0
      MOVL      @4(AP),R1
      MOVL      @8(AP),R2
      BLEQ      5$
      CLRL      R3
1$:   MOVZBL      (R1)+,R4
      CMPB      R4,#^A/0/
      BLSS      4$
      CMPB      R4,#^A/9/
      BGTR      2$
; DIGIT 0-9:
      MOVL      #1,R0
      SUBL2      #48,R4
      BRB      3$
2$:   CMPB      R4,#^A/A/
      BLSS      4$
      CMPB      R4,#^A/F/
      BGTR      4$
; MLETTE A-F:
      MOVL      #1,R0
      SUBL2      #55,R4
3$:   ASHL      #4,R3,R3
      BISL2      R4,R3
      SOBGTR      R2,1$
      INCL      R1
4$:   SUBL3      #1,R1,@4(AP)
      MOVL      R2,@8(AP)
      MOVL      R3,@12(AP)
5$:   RET
      .TITLE      MLNUMB
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      MLNUMB,^M<R2,R3,R4,R5>
      CLRL      @12(AP)
      CLRL      R0
      MOVL      @4(AP),R1
      MOVL      @8(AP),R2
      BLEQ      4$
      SUBB3      #^X30,(R1),R3
      BLSS      4$
      CMPB      R3,#9
      BGTR      4$
      INCL      R0
      BRB      2$
1$:   SUBB3      #^X30,(R1),R3
      BLSS      3$
      CMPB      R3,#9
      BGTR      3$
2$:   MOVZBL      R3,R3
      EMUL      #10,@12(AP),R3,R4
      MOVL      R4,@12(AP)
      INCL      R1
      SOBGTR      R2,1$
3$:   MOVL      R1,@4(AP)
      MOVL      R2,@8(AP)
4$:   RET
      .TITLE      MLSEQS
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      MLSEQS,^M<R2,R3,R4>
      CLRL      R4
      MOVL      @8(AP),R1
      CMPL      R1,@16(AP)
      BNEQ      1$
      MOVL      @4(AP),R2
      MOVL      @12(AP),R3
      CMPC3      R1,(R2),(R3)
      BNEQ      1$
      INCL      R4
1$:   MOVL      R4,R0
      RET
      .TITLE      MLSGES
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      MLSGES,^M<R2,R3,R4>
      CLRL      R4
      MOVL      @16(AP),R1
      CMPL      R1,@8(AP)
      BGTR      1$
      MOVL      @4(AP),R2
      MOVL      @12(AP),R3
      CMPC3      R1,(R2),(R3)
      BNEQ      1$
      INCL      R4
1$:   MOVL      R4,R0
      RET
      .TITLE      MCSGES
      .IDENT      /01/
;    FUNCTION MCSGES(J,N,JTMP,NTMP)
      .EXTERNAL CGCONT
      .PSECT      $LOCAL,PIC,CON,REL,LCL,NOSHR,NOEXE,RD,WRT,LONG
J:    .BLKL      1
N:    .BLKL      1
      .PSECT      $PDATA,PIC,CON,REL,LCL,SHR,NOEXE,RD,NOWRT,LONG
ARGB: .LONG      2
      .LONG      J
      .LONG      N
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      MCSGES,^M<R2,R3,R4,R5,R6>
      MOVL      @4(AP),J
      MOVL      @8(AP),N
      MOVL      J,R4
      MOVL      N,R5
      MOVL      @12(AP),R2
      MOVL      @16(AP),R3
      BNEQ      91$
      BRW      71$
91$:  CMPB      (R2),#9
      BEQL      92$
      CMPB      (R2),#^X20
      BEQL      92$
      BRB      9$
92$:  INCL      R2
      BRB      5$
9$:   CALLG      ARGB,MKBLAN
      MOVL      J,R4
      MOVL      N,R5
      BNEQ      1$
      BRW      81$
1$:   MOVB      (R2)+,R6
      CMPB      R6,#9
      BEQL      5$
      CMPB      R6,#^X20
      BEQL      5$
2$:   CMPB      R6,#^X61
      BLSS      3$
      CMPB      R6,#^X7A
      BGTR      3$
      BICB2      #^X20,R6
3$:   MOVB      (R4)+,R1
      CMPB      R1,#^X61
      BLSS      4$
      CMPB      R1,#^X7A
      BGTR      4$
      BICB2      #^X20,R1
4$:   CMPB      R6,R1
      BEQL      10$
      BRW      8$
10$:  DECL      R3
      BEQL      7$
      SOBGTR      R5,1$
      BRW      81$
5$:   MOVB      (R4)+,R1
      CMPB      R1,#9
      BEQL      51$
      CMPB      R1,#^X20
      BNEQ      8$
51$:  DECL      R3
      BEQL      6$
      DECL      R5
      BEQL      81$
      MOVB      (R2)+,R6
      CMPB      R6,#9
      BEQL      5$
      CMPB      R6,#^X20
      BEQL      5$
      MOVL      R4,J
      MOVL      R5,N
      CALLG      ARGB,MKBLAN
      MOVL      J,R4
      MOVL      N,R5
      BRW      2$
6$:   MOVL      R4,J
      SUBL3      #1,R5,N
      CALLG      ARGB,MKBLAN
      MOVL      J,R4
      ADDL3      #1,N,R5
7$:   MOVL      R4,@4(AP)
      SUBL3      #1,R5,@8(AP)
71$:  MOVL      #1,R0
      RET
8$:   DECL      R2
      DECL      R4
81$:  MOVL      R4,J
      MOVL      R5,N
      CALLG      ARGB,CGCONT
      TSTL      R0
      BEQL      82$
      MOVL      J,@4(AP)
      MOVL      N,@8(AP)
      BRW      9$
82$:  RET
      .TITLE      MDEXP
;    LOGICAL FUNCTION MDEXP(L,I)
      .IDENT      /01/
      .EXTERNAL  LIB$GET_VM
      .PSECT      $LOCAL,PIC,CON,REL,LCL,NOSHR,NOEXE,RD,WRT,LONG
IM:   .BLKL      1
LEN:  .BLKL      1
      .PSECT      MDPOOL,PIC,OVR,REL,GBL,SHR,NOEXE,RD,WRT,LONG
IQ:   .BLKL      6
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      MDEXP,^M<R2,R3,R4,R5,R6,R7>
      MOVL      #1,R2
      MOVL      @4(AP),R1
1$:   ADDL3      #1,R2,R3
      MOVL      L^IQ-4[R2],R4
      CMPL      L^IQ-4[R3],R1
      BGEQ      2$
      TSTL      L^IQ-4[R4]
      BEQL      3$
      MOVL      R4,R2
      BRB      1$
2$:   MOVL      R4,@8(AP)
      ADDL2      R1,L^IQ-4[R2]
      SUBL2      R1,L^IQ-4[R3]
      ADDL3      R4,R1,R0
      MOVL      L^IQ[R4],L^IQ[R0]
      MOVL      L^IQ-4[R4],L^IQ-4[R0]
      MOVL      #-1,R0
      RET
3$:   BLBC      R1,4$
      INCL      R1
4$:   ADDL2      #2,R1
      ASHL      #2,R1,LEN
      PUSHAL      IM
      PUSHAL      LEN
      CALLS      #2,G^LIB$GET_VM
      BLBC      R0,6$
      MOVL      @4(AP),R1
      ASHL      #-2,LEN,R5
      SUBL2      #IQ-4,IM
      ASHL      #-2,IM,R6
      ADDL3      R4,L^IQ-4[R3],R7
      ADDL2      #2,R7
      CMPL      R6,R7
      BNEQ      5$
      ADDL2      R5,L^IQ-4[R3]
      BRW      2$
5$:   MOVL      R6,L^IQ-4[R4]
      SUBL3      #2,R5,L^IQ[R4]
      CLRL      L^IQ-4[R6]
      CLRL      L^IQ[R6]
      MOVL      R4,R2
      ADDL3      #1,R2,R3
      MOVL      R6,R4
      BRW      2$
6$:   CLRL      R0
      RET
      .TITLE      MLCMP8
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      MLCMP8,^M<R2,R3>
      CMPC3      #8,@4(AP),@8(AP)
      BGTRU      2$
      BEQL      1$
      MOVL      #-1,R0
      RET
1$:   CLRL      R0
      RET
2$:   MOVL      #1,R0
      RET
      .TITLE      MLEMPT
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      MLEMPT,^M<>
      CLRL      R0
      RET
      .TITLE      MNBLAN
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      MNBLAN,^M<R2>
      MOVL      @8(AP),R0
      BLEQ      3$
      ADDL3      @4(AP),R0,R1
1$:   MOVB      -(R1),R2
      CMPB      R2,#9
      BEQL      2$
      CMPB      R2,#^X20
      BNEQ      3$
2$:   SOBGTR      R0,1$
3$:   RET
      .TITLE      CSBCLL
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
CIFNCL::
CLFCLL::
      .ENTRY      CSBCLL,^M<>
      SUBL3      #1,(AP)+,R0
      BEQL      2$
      MOVL      R0,R1
1$:   PUSHL      (AP)[R1]
      SOBGTR      R1,1$
2$:   MOVL      @0(AP),R1
      CALLS      R0,(R1)
      RET
      .TITLE      CSCHAR
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
CSICHA::
      .ENTRY      CSCHAR,^M<>
      MOVL      @4(AP),R0
      MOVB      @8(AP),(R0)
      RET
      .TITLE      CSSETC
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      CSSETC,^M<R2,R3,R4,R5>
      MOVL      @4(AP),R0
      MOVC5      #0,(AP),@12(AP),@8(AP),(R0)
      RET
      .TITLE      CSWORD
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      CSWORD,^M<>
      ROTL      #2,@4(AP),R1
      MOVL      @8(AP),(R1)
      RET
      .TITLE      CSCALL
      .IDENT      /01/
      .EXTERNAL CSCALX,CSJCAX
;
;      CALL CSCALL(STRING,N,P1,...,PN)
;      CALL CSJCAL(J,N,P1,...,PN)
;
;   OUTPUT: CALL CSCALX(STRING,N,M)
;           CALL CSJCAX(J,N,M)
;   WHERE ARRAY M CONTAINS ADDRESSES OF P1,...,PN
;
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      CSCALL,^M<R2,R3,R4>
      CLRL      R0
      BRB      L14
      .ENTRY      CSJCAL,^M<R2,R3,R4>
      MOVL      #1,R0
L14:  MOVL      (AP)+,R1
      MOVAL      @(AP)+,R1      ; R1= ADDRESS OF THE FIRST PARAMETER
      MOVAL      @(AP)+,R2      ; R2= ADDRESS OF N
      SUBL3      #1,(R2),R3      ; R3=N-1
      BLSS      2$            ; IF N=0
1$:   ROTL      #-2,(AP)[R3],R4
      PUSHL      R4
      SOBGEQ      R3,1$
2$:   MOVL      SP,R3            ; R3= ADDRESS OF M
      PUSHL      R3
      PUSHL      R2
      PUSHL      R1
      TSTL      R0
      BEQL      3$
      CALLS      #3,CSJCAX
      RET
3$:   CALLS      #3,CSCALX
      RET
      .TITLE      CSICAL
      .IDENT      /01/
      .EXTERNAL CSICAX,CSIJCX
;
;      I=CSICAL(STRING,N,P1,...,PN)
;      I=CSIJCL(J,N,P1,...,PN)
;
;   OUTPUT: CALL CSICAX(STRING,N,M)
;           CALL CSIJCX(J,N,M)
;   WHERE ARRAY M CONTAINS ADDRESSES OF P1,...,PN
;
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      CSICAL,^M<R2,R3,R4>
      CLRL      R0
      BRB      L24
      .ENTRY      CSIJCL,^M<R2,R3,R4>
      MOVL      #1,R0
L24:  MOVL      (AP)+,R1
      MOVAL      @(AP)+,R1      ; R1= ADDRESS OF THE FIRST PARAMETER
      MOVAL      @(AP)+,R2      ; R2= ADDRESS OF N
      SUBL3      #1,(R2),R3      ; R3=N-1
      BLSS      2$            ; IF N=0
1$:   ROTL      #-2,(AP)[R3],R4
      PUSHL      R4
      SOBGEQ      R3,1$
2$:   MOVL      SP,R3            ; R3= ADDRESS OF M
      PUSHL      R3
      PUSHL      R2
      PUSHL      R1
      TSTL      R0
      BEQL      3$
      CALLS      #3,CSIJCX
      RET
3$:   CALLS      #3,CSICAX
      RET
      .TITLE      CSRCAL
      .IDENT      /01/
      .EXTERNAL CSRCAX,CSRJCX
;
;      R=CSRCAL(STRING,N,P1,...,PN)
;      R=CSRJCL(J,N,P1,...,PN)
;
;   OUTPUT: CALL CSRCAX(STRING,N,M)
;           CALL CSRJCX(J,N,M)
;   WHERE ARRAY M CONTAINS ADDRESSES OF P1,...,PN
;
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      CSRCAL,^M<R2,R3,R4>
      CLRL      R0
      BRB      L34
      .ENTRY      CSRJCL,^M<R2,R3,R4>
      MOVL      #1,R0
L34:  MOVL      (AP)+,R1
      MOVAL      @(AP)+,R1      ; R1= ADDRESS OF THE FIRST PARAMETER
      MOVAL      @(AP)+,R2      ; R2= ADDRESS OF N
      SUBL3      #1,(R2),R3      ; R3=N-1
      BLSS      2$            ; IF N=0
1$:   ROTL      #-2,(AP)[R3],R4
      PUSHL      R4
      SOBGEQ      R3,1$
2$:   MOVL      SP,R3            ; R3= ADDRESS OF M
      PUSHL      R3
      PUSHL      R2
      PUSHL      R1
      TSTL      R0
      BEQL      3$
      CALLS      #3,CSRJCX
      RET
3$:   CALLS      #3,CSRCAX
      RET
      .TITLE      CSDCAL
      .IDENT      /01/
      .EXTERNAL CSDCAX,CSDJCX
;
;      DP=CSDCAL(STRING,N,P1,...,PN)
;      DP=CSDJCL(J,N,P1,...,PN)
;
;   OUTPUT: CALL CSDCAX(STRING,N,M)
;           CALL CSDJCX(J,N,M)
;   WHERE ARRAY M CONTAINS ADDRESSES OF P1,...,PN
;
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      CSDCAL,^M<R2,R3,R4>
      CLRL      R0
      BRB      L44
      .ENTRY      CSDJCL,^M<R2,R3,R4>
      MOVL      #1,R0
L44:  MOVL      (AP)+,R1
      MOVAL      @(AP)+,R1      ; R1= ADDRESS OF THE FIRST PARAMETER
      MOVAL      @(AP)+,R2      ; R2= ADDRESS OF N
      SUBL3      #1,(R2),R3      ; R3=N-1
      BLSS      2$            ; IF N=0
1$:   ROTL      #-2,(AP)[R3],R4
      PUSHL      R4
      SOBGEQ      R3,1$
2$:   MOVL      SP,R3            ; R3= ADDRESS OF M
      PUSHL      R3
      PUSHL      R2
      PUSHL      R1
      TSTL      R0
      BEQL      3$
      CALLS      #3,CSDJCX
      RET
3$:   CALLS      #3,CSDCAX
      RET
      .TITLE      CSCOM
      .IDENT      /01/
      .EXTERNAL CSCOMX
;
;      CALL CSCOM(STRING,P1,...,PN)
;
;   OUTPUT: CALL CSCOMX(STRING,N,M)
;   WHERE ARRAY M CONTAINS ADDRESSES OF P1,...,PN
;
      .PSECT      $LOCAL,PIC,CON,REL,LCL,NOSHR,NOEXE,RD,WRT,LONG
N1:      .BLKL      1
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      CSCOM,^M<R2>
      SUBL3      #1,(AP)+,R0      ; R0=N1
      MOVL      R0,R1
1$:   ROTL      #-2,(AP)[R1],R2
      PUSHL      R2
      SOBGTR      R1,1$
      MOVL      SP,R1            ; R1=ADDRESS OF M
      PUSHL      R1
      MOVL      R0,N1
      PUSHAL      N1
      PUSHL      (AP)
      CALLS      #3,CSCOMX
      RET
      .TITLE      CSEXT
      .IDENT      /01/
      .EXTERNAL CSEXTX
;
;      CALL CSEXT(STRING,P1,...,PN)
;
;   OUTPUT: CALL CSEXTX(STRING,N,M)
;   WHERE ARRAY M CONTAINS ADDRESSES OF P1,...,PN
;
      .PSECT      $LOCAL,PIC,CON,REL,LCL,NOSHR,NOEXE,RD,WRT,LONG
N2:   .BLKL      1
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
      .ENTRY      CSEXT,^M<>
      SUBL3      #1,(AP)+,R0      ; R0=N2
      MOVL      R0,R1
1$:   PUSHL      (AP)[R1]
      SOBGTR      R1,1$
      MOVL      SP,R1            ; R1=ADDRESS OF M
      PUSHL      R1
      MOVL      R0,N2
      PUSHAL      N2
      PUSHL      (AP)
      CALLS      #3,CSEXTX
      RET
      .TITLE      CSLCAL
      .IDENT      /01/
      .PSECT      $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
CSCALR::
CSCALD::
CSCALI::
      .ENTRY      CSLCAL,^M<R2,R3>
      MOVL      @4(AP),R0
      MOVL      @8(AP),R1
      SUBL3      #1,R1,R2
      BLSS      2$
      MOVAL      @12(AP),R3
1$:   PUSHL      (R3)[R2]
      SOBGEQ      R2,1$
2$:   CALLS      R1,(R0)
      RET
      .END
