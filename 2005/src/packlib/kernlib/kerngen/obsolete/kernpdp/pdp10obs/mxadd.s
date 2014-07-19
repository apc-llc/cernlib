*
* $Id: mxadd.s,v 1.1.1.1 1996/02/15 17:53:46 mclareni Exp $
*
* $Log: mxadd.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:46  mclareni
* Kernlib
*
*
        TITLE   MXADD
        SUBTTL  P. SCHMITZ
        SEARCH HELP
          RELOC 0
        HELLO (MXADD)
          MOVE 0,EINS        ; CALL MXADD(A,B,C,I,J)
          JRST A             ; C=A+B
        HELLO (MXSUB)
          MOVN 0,EINS        ; CALL MXSUB(A,B,C,I,J)
A:        MOVEM 1,SP1        ; C=A-B
          MOVEI 1,0
          JRST START
        HELLO (MXTRA)
          MOVEI 0,1          ; CALL MXTRA(A,B,C,I,J)
          JRST B             ; C=A
        HELLO (MXMTR)
          MOVE 0,@1(16)      ; CALL MXMTR(A,B,C,I,J)
          JRST B             ; C=B*A
        HELLO (MXNTR)
          RADIX 10
          MOVEI 0,2          ; CALL MXNTR(A,B,C,I,J)
B:        MOVEM 1,SP1        ; C=-A
          MOVEI 1,1
START:   MOVEM 0,WERT
         MOVE 0,BLOUT
         BLT 0,SP3+2
TEST:     MOVE 2,@3(14)
          JUMPLE 2,RETURN
          IMUL 2,@4(14)
          JUMPLE 2,RETURN    ; I UND J MUESSEN POSITIVE ZAHLEN SEIN.
          ADDI 2,@(14)       ; REELLE ENDADRESSE
          MOVEI 3,@(14)      ; ADRESSE VON A(1)
          MOVEI 4,@1(14)     ; ADRESSE VON B(1)
          MOVEI 5,@2(14)     ; ADRESSE VON C(1)
DO:       MOVE 0,WERT
          JUMPLE 1,ADDSUB
          CAIN 0,1
          JRST TRA
          CAIN 0,2
          JRST NTR
          FMPR 0,(3)
          JRST ERHOEH
TRA:      MOVE 0,(3)
          JRST ERHOEH
NTR:      MOVN 0,(3)
          JRST ERHOEH
ADDSUB:   FMPR 0,(4)         ; EINGANG FUER: MXADD UND MXSUB.
          FADR 0,(3)
ERHOEH:   MOVEM 0,(5)        ; ABSPEICHERN DES RESULTATES IN C(I)
          AOS 3              ; ERHOEHEN DER INDICES
          AOS 4
          CAMGE 3,2          ; ENDABFRAGE DER DO-SCHLEIFE.
          AOJA 5,DO
RETURN:   MOVE 5,BLIN
          BLT 5,5
          RADIX 8
        GOODBY
BLOUT:    XWD 3,SP3
BLIN:     XWD SP1,1
EINS:     DEC 1.
SP1:    Z
SP2:      Z
SP3:      BLOCK 3
WERT:     Z
          PRGEND
#ifdef CERNLIB_TCGENR_MXADD
#undef CERNLIB_TCGENR_MXADD
#endif
