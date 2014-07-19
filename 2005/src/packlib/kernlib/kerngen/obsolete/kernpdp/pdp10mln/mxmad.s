*
* $Id: mxmad.s,v 1.1.1.1 1996/02/15 17:53:36 mclareni Exp $
*
* $Log: mxmad.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:36  mclareni
* Kernlib
*
*
        TITLE   MXMAD
;#
; CERN PROGLIB# F110    MXMAD           .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH HELP
      RELOC 0                ; ZUR ERHOEHUNG DER RECHENGESCHWINDIGKEIT I
        HELLO (MXMAD)
      MOVEI 0,25
      JRST ADCEL
        HELLO (MXMAD1)
      MOVEI 0,26
      JRST ADCEL
        HELLO (MXMAD2)
      MOVEI 0,27
      JRST ADCEL
        HELLO (MXMAD3)
      MOVEI 0,30
      JRST ADCEL
        HELLO (MXMPY3)
       MOVEI 0,16
       JRST ADCEL
        HELLO (MXMUB)
      MOVEI 0,37
      JRST ADCEL
        HELLO (MXMUB1)
      MOVEI 0,40
      JRST ADCEL
        HELLO (MXMUB2)
      MOVEI 0,41
      JRST ADCEL
        HELLO (MXMUB3)
      MOVEI 0,42
       RADIX 10
ADCEL:MOVEM 0,IANDJ
       MOVEM 13,AC15
       MOVEM 15,AC17
      MOVE 15,@3(14)         ;IF    (I.LE.0)    RETURN   ,   IIA=I
      JUMPLE 15,RETURN
      MOVE 12,@5(14)         ;IF    (K.LE.0)    RETURN     ,  IIB=K
      JUMPLE 12,RETURN
      MOVE 2,IANDJ           ;N1=AC2
      IDIVI 2,10             ;N2=AC3
      MOVEI 11,1             ;IOB=1
      MOVEI 13,1             ;IOA=1
      CAILE 3,2              ; GO TO     (3,3,5,5),N2
      JRST A7
   A3:MOVEI 15,1             ;IIA=1
      MOVE 13,@4(14)         ;IOA=J
   A7:IDIVI 3,2              ; GO TO  (9,11,9,11),N2
      JUMPN 4,A13
  A11:MOVEI 12,1             ;IIB=1
      MOVE 11,@4(14)         ;IOB=J
  A13:MOVEI 10,@(14)         ;IA=1  ALS REELLE ADRESSE DES A-ARRAYS: A(1
      MOVEI 8,@2(14)         ;IC=1  ALS REELLE ADRESSE DES C-ARRAYS: C(1
      MOVEI 5,1              ; DO      25        L=1,I
  DO1:MOVEI 9,@1(14)         ;IB=1  ALS REELLE ADRESSE VON B(1)
      MOVEI 4,1              ; DO      23        M=1,K
  DO2:JRST @DO2(2)           ; GO TO (15,18,16),N1
      MOVE A15
      MOVE A18
      MOVE A16
  A15:SETZM (8)              ; C(IC)=0.
      JRST A18               ; GO TO     18
  A16:MOVNS (8)              ; C(IC)=-C(IC)
  A18:MOVE 1,@4(14)          ; IF    (J.EQ.0)     GO TO     23
      JUMPE 1,A23
      MOVE 7,10              ; JA=IA
      MOVE 6,9               ; JB=IB
      MOVE 7,10              ; JA=IA
      MOVE 6,9               ; JB=IB
      MOVEI 3,1              ; DO      20        N=1,J
  DO3:MOVE 0,(7)
      FMPR 0,(6)             ; C(IC)=C(IC)+A(JA)*B(JB)
      FADRM 0,(8)
      ADD 7,15               ; JA=JA+IIA
  A20:ADD 6,12               ; JB=JB+IIB
      CAMGE 3,@4(14)         ; ENDE DER INNERSTEN DO-SCHLEIFE. *********
      AOJA 3,DO3
      ADD 9,11               ; IB=IB+IOB
  A23:AOS 8                  ; IC=IC+1
      CAMGE 4,@5(14)         ; ENDE DER ZWEITEN DO-SCHLEIFE. ***********
      AOJA 4,DO2
  A25:ADD 10,13              ; IA=IA+IOA
      CAMGE 5,@3(14)         ; ENDE DER AEUSSEREN DO-SCHLEIFE. *********
      AOJA 5,DO1
RETURN:MOVE 13,AC15
       MOVE 15,AC17
       RADIX 8
        GOODBY
IANDJ:0
AC15: 0
AC17: 0
      PRGEND
#ifndef CERNLIB_MXFLAG1
#define CERNLIB_MXFLAG1
#endif
