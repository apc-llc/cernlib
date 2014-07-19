*
* $Id: g900.s,v 1.1.1.1 1996/02/15 17:47:57 mclareni Exp $
*
* $Log: g900.s,v $
* Revision 1.1.1.1  1996/02/15 17:47:57  mclareni
* Kernlib
*
*
%         KERNNUM PACKAGE G900
%         RANDOM NUMBER GENERATOR GIVES SAME SEQUENCE ON CDC
%         ND500 VERSION 821117 BY M.GUANZIROLI / ETH ZURICH
                    MODULE M_RANF
                    ROUTINE RANF
                    LIB RANF
                    EXPORT RANF
                    EXPORT DRANF
                    EXPORT G900GT
                    EXPORT G900ST
                    EXPORT RANGET
                    EXPORT RANSET
BBASE:    STACK FIXED
DUMMY:    W BLOCK 1
S1:       W DATA 00012743214B
S0:       W DATA 00077413155B
          ENDSTACK
M1:       EQU 00012072642B
M0:       EQU 00071730565B
RXM48:    EQU 01400000000B
DX24:     EQU 00600000000B
MASK24:   EQU 00077777777B
%         X = RANF(DUMMY)
RANF:     ENTF BBASE
          W2 UMUL B.S0,M0,W1          % UNSIGNED MULT WITH OVERFL
          W3 := W1
          W3 AND MASK24
          W DCONV W3,D1
          W SHL W1,-30B
          W SHL W2, 10B
          W1 + W2                     % CARRY TO UPPER 3 BYTES
          W4 UMUL B.S0,M1,W2
          W2 AND MASK24
          W1 + W2
          W4 UMUL B.S1,M0,W2
          W2 AND MASK24
          W1 + W2
          W1 AND MASK24
          W1 =: B.S1                  % NEW SEED (UPPER 3 BYTES)
          W3 =: B.S0                  % NEW SEED (LOWER 3 BYTES)
          W DCONV W1,D2
          A2 =: W2
          W2 + DX24                   % UPPER BYTES * 2**24
          A2 := W2
          D1 + D2
          A1 =: W1
          W1 - RXM48                  % NORMAL. TO [0.,1)
          A1 := W1
          RET
%         D = DRANF(DUMMY)              (DOUBLE PRECISION)
DRANF:    ENTF BBASE
          W2 UMUL B.S0,M0,W1          % UNSIGNED MULT WITH OVERFL
          W3 := W1
          W3 AND MASK24
          W DCONV W3,D1
          W SHL W1,-30B
          W SHL W2, 10B
          W1 + W2                     % CARRY TO UPPER 3 BYTES
          W4 UMUL B.S0,M1,W2
          W2 AND MASK24
          W1 + W2
          W4 UMUL B.S1,M0,W2
          W2 AND MASK24
          W1 + W2
          W1 AND MASK24
          W1 =: B.S1                  % NEW SEED (UPPER 3 BYTES)
          W3 =: B.S0                  % NEW SEED (LOWER 3 BYTES)
          W DCONV W1,D2
          A2 =: W2
          W2 + DX24                   % UPPER BYTES * 2**24
          A2 := W2
          D1 + D2
          A1 =: W1
          W1 - RXM48                  % NORMAL. TO [0.,1)
          A1 := W1
          RET
%         G900GT = DSEED
G900GT:   ENTF BBASE
          D1 := IND(B.DUMMY)
          RET
%         G900ST(DSEED)
G900ST:   ENTF BBASE
          D MOVE IND(B.DUMMY),B.S1
          RET
%         CALL RANGET(DSEED)
RANGET:   ENTF BBASE
          D MOVE B.S1,IND(B.DUMMY)
          RET
%         CALL RANSET(DSEED)
RANSET:   ENTF BBASE
          D MOVE IND(B.DUMMY),B.S1
          RET
          ENDROUTINE
          ENDMODULE
