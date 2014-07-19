*
* $Id: rndm2.s,v 1.1.1.1 1996/02/15 17:54:49 mclareni Exp $
*
* $Log: rndm2.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:49  mclareni
* Kernlib
*
*
       MODULE M_RNDM2
%
% CERN PROGLIB# V107    RNDM2           .VERSION KERNNOR  2.05  880419
% ORIG.  24-oct-87, P.S.Iversen, Dep. of Physics, Univ. Bergen
%
%      Random routine with same sequence as on VAX
%
       EXPORT  RNDM2,IRNDM2,RD2OUT,RD2IN
%
MCGN:  W DATA   30071B            % 12345 decimal.
SRGN:  W DATA    2061B            % 1073 decimal.
%
       ROUTINE RNDM2
       LIB     RNDM2
VBAS:  STACK FIXED
INPT:  W BLOCK 1
       ENDSTACK
RNDM2: ENTF VBAS                  % REAL=RNDM2(DUMMY)
       W2 := SRGN                 % REGB <- SRGN
       W1 := W2                   % REGC <- REGB
       W SHL W1,-15D              % Right shift REGC 15 bits.
       W2 XOR W1                  % REGB <- XOR(REGB,REGC)
       W1 := W2                   % REGC <- REGB
       W SHL W1,17D               % Left shift REGC 17 bits.
       W2 XOR W1                  % REGB <- XOR(REGB,REGC)
       W2 =: SRGN                 % SRGN <- REGB
       W1 := MCGN                 % REGD <- MCGN
       W1 * 206715B               % REGD <- REGD*69069
       W1 =: MCGN                 % MCGN <- REGD
       W1 XOR W2                  % REGD <- XOR(REGD,REGB)
       W SHL W1,-1D               % Left shift REGD 1 bit.
       W FCONV W1,F1              % Convert to float in A1.
       F MUL2 F1,07040000000B     % Normalize result.
       RET                        % Return, finished.
       ENDROUTINE
%
       ROUTINE IRNDM2
       LIB     IRNDM2
VBAS:  STACK FIXED
INPT:  W BLOCK 1
       ENDSTACK
IRNDM2:ENTF VBAS                  % INTEGER=IRNDM2(DUMMY)
       W2 := SRGN                 % REGB <- SRGN
       W1 := W2                   % REGC <- REGB
       W SHL W1,-15D              % Right shift REGC 15 bits.
       W2 XOR W1                  % REGB <- XOR(REGB,REGC)
       W1 := W2                   % REGC <- REGB
       W SHL W1,17D               % Left shift REGC 17 bits.
       W2 XOR W1                  % REGB <- XOR(REGB,REGC)
       W2 =: SRGN                 % SRGN <- REGB
       W1 := MCGN                 % REGD <- MCGN
       W1 * 206715B               % REGD <- REGD*69069
       W1 =: MCGN                 % MCGN <- REGD
       W1 XOR W2                  % REGD <- XOR(REGD,REGB)
       W SHL W1,-1D               % Left shift REGD 1 bit, return value.
       RET                        % Return, finished.
       ENDROUTINE
%
       ROUTINE RD2IN              % CALL RD2IN(IX,IY)
       LIB     RD2IN
VBAS:  STACK FIXED
IX:    W BLOCK 1
IY:    W BLOCK 1
       ENDSTACK
RD2IN: ENTF VBAS
       W MOVE IND(B.IX),MCGN      % Set MCGN value.
       W MOVE IND(B.IY),SRGN      % Set SRGN value.
       RET                        % Return, finished.
       ENDROUTINE
%
       ROUTINE RD2OUT             % CALL RD2OUT(IX,IY)
       LIB     RD2OUT
VBAS:  STACK FIXED
IX:    W BLOCK 1
IY:    W BLOCK 1
       ENDSTACK
RD2OUT:ENTF VBAS
       W MOVE MCGN,IND(B.IX)      % Get MCGN value.
       W MOVE SRGN,IND(B.IY)      % Get SRGN value.
       RET                        % Return, finished.
       ENDROUTINE
       ENDMODULE
