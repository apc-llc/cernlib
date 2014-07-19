*
* $Id: rndm.s,v 1.1.1.1 1996/02/15 17:54:49 mclareni Exp $
*
* $Log: rndm.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:49  mclareni
* Kernlib
*
*
       MODULE M_RNDM
%
% CERN PROGLIB# V104    RNDM            .VERSION KERNNOR  2.05  880419
% ORIG.  24-oct-87, P.S.Iversen, Dep. of Physics, Univ. Bergen
%
%      Fast random routine with same sequence as on IBM + VAX
%
       EXPORT  RNDM,IRNDM,RDMIN,RDMOUT
%
SEED:  W DATA  30071B             % Initial seed, if not set by user.
%
       ROUTINE RNDM
       LIB     RNDM
VBAS:  STACK FIXED
INPT:  W BLOCK 1
       ENDSTACK
RNDM:  ENTF VBAS                  % REAL=RNDM(DUMMY)
       W1 := SEED                 % Get SEED value.
       W1 * 2503625B              % Multiply SEED.
       W1 =: SEED                 % Put result back to SEED.
       W SHL W1,-1D               % Left shift SEED 1 bit.
       W FCONV W1,F1              % Convert to float in A1.
       F MUL2 F1,07040000000B     % Normalize result.
       RET                        % Return, finished.
       ENDROUTINE
%
       ROUTINE IRNDM
       LIB     IRNDM
VBAS:  STACK FIXED
INPT:  W BLOCK 1
       ENDSTACK
IRNDM: ENTF VBAS                  % INTEGER=IRNDM(DUMMY)
       W1 := SEED                 % Get SEED value.
       W1 * 2503625B              % Multiply SEED.
       W1 =: SEED                 % Put result back to SEED.
       W SHL W1,-1D               % Left shift SEED 1 bit.
       RET                        % Return, finished.
       ENDROUTINE
%
       ROUTINE RDMIN              % CALL RDMIN(IX)
       LIB     RDMIN
VBAS:  STACK FIXED
INPT:  W BLOCK 1
       ENDSTACK
RDMIN: ENTF VBAS
       W MOVE IND(B.INPT),SEED    % Set SEED value.
       RET                        % Return, finished.
       ENDROUTINE
%
       ROUTINE RDMOUT             % CALL RDMOUT(IX)
       LIB     RDMOUT
VBAS:  STACK FIXED
INPT:  W BLOCK 1
       ENDSTACK
RDMOUT:ENTF VBAS
       W MOVE SEED,IND(B.INPT)    % Get SEED value.
       RET                        % Return, finished.
       ENDROUTINE
       ENDMODULE
