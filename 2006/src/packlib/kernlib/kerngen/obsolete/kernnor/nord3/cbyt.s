*
* $Id: cbyt.s,v 1.1.1.1 1996/02/15 17:54:47 mclareni Exp $
*
* $Log: cbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:47  mclareni
* Kernlib
*
*
       MODULE M_CBYT
%
% CERN PROGLIB# M421    CBYT            .VERSION KERNNOR  1.01  800718
% ORIG.  H.OVERAS, CERN, 791213
%
% CALL CBYT(AM,JA,XTM,JT,NBIT)   COPY BYTE JA TO BYTE JT
%
       EXPORT CBYT
       ROUTINE CBYT
       LIB CBYT
VBAS:  STACK FIXED
PAR:   W BLOCK 5
       ENDSTACK
CBYT:  ENTF VBAS
       W2:=IND(B.PAR+4)
       W2-1
       W3:=IND(B.PAR+12)
       W3-1
       W4:=IND(B.PAR+16)
       W1 GETBF IND(B.PAR),BY2,BY4
       W1 PUTBF IND(B.PAR+8),BY3,BY4
       RET
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_CBYT
#undef CERNLIB_TCGEN_CBYT
#endif
