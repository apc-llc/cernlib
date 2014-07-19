*
* $Id: jbytet.s,v 1.1.1.1 1996/02/15 17:54:48 mclareni Exp $
*
* $Log: jbytet.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:48  mclareni
* Kernlib
*
*
       MODULE M_JBYTET
%
% CERN PROGLIB# M421    JBYTET          .VERSION KERNNOR  1.01  800718
% ORIG.  H.OVERAS, CERN, 791213
%
% IX = JBYTET(AM,BM,JB,NBIT)     GET LOGICAL AND OF BYTES
%
       EXPORT JBYTET
       ROUTINE JBYTET
       LIB JBYTET
VBAS:  STACK FIXED
PAR:   W BLOCK 4
       ENDSTACK
JBYTET:  ENTF VBAS
       W2:=IND(B.PAR+8)
       W2-1
       W3:=IND(B.PAR+12)
       W1 GETBF IND(B.PAR+4),BY2,BY3
       W1 AND IND(B.PAR)
       RET
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_JBYTET
#undef CERNLIB_TCGEN_JBYTET
#endif
