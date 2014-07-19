*
* $Id: jbyt.s,v 1.1.1.1 1996/02/15 17:54:48 mclareni Exp $
*
* $Log: jbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:48  mclareni
* Kernlib
*
*
       MODULE M_JBYT
%
% CERN PROGLIB# M421    JBYT            .VERSION KERNNOR  1.01  800718
% ORIG.  H.OVERAS, CERN, 791213
%
% IX = JBYT(AM,J,NBIT)      GET 1 BYTE OF NBIT BITS
%
       EXPORT JBYT
       ROUTINE JBYT
       LIB JBYT
VBAS:  STACK FIXED
PAR:   W BLOCK 3
       ENDSTACK
JBYT:  ENTF VBAS
       W2:=IND(B.PAR+4)
       W2-1
       W3:=IND(B.PAR+8)
       W1 GETBF IND(B.PAR),BY2,BY3
       RET
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_JBYT
#undef CERNLIB_TCGEN_JBYT
#endif
