*
* $Id: jrsbyt.s,v 1.1.1.1 1996/02/15 17:54:48 mclareni Exp $
*
* $Log: jrsbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:48  mclareni
* Kernlib
*
*
       MODULE M_JRSBYT
%
% CERN PROGLIB# M421    JRSBYT          .VERSION KERNNOR  1.01  800718
% ORIG.  H.OVERAS, CERN, 791213
%
% IX = JRSBYT(IM,XM,J,NBIT)       READ BYTE AND RESET TO IM
%
       EXPORT JRSBYT
       ROUTINE JRSBYT
       LIB JRSBYT
VBAS:  STACK FIXED
PAR:   W BLOCK 4
       ENDSTACK
JRSBYT:  ENTF VBAS
       W2:=IND(B.PAR+8)
       W2-1
       W4:=IND(B.PAR+12)
       W1 GETBF IND(B.PAR+4),BY2,BY4
       W3:=IND(B.PAR)
       W3 PUTBF IND(B.PAR+4),BY2,BY4
       RET
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_JRSBYT
#undef CERNLIB_TCGEN_JRSBYT
#endif
