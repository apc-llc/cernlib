*
* $Id: sbyt.s,v 1.1.1.1 1996/02/15 17:54:50 mclareni Exp $
*
* $Log: sbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:50  mclareni
* Kernlib
*
*
       MODULE M_SBYT
%
% CERN PROGLIB# M421    SBYT            .VERSION KERNNOR  1.01  800718
% ORIG.  H.OVERAS, CERN, 791213
%
% CALL SBYT(IM,XM,J,NBIT)   SET BYTE OF NBIT BITS
%
       EXPORT SBYT
       ROUTINE SBYT
       LIB SBYT
VBAS:  STACK FIXED
PAR:   W BLOCK 4
       ENDSTACK
SBYT:  ENTF VBAS
       W2:=IND(B.PAR+8)
       W2-1
       W3:=IND(B.PAR+12)
       W1:=IND(B.PAR)
       W1 PUTBF IND(B.PAR+4),BY2,BY3
       RET
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_SBYT
#undef CERNLIB_TCGEN_SBYT
#endif
