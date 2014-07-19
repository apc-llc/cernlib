*
* $Id: iufind.s,v 1.1.1.1 1996/02/15 17:54:48 mclareni Exp $
*
* $Log: iufind.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:48  mclareni
* Kernlib
*
*
       MODULE M_IUFIND
%
% CERN PROGLIB# V304    IUFIND          .VERSION KERNNOR  1.01  800718
% ORIG.  H.OVERAS, CERN, 791213
%
% JX = IUFIND(CM,AVM,JL,JR)    FIND C IN AV(JL/JR), JX=JR+1 NOT FOUND
%
       EXPORT IUFIND
       ROUTINE IUFIND
       LIB IUFIND
VBAS:  STACK FIXED
PAR:   W BLOCK 4
       ENDSTACK
IUFIND:  ENTF VBAS
       W1:=IND(B.PAR+8)
       W1-1
       W2:=IND(B.PAR+12)
       W2-1
       W3:=IND(B.PAR)
BEGLP: W3 COMP IND(B.PAR+4)(R1)
       IF=GO FIN
       W LOOPI R1,R2,BEGLP
FIN:   W1+1
       RET
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_IUFIND
#undef CERNLIB_TCGEN_IUFIND
#endif
