*
* $Id: iuhunt.s,v 1.1.1.1 1996/02/15 17:54:48 mclareni Exp $
*
* $Log: iuhunt.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:48  mclareni
* Kernlib
*
*
       MODULE M_IUHUNT
%
% CERN PROGLIB# V304    IUHUNT          .VERSION KERNNOR  1.01  800718
% ORIG.  H.OVERAS, CERN, 791213
%
% JX = IUHUNT(CM,AVM,N,NA)     FIND C IN AV,EVERY NA,  JX=0 NOT FOUND
%
       EXPORT IUHUNT
       ROUTINE IUHUNT
       LIB IUHUNT
VBAS:  STACK FIXED
PAR:   W BLOCK 3
       ENDSTACK
IUHUNT:  ENTF VBAS
       W1:=0
       W2:=IND(B.PAR+8)
       IF=GO OUT
       W2-1
       W3:=IND(B.PAR)
       W4:=IND(B.PAR+12)
BEGLP: W3 COMP IND(B.PAR+4)(R1)
       IF=GO MATCH
       W LOOP R1,R4,R2,BEGLP
       W1:=0
OUT:   RET
MATCH: W1+1
       RET
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_IUHUNT
#undef CERNLIB_TCGEN_IUHUNT
#endif
