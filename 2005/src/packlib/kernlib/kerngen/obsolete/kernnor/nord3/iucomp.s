*
* $Id: iucomp.s,v 1.1.1.1 1996/02/15 17:54:48 mclareni Exp $
*
* $Log: iucomp.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:48  mclareni
* Kernlib
*
*
       MODULE M_IUCOMP
%
% CERN PROGLIB# V304    IUCOMP          .VERSION KERNNOR  1.01  800718
% ORIG.  H.OVERAS, CERN, 791213
%
% JX = IUCOMP(CM,AVM,N)           FIND C IN AV, JX=0 IF NOT FOUND
%
       EXPORT IUCOMP
       ROUTINE IUCOMP
       LIB IUCOMP
VBAS:  STACK FIXED
PAR:   W BLOCK 3
       ENDSTACK
IUCOMP:  ENTF VBAS
       W1:=0
       W2:=IND(B.PAR+8)
       IF=GO OUT
       W2-1
       W3:=IND(B.PAR)
BEGLP: W3 COMP IND(B.PAR+4)(R1)
       IF=GO MATCH
       W LOOPI R1,R2,BEGLP
       W1:=0
OUT:   RET
MATCH: W1+1
       RET
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_IUCOMP
#undef CERNLIB_TCGEN_IUCOMP
#endif
