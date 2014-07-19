*
* $Id: uopt.s,v 1.1.1.1 1996/02/15 17:54:50 mclareni Exp $
*
* $Log: uopt.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:50  mclareni
* Kernlib
*
*
       MODULE M_UOPT
%
% CERN PROGLIB# M502    UOPT            .VERSION KERNNOR  1.01  800718
% ORIG.  H.OVERAS, CERN, 791213
%
% CALL UOPT(ACTVM,POSSVM,IX,N)   SELECT OPTIONS FROM POSSIBILITES
%
       EXPORT UOPT
       ROUTINE UOPT
       LIB UOPT
VBAS:  STACK FIXED
PAR:   W BLOCK 4
       ENDSTACK
UOPT:  ENTF VBAS
       W1:=0
       W2:=IND(B.PAR+12)
       W2-1
ZERO:  W MOVE 0,IND(B.PAR+8)(R1)
       W LOOPI R1,R2,ZERO
       W1:=0
       W3:=0
OULP:  BY4:=IND(B.PAR)(R3)
INLP:  BY4 COMP IND(B.PAR+4)(R1)
       IF=GO ONE
       W LOOPI R1,R2,INLP
       RET
ONE:   W MOVE 1,IND(B.PAR+8)(R1)
       W3+1
       W1:=0
       GO OULP
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_UOPT
#undef CERNLIB_TCGEN_UOPT
#endif
