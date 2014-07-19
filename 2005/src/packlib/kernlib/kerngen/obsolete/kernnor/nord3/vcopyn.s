*
* $Id: vcopyn.s,v 1.1.1.1 1996/02/15 17:54:51 mclareni Exp $
*
* $Log: vcopyn.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:51  mclareni
* Kernlib
*
*
       MODULE M_VCOPYN
%
% CERN PROGLIB# F121    VCOPYN          .VERSION KERNNOR  2.05  880419
% ORIG.  H.OVERAS, CERN, 791213
% MODIF. P.S.Iversen, 8-dec-87
%
% CALL VCOPYN(AV,XV,N)          XV(I)=-AV(I)
%
       EXPORT VCOPYN
       ROUTINE VCOPYN
       LIB VCOPYN
VBAS:  STACK FIXED
PAR:   W BLOCK 3
       ENDSTACK
VCOPYN:  ENTF VBAS
       W1:=0
       W2:=IND(B.PAR+8)
       IF <= GO OUT                              % If <= zero, exit
       W2-1
BEGLP: F3:=IND(B.PAR)(R1)
       F3 NEG
       F3=:IND(B.PAR+4)(R1)
       W LOOPI R1,R2,BEGLP
OUT:   RET
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_VCOPYN
#undef CERNLIB_TCGEN_VCOPYN
#endif
