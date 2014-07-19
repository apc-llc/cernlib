*
* $Id: vfix.s,v 1.1.1.1 1996/02/15 17:54:51 mclareni Exp $
*
* $Log: vfix.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:51  mclareni
* Kernlib
*
*
       MODULE M_VFIX
%
% CERN PROGLIB# F121    VFIX            .VERSION KERNNOR  2.05  880419
% ORIG.  H.OVERAS, CERN, 791213
% MODIF. P.S.Iversen, 8-dec-87
%
% CALL VFIX(AV,IXV,N)           IXV(I)=AV(I)
%
       EXPORT VFIX
       ROUTINE VFIX
VBAS:  STACK FIXED
PAR:   W BLOCK 3
       ENDSTACK
       LIB VFIX
VFIX:  ENTF VBAS
       W1:=0
       W2:=IND(B.PAR+8)
       IF <= GO OUT                              % If <= zero, exit
       W2-1
BEGLP: F WCONV IND(B.PAR)(R1),IND(B.PAR+4)(R1)
       W LOOPI R1,R2,BEGLP
OUT:   RET
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_VFIX
#undef CERNLIB_TCGEN_VFIX
#endif
