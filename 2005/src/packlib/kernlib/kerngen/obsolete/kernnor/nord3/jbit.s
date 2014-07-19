*
* $Id: jbit.s,v 1.1.1.1 1996/02/15 17:54:48 mclareni Exp $
*
* $Log: jbit.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:48  mclareni
* Kernlib
*
*
       MODULE M_JBIT
%
% CERN PROGLIB# M421    JBIT            .VERSION KERNNOR  1.01  800718
% ORIG.  H.OVERAS, CERN, 791213
%
% IX = JBIT(AM,J)             GET 1 BIT
%
       EXPORT JBIT
       ROUTINE JBIT
       LIB JBIT
VBAS:  STACK FIXED
PAR:   W BLOCK 2
       ENDSTACK
JBIT:  ENTF VBAS
       W2:=IND(B.PAR+4)
       W2-1
       W1 GETBI IND(B.PAR),BY2
       RET
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_JBIT
#undef CERNLIB_TCGEN_JBIT
#endif
