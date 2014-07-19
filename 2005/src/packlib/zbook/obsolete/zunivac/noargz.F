*
* $Id: noargz.F,v 1.1.1.1 1996/03/08 12:01:16 mclareni Exp $
*
* $Log: noargz.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:16  mclareni
* Zbook
*
*
#include "sys/CERNLIB_machine.h"
#include "_zbook/pilot.h"
#if defined(CERNLIB_UNIVAC)
@ASG,T B.
@COPY,R ZPRINT,B.AZPRIN
@DELETE,R ZPRINT
@MASM,IS B.NOARG
 . SUBROUTINE NOARG(NARG) RETURNS NUMBER OF ARGUMENTS IN A SUBROUTINE
 .   OR FUNCTION CALL.
 . UNIVAC  F T N  VERSION BY B.S. NILSSON, NBI, MAY 1981.
 .
          AXR$                        .
  $INFO  2 'HNOARG',2                 .
$(2),NARG +         0                 .
$(1),NOARG*
          LA,U      A2,NARG           . GET ADDR(NARG) IN A2
          LA        A1,0,A0           . GET ADDRESS OF ARG
          LA,Q2     A0,0,A2           . # OF ARGUMENTS
          SA        A0,0,A1           . STORE IT
          J         0,X11             . RETURN
          END       .
@MASM,IS B.ZPRINT$A
         AXR$                         .
  $INFO  2 'HNOARG',2                 .
$(2),NARG  +        0                 .
$(1),ZPRINT* LA,U   A1,NARG           . ADDR(NARG)
          SA        A0,0,A1           . STORE A0 IN NARG
          J         AZPRIN            .
 END
@MAP,ISR ,TPF$.ZPRINT
 IN B.AZPRIN,.ZPRINT$A,.NOARG
 DEF ZPRINT
#endif
