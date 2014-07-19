*
* $Id: dummy.s,v 1.1.1.1 1996/02/15 17:54:07 mclareni Exp $
*
* $Log: dummy.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:07  mclareni
* Kernlib
*
*
. $$$*****$$$*****$$$*****$$$*****
.
.     ROUTINE  DUMMY  (ANY NUMBER OF ARGUMENTS)  *** N 107 ***
.     TO PREVENT LOADING OF UNWANTED ROUTINES YOU MAY TRANSFORM THEN
.     INTO DUMMY ROUTINES USING THE FOLLOWING TRICK :
.     IN YOUR @MAP INTRODUCE THE DIRECTIVES:
.     IN GNSECLIB.DUMMY
.     EQU   XXX/DUMMY    (XXX STANDS FOR YOUR ROUTINE NEME)
.
. $$$*****$$$*****$$$*****$$$*****
 
          AXR$
#if defined(CERNLIB_GUYFORE)||defined(CERNLIB_GUYFORT)
$(1)
DUMMY*
. -------------------------  FORTRAN V VERSION  (SAME FOR FORTRAN E AND FORTRAN
          SLJ       N$ARG              . RETURNS IN A0 THE 'RETURN INCREMENT'  .
          LXI,XU    X11,-1
          AH        A0,X11             . (A0) = LOC( RETURN POINT )
          NOP       A0,0,*X11          . (X11) = LOC( LMJ INSTRUCTION)
          S,H2      A0,0,X11 . LMJ X11,SUBR  TURNED TO  LMJ X11,$+RETURN INCREME
          J         0,A0               . RETURN                                .
#endif
#if defined(CERNLIB_GUYFTN)
$(1),DUMMY*  J      0,X11              . RETURN
#endif
 END
