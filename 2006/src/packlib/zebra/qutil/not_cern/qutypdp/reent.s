*
* $Id: reent.s,v 1.1.1.1 1996/03/06 10:47:19 mclareni Exp $
*
* $Log: reent.s,v $
* Revision 1.1.1.1  1996/03/06 10:47:19  mclareni
* Zebra
*
*
        TITLE REENT (ROUT)
        ENTRY   REENT
        SIXBIT  /REENT/
REENT:
#include "qhelopdp.inc"
* Ignoring t=pass
        MOVEI   0,@(16)
        MOVEM   0,124
        POPJ    17,0
PRGEND
