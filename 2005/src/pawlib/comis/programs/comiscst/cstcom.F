*
* $Id: cstcom.F,v 1.1.1.1 1996/02/26 17:16:40 mclareni Exp $
*
* $Log: cstcom.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:40  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_PCOM)
*CMZ :  1.11/02 10/02/91  16.50.12  by  Unknown
*-- Author :
$ ON ERROR THEN GOTO ENDOP
$ IF P1 .EQS. "" THEN $ INQUIRE P1 "INPUT"
$ IF P2 .EQS. "" THEN $ INQUIRE P2 "OUTPUT"
$ INPUT=F$PARSE(P1,".MAP")
$ OUTPUT=F$PARSE(P2,".CST")
$ OPEN/READ/ERROR=OPER FOR001 'INPUT'
$ START:=$DISK$DELPHI:[BEREZHNOI]CST.EXE
$ START 'OUTPUT'
$ENDOP:
$ CLOSE FOR001
$ EXIT
$OPER:
$MESS= "CST -- CAN'T OPEN "+INPUT+" : "+F$MESSAGE($STATUS)
$ WRITE SYS$OUTPUT MESS
$ EXIT
#endif
