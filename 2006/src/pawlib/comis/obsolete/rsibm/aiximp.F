*
* $Id: aiximp.F,v 1.1.1.1 1996/02/26 17:16:46 mclareni Exp $
*
* $Log: aiximp.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:46  mclareni
* Comis
*
*
#include "sys/CERNLIB_machine.h"
#include "_comis/pilot.h"
#if defined(CERNLIB_NEVER)
/*CMZ :  1.18/02 13/04/94  17.45.14  by  Fons Rademakers*/
/*-- Author :    Vladimir Berezhnoi   30/03/94*/
/usr/ucb/nm $1 | egrep ' [BAD] '| cut -f3 -d' ' |sed -e 's/^#/ #/' | sort \
| uniq > $1.o

#endif
