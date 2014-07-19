/*
 * $Id: zjump.c,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
 *
 * $Log: zjump.c,v $
 * Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
 * Zbook
 *
 */
#include "zbook/pilot_c.h"
#if defined(CERNLIB_UNIX)
#if defined(CERNLIB_QX_SC)
zjump_(name,p1,p2,p3,p4)
#endif
#if defined(CERNLIB_QXNO_SC)
zjump(name,p1,p2,p3,p4)
#endif
#if defined(CERNLIB_QXCAPT)
ZJUMP(name,p1,p2,p3,p4)
#endif
char *p1, *p2, *p3, *p4;
void (**name)();
{
   (**name)(p1, p2, p3, p4);
}
#endif
