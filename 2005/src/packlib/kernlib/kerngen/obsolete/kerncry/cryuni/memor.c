/*
 * $Id: memor.c,v 1.1.1.1 1996/02/15 17:52:43 mclareni Exp $
 *
 * $Log: memor.c,v $
 * Revision 1.1.1.1  1996/02/15 17:52:43  mclareni
 * Kernlib
 *
 */
#include "kerncry/pilot.h"
/*>    ROUTINE MEMOR
  CERN PROGLIB#         MEMOR           .VERSION KERNCRY  1.14  910318
  ORIG. 27/06/89  FCA
*/
#include <sys/types.h>
#include <sys/category.h>
#include <sys/jtab.h>
#include <sys/resource.h>
#include <fortran.h>
MEMOR (memuse, memlimit)
long *memuse, *memlimit;
{
        int j;
        long limit();
        struct jtab jtab;
        int getjtab();
        j = getjtab(&jtab);
        if (j > 0) {
                *memuse= jtab.j_memuse*512;
        }
        *memlimit = limit (C_PROC, 0, L_MEM, -1)*512;
}
/*> END <----------------------------------------------------------*/
