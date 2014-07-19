*
* $Id: vmtest.jcl,v 1.1.1.1 1996/03/08 15:21:46 mclareni Exp $
*
* $Log: vmtest.jcl,v $
* Revision 1.1.1.1  1996/03/08 15:21:46  mclareni
* Epio
*
*
#include "sys/CERNLIB_machine.h"
#include "_epio/pilot.h"
#if defined(CERNLIB_NEVER)
/* EXEC file to test EPIO random access reading of records */
 
/* Compile the 3 parts   */
'VFORT EPWRTEST (NOPRINT'
'VFORT EPRDTEST (NOPRINT'
'VFORT EPRATEST (NOPRINT'
 
/* Remove previous files */
 
ERASE RANTEST DATA A
ERASE RANTEST REFTABLE A
 
/* part 1: create a binary EPIO output file */
 
'FI IOFILE11 DISK RANTEST DATA A (BLKSIZE 3600 RECFM F'
'LOAD EPWRTEST EPIONEW (START NOAUTO'
 
/* part 2: read file back, write record/block reference table */
 
FI 22 DISK RANTEST REFTABLE A
'LOAD EPRDTEST EPIONEW (START NOAUTO'
 
/* part 3: read in random acces, check */
 
'LOAD EPRATEST EPIONEW (START NOAUTO'
 
EXIT
#endif
