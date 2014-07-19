*
* $Id: mvstest.jcl,v 1.1.1.1 1996/03/08 15:21:46 mclareni Exp $
*
* $Log: mvstest.jcl,v $
* Revision 1.1.1.1  1996/03/08 15:21:46  mclareni
* Epio
*
*
#include "sys/CERNLIB_machine.h"
#include "_epio/pilot.h"
#if defined(CERNLIB_NEVER)
// JOB
//
// first job
//
// EXEC JFORTCG
//C.SYSIN DD *
//
// implement WRTEST here
//
//G.IOFILE11 DD DSN=gg.uuu.TESTDFIX,DISP=(NEW,CATLG,DELETE),
// SPACE=(TRK,(20,20),RLSE),DCB=(RECFM=F,BLKSIZE=3600,LRECL=3600),
// UNIT=SYSDA
//
// end of first job
//
// JOB
//
// second job
//
// EXEC JFORTCG
//C.SYSIN DD *
//
// implement RDTEST here
//
//G.IOFILE11 DD DSN=gg.uuu.TESTDFIX,DISP=SHR
//G.FT22F001 DD DSN=gg.uuu.TESTLIST,
// DISP=(NEW,CATLG,DELETE),
// DCB=(RECFM=FB,LRECL=80,BLKSIZE=6000),
// SPACE=(TRK,(10,10),RLSE),UNIT=SYSDA
//
// end of second job
//
// JOB
//
// third job
//
// EXEC JFORTCG
//C.SYSIN DD *
//
// implement RATEST here
//
//G.FT22F001 DD DSN=gg.uuu.TESTLIST,DISP=SHR
//
// end of third job
//
#endif
