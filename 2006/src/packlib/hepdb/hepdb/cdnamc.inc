*
* $Id: cdnamc.inc,v 1.1.1.1 1996/02/28 16:24:02 mclareni Exp $
*
* $Log: cdnamc.inc,v $
* Revision 1.1.1.1  1996/02/28 16:24:02  mclareni
* Hepdb, cdlib, etc
*
*
#ifndef CERNLIB_HEPDB_CDNAMC_INC
#define CERNLIB_HEPDB_CDNAMC_INC
*
*
* cdnamc.inc
*
*     Maximum number of databases that can be handled by one server
      PARAMETER    (MAXDB=50)
*     Maximum number of remote servers
      PARAMETER    (MAXSRV=50)
*     File names, directories for backup files, input queues, remote queues
      COMMON/CDNAMC/LQUED ,LTODO, LLOGD ,LSAVD, LREMQ, LBADD,
     +              NFILES,NSERV,MASTER,
     +              CDTODO,CDQUED,CDLOGD,CDSAVD,CDBADD,CDPRFX,
     +              CDFILE,CDREMQ,CDMODE
      CHARACTER*255 CDFILE(MAXDB),CDSAVD,CDTODO,CDQUED,CDBADD,CDLOGD
      CHARACTER*255 CDREMQ(MAXDB,MAXSRV)
      CHARACTER*2   CDPRFX(MAXDB)
      CHARACTER*1   CDMODE
      INTEGER       LDFILE(MAXDB),NSERV(MAXDB),LREMQ(MAXDB,MAXSRV)
      LOGICAL       MASTER

#endif
