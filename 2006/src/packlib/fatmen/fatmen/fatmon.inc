*
* $Id: fatmon.inc,v 1.1.1.1 1996/03/07 15:17:52 mclareni Exp $
*
* $Log: fatmon.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:52  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FATMON_INC
#define CERNLIB_FATMEN_FATMON_INC
*
*
* fatmon.inc
*
*
*     Fatmen monitoring information
*     IHOWFA: how was file accessed?
*     ITIMFA: elapsed time in FMOPEN (in seconds)
*     CHFNFA: fully qualified file name, except VM
*
      CHARACTER*255 CHFNFA
      COMMON/FATMON/IHOWFA,ITIMFA,CHFNFA
*     Local disk (standard file system)
      PARAMETER (JLOCFA=1)
*     VM shared file system
      PARAMETER (JSFSFA=2)
*     MSCP (VAXcluster)
      PARAMETER (JMSCFA=3)
*     Andrew file system
      PARAMETER (JAFSFA=4)
*     OSF distributed file system
      PARAMETER (JOSFFA=5)
*     DEC DFS
      PARAMETER (JDFSFA=6)
*     Sun NFS
      PARAMETER (JNFSFA=7)
*     DECnet
      PARAMETER (JDECFA=8)
*     CSPACK server
      PARAMETER (JCSPFA=9)
*     FPACK server
      PARAMETER (JFPKFA=10)
*     RFIO
      PARAMETER (JRFIFA=11)
*     Lachman OSM
      PARAMETER (JOSMFA=12)
*     Unitree
      PARAMETER (JUNIFA=13)
*     Adstar ADSM
      PARAMETER (JDSMFA=14)
*     E-MASS
      PARAMETER (JEMSFA=15)
*     Stage required
      PARAMETER (JSTGFA=31)
*     TPMNT (=not staged)
      PARAMETER (JTPMFA=32)

#endif
