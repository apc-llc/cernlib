*
* $Id: p3dbl3.inc,v 1.1.1.1 1996/02/28 16:24:06 mclareni Exp $
*
* $Log: p3dbl3.inc,v $
* Revision 1.1.1.1  1996/02/28 16:24:06  mclareni
* Hepdb, cdlib, etc
*
*
#ifndef CERNLIB_HEPDB_P3DBL3_INC
#define CERNLIB_HEPDB_P3DBL3_INC
*
*
* p3dbl3.inc
*
      COMMON /P3DBL3/ IDDBP3, NWDBP3, IWDBP3(32), LIDBP3, LODBP3, IQDBP3
     +              , IXDBP3, IPASP3, NDIRP3, NBKDP3, NBKYP3, INDXP3
     +              , LNK1P3, LNK2P3, LNK3P3, LNK4P3, LNK5P3, LNK6P3
     +              , LNK7P3, LNK8P3, LNK9P3
*
      COMMON /P3DBC3/ RNDBP3  , MSDBP3
      CHARACTER       RNDBP3*8, MSDBP3*80
*

#endif
