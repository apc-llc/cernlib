*
* $Id: crccom.inc,v 1.1.1.1 1996/02/28 16:24:05 mclareni Exp $
*
* $Log: crccom.inc,v $
* Revision 1.1.1.1  1996/02/28 16:24:05  mclareni
* Hepdb, cdlib, etc
*
*
#ifndef CERNLIB_HEPDB_CRCCOM_INC
#define CERNLIB_HEPDB_CRCCOM_INC
*
*
* crccom.inc
*
      PARAMETER       (NMAXCR=20000)
      COMMON /CRCCOM/ IFRSCR, IWORCR(4), QVECCR(NMAXCR), ILSTCR(128)
      DIMENSION       IQVECR(NMAXCR)
      EQUIVALENCE     (QVECCR, IQVECR)
CDEC$ PSECT /CRCCOM/ ALIGN=9
*

#endif
