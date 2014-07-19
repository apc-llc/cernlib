*
* $Id: cplots.inc,v 1.1.1.1 1996/02/28 16:24:05 mclareni Exp $
*
* $Log: cplots.inc,v $
* Revision 1.1.1.1  1996/02/28 16:24:05  mclareni
* Hepdb, cdlib, etc
*
*
#ifndef CERNLIB_HEPDB_CPLOTS_INC
#define CERNLIB_HEPDB_CPLOTS_INC
*
*
* cplots.inc
*
      PARAMETER       (NOBMCP=50, NPLMCP=500, NPMXCP=10)
      PARAMETER       (NMAXCP=2*NOBMCP)
      COMMON /CPLOTS/ IBINCP(8), INDXCP(2,NOBMCP), KOBJCP(NMAXCP)
     +              , NKSMCP(NPMXCP), NKSTCP(NPMXCP), NDSMCP(NPMXCP)
     +              , NDSTCP(NPMXCP), IBEGCP(NPMXCP), NDEMCP(NPMXCP)
     +              , IOTYCP(NMAXCP), NSKYCP(NPMXCP), NKEYCP(80,NPMXCP)
     +              , KKEYCP(80,NPMXCP), VMAXCP, VMINCP
      COMMON /CPLOCH/ CHRPCP, CTTLCP, DYENCP, DYSTCP
      CHARACTER       CHRPCP*119, CTTLCP*75, DYENCP*10, DYSTCP*10
*

#endif
