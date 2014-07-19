*
* $Id: fatform0.inc,v 1.3 1997/06/03 07:49:42 jamie Exp $
*
* $Log: fatform0.inc,v $
* Revision 1.3  1997/06/03 07:49:42  jamie
* Support long blocks for new tapes
*
* Revision 1.2  1996/09/23 14:23:56  jamie
* Add EBCD (EBCDIC) format
*
* Revision 1.1.1.1  1996/03/07 15:17:57  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FATFORM0_INC
#define CERNLIB_FATMEN_FATFORM0_INC
*
*
* fatform0.inc
*
*
*     Sequence FATFORM. File format (MFLFFA) and record format (MRFMFA)
*     definitions
*
      PARAMETER (NFATFM=21)
      CHARACTER*4 FATFRM(NFATFM)
 
      PARAMETER (NRECFM=8)
      CHARACTER*4 FRCFM(NRECFM)
 
      INTEGER   FMXBLK,FMXREC
*
*     New tapes support block lengths 8 times ANSI standard
*
      PARAMETER (FMXBLK=65536)
      PARAMETER (FMXREC=65536)
*     PARAMETER (FMXBLK=8192)
*     PARAMETER (FMXREC=8192)
 

#endif
