*
* $Id: fmrank.F,v 1.1.1.1 1996/03/07 15:18:07 mclareni Exp $
*
* $Log: fmrank.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:07  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMRANK(CHFILES,KEYS,NFILES,JSORT,CHOPT,IRC)
#include "fatmen/faust.inc"
      CHARACTER*(*) CHFILES(NFILES)
      PARAMETER     (MAXFIL=100)
      CHARACTER*12  CHVID(MAXFIL)
#include "fatmen/fmnkeys.inc"
      DIMENSION     KEYS(LKEYFA,NFILES)
      DIMENSION     JSORT(NFILES)
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatopts.inc"
 
      IRC    = 0
 
      NFRANK = NFRANK + 1
*
*     First loop over all files extracting the VID and FSEQ
*
      DO 10 I=1,MIN(NFILES,MAXFIL)
 
         CALL FMGETK(CHFILES(I),LTDSFA,KEYS(1,I),IRC)
         IF(IRC.NE.0) GOTO 10
         CALL UHTOC(IQ(LTDSFA+KOFUFA+MVIDFA),4,CHVID(I),NVIDFA)
         WRITE(CHVID(I)(7:12),'(I6.6)') IQ(LTDSFA+KOFUFA+MFSQFA)
         CALL MZDROP(IXSTOR,LTDSFA,' ')
         LTDSFA   = 0
 
10    CONTINUE
*
*     Now rank by VID/FSEQ combination
*
      CALL SORTCH(CHVID,JSORT,MIN(NFILES,MAXFIL))
 
      END
