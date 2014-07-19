*
* $Id: fatrek.F,v 1.1.1.1 1996/03/07 15:18:06 mclareni Exp $
*
* $Log: fatrek.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:06  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FATREK(CHPATH,NLEVEL,LWRITE,IRC)
*
************************************************************************
*
*        To traverse the FATMEN directory tree starting from
*        the specified pathname.
*        Modified from the RZ routine RZSTAT / RZSCAN
* Input:
*   CHPATH  The pathname of the starting directory
*   NLEVEL  Number of levels  below CHPATH about which  space information
*           has to be accumulated.
*   UROUT   External user routine to be called for each pathname
*
* Output:
*           IQUEST(11) = number of directories found
*
* Called by <USER>
*
*
************************************************************************
#include "zebra/rzcl.inc"
#include "zebra/rzdir.inc"
#include "zebra/rzch.inc"
#include "zebra/rzk.inc"
      CHARACTER*(*) CHPATH
      DIMENSION ISD(15),NSD(15),IHDIR(4)
*     DIMENSION    IOPTV(2)
*     EQUIVALENCE (IOPTQ,IOPTV(1)), (IOPTX,IOPTV(2))
*
*-----------------------------------------------------------------------
*
*
      IRC=0
      IQUEST(1)  = 0
      IQUEST(11) = 0
*
      ITIME=0
      NLEV=NLEVEL
      IF(NLEV.LE.0)NLEV=99
      NFOUND = 0
      ITEMP  = 0
      CALL FACDIR(CHWOLD,'R')
*
*            Set CWD to the current level
*
   10 CONTINUE
      IF(ITIME.NE.0)THEN
         CALL RZPAFF(CHPAT,NLPAT,CHL)
      IF(IQUEST(1).NE.0) THEN
         NLPAT = NLPAT - 1
         GOTO 20
      ENDIF
         CALL FACDIR(CHL,' ')
      ELSE
         CALL FACDIR(CHPATH,' ')
         IF(IQUEST(1).NE.0)GO TO 99
         NLPAT0=NLPAT
         CHL = CHPATH
      ENDIF
      IF(IQUEST(1).NE.0) THEN
         NLPAT = NLPAT - 1
         GOTO 20
      ENDIF
*
*     Print current directory
*
      LCHL = LENOCC(CHL)
      IF(ITIME.NE.0) THEN
         ISTART = INDEXB(CHL(1:LCHL),'/')-1
         CALL CFILL(' ',CHL,1,ISTART)
      ENDIF
      WRITE (LWRITE,*) CHL(1:LCHL)
      ISD(NLPAT)=0
      NSD(NLPAT)=IQ(KQSP+LCDIR+KNSD)
*
*            Process possible down directories
*
  20  ISD(NLPAT)=ISD(NLPAT)+1
      IF((ISD(NLPAT).LE.NSD(NLPAT)).AND.
     +   (NLPAT.LT.(NLPAT0+NLEV))) THEN
         NLPAT=NLPAT+1
         LS=IQ(KQSP+LCDIR+KLS)
         IH=LS+7*(ISD(NLPAT-1)-1)
         CALL ZITOH(IQ(KQSP+LCDIR+IH),IHDIR,4)
         CALL UHTOC(IHDIR,4,CHPAT(NLPAT),16)
         ITIME=ITIME+1
         GO TO 10
      ELSE
*
*            Write information on current directory
*
         CALL RZPAFF(CHPAT,NLPAT,CHL)
         IF(NLPAT.LE.(NLPAT0+NLEV))THEN
            NFOUND = NFOUND + 1
         ENDIF
   30    CONTINUE
         NLPAT=NLPAT-1
         IF(NLPAT.GE.NLPAT0)THEN
            LUP=LQ(KQSP+LCDIR+1)
            CALL MZDROP(JQPDVS,LCDIR,' ')
            LCDIR=LUP
            GO TO 20
         ENDIF
      ENDIF
*
*            Reset CWD
*
   90 CALL FACDIR(CHWOLD,' ')
*
   99 CONTINUE
      IRC = IQUEST(1)
      IQUEST(11) = NFOUND
      RETURN
      END
