*
* $Id: cdtrek.F,v 1.1.1.1 1996/02/28 16:24:37 mclareni Exp $
*
* $Log: cdtrek.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:37  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDTREK(CHPATH,NLEVEL,CHOPT,IRC)
*
************************************************************************
*
*        To traverse the HEPDB directory tree starting from
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
#include "hepdb/cdunit.inc"
#include "zebra/rzcl.inc"
#include "zebra/rzdir.inc"
#include "zebra/rzch.inc"
#include "zebra/rzk.inc"
      CHARACTER*(*) CHPATH
      CHARACTER*8   CHALIA
      DIMENSION ISD(15),NSD(15),IHDIR(4)
#include "hepdb/hdbopts.inc"
*
*-----------------------------------------------------------------------
*
*
*
*     Incompatible options
*
      IF(IOPTN.NE.0) IOPTO = 0
      ISPACE = IOPTO+IOPTS+IOPTC+IOPTM
 
      IRC        = 0
      IQUEST(1)  = 0
      IQUEST(11) = 0
*
      ITIME=0
      NLEV=NLEVEL
      IF(NLEV.LE.0)NLEV=99
      NFOUND = 0
      ITEMP  = 0
      CALL RZCDIR(CHWOLD,'R')
*
*            Set CWD to the current level
*
   10 CONTINUE
      IF(ITIME.NE.0)THEN
         CALL RZPAFF(CHPAT,NLPAT,CHL)
         CALL RZCDIR(CHL,' ')
      ELSE
         CALL RZCDIR(CHPATH,' ')
         IF(IQUEST(1).NE.0)GO TO 99
         NLPAT0=NLPAT
         CHL = CHPATH
      ENDIF
      IF(IQUEST(1).NE.0) THEN
         NLPAT = NLPAT - 1
         GOTO 90
      ENDIF
*
*     Print current directory
*
      LCHL = LENOCC(CHL)
 
      IF(IOPTA.NE.0) THEN
         CALL CDGALI(CHL(1:LCHL),CHALIA,IRC)
      ENDIF
      IF(ITIME.NE.0) THEN
         ISTART = INDEXB(CHL(1:LCHL),'/')-1
         CALL CFILL(' ',CHL,1,ISTART)
      ENDIF
 
      IF(IOPTA.EQ.0) THEN
         WRITE (LOUTCD,*) CHL(1:LCHL)
      ELSE
         WRITE (LOUTCD,9001) CHALIA,CHL(1:LCHL)
9001  FORMAT(1X,A8,1X,A)
      ENDIF
*
*     Counters
*
*.    IQUEST(7)=IQ(KQSP+LCDIR+KNKEYS)
*.    IQUEST(8)=IQ(KQSP+LCDIR+KNWKEY)
*.    IQUEST(9)=IQ(KQSP+LCDIR+KNSD)
*.    IQUEST(14)=IDATEC
*.    IQUEST(15)=ITIMEC
*.    IQUEST(16)=IDATEM
*.    IQUEST(17)=ITIMEM
 
      IF(IOPTN.NE.0.AND.IQUEST(7).GT.0) WRITE (LOUTCD,9002) IQUEST(7)
9002  FORMAT(' Objects       : ',I11)
      IF(IOPTO.NE.0) WRITE (LOUTCD,9002) IQUEST(7)
      IF(IOPTS.NE.0) WRITE (LOUTCD,9003) IQUEST(9)
9003  FORMAT(' Subdirectories: ',I11)
      IF(IOPTC.NE.0) WRITE (LOUTCD,9004) IQUEST(14),IQUEST(15)
9004  FORMAT(' Created       : ',I6,1X,I4)
      IF(IOPTM.NE.0) WRITE (LOUTCD,9005) IQUEST(16),IQUEST(17)
9005  FORMAT(' Modified      : ',I6,1X,I4)
 
      JSPACE = ISPACE + IQUEST(7)
      IF(JSPACE.NE.0) WRITE(LOUTCD,*)
 
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
   90 CALL RZCDIR(CHWOLD,' ')
*
   99 CONTINUE
      IRC = IQUEST(1)
      IQUEST(11) = NFOUND
      RETURN
      END
