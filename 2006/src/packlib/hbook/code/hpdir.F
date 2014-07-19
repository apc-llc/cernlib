*
* $Id: hpdir.F,v 1.1.1.1 1996/01/16 17:07:45 mclareni Exp $
*
* $Log: hpdir.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:45  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.13/00 17/07/91  17.34.00  by  Rene Brun
*-- Author :
      SUBROUTINE HPDIR(CHPATH,CHOPT)
*.==========>
*.      To Print histograms of directory CHPATH.
*. Input:
*.   *CHPATH* Character  variable  specifying  the  pathname
*.            (if CHPATH=' ') same as Current directory
*.   CHOPT    Character variable specifying the option
*.            ' '   Print only histograms in CHPATH
*.            'I'   Print an index
*.            'T'   Print subdirectories tree as well
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcdire.inc"
      CHARACTER*(*) CHPATH,CHOPT
      CHARACTER*128 CHWOLD
      DIMENSION LCUR(15),IOPT(2)
      EQUIVALENCE (IOPTT,IOPT(1)),(IOPTI,IOPT(2))
*.___________________________________________
      IF(LHBOOK.EQ.0)GO TO 99
      CALL HUOPTC (CHOPT,'TI',IOPT)
*
*             Save Current directory
*
      CALL HPAFF(CHCDIR,NLCDIR,CHWOLD)
*
*             Set Current Working directory to CHPATH
*
      CALL HCDIR(CHPATH,' ')
      IF(ICHTOP(ICDIR).NE.0)THEN
         CALL HBUG('HPDIR cannot print RZ files','HPDIR',0)
         GO TO 40
      ENDIF
*
      NLPAT0=NLPAT
      LCUR(NLPAT)=LCDIR
*
*            Lists current level
*
      IF(IOPTI.NE.0)CALL HINDEX
      CALL HPRINT(0)
      IF(IOPTT.NE.0)THEN
  20     NLPAT=NLPAT+1
         LCDIR=LQ(LCDIR-1)
  30     LCUR(NLPAT)=LCDIR
         IF(LCDIR.EQ.0)THEN
            NLPAT=NLPAT-1
            LCDIR=LCUR(NLPAT)
            IF(NLPAT.LE.NLPAT0)GO TO 40
            LCDIR=LQ(LCDIR)
            GO TO 30
         ENDIF
         CALL UHTOC(IQ(LCDIR+1),4,CHCDIR(NLPAT),16)
         LIDS=LQ(LCDIR-2)
         LTAB=LQ(LCDIR-3)
         IF(IOPTI.NE.0)CALL HINDEX
         CALL HPRINT(0)
         GO TO 20
      ENDIF
*
*          Restore Current Directory
*
  40  CALL HCDIR(CHWOLD,' ')
*
  99  RETURN
      END
