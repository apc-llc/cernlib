*
* $Id: pnutil.F,v 1.6 1999/01/07 13:03:12 couet Exp $
*
* $Log: pnutil.F,v $
* Revision 1.6  1999/01/07 13:03:12  couet
* - KUGETS is called to get the MATCH parameter (instead of KUGETC) in
*   NT/READ. This allows to have case sensitive MATCH strings.
*
* Revision 1.5  1997/01/09 12:33:31  couet
* - A call to HNTLD was missing in NT/READ
*
* Revision 1.4  1996/09/13 15:35:05  couet
* - OLD Qp remove
*
* Revision 1.3  1996/09/11 14:59:36  couet
* - Hgetnt and Hgetn2 (old qp routines) are now replaced by hntld
*
* Revision 1.2  1996/06/13 15:02:47  couet
* - In NT/READ, if the parameter MATCH is not defined, the READ is done
*   directly in the file (not via a temporary character string). This
*   allows to have FORMAT containing '/'.
*
* Revision 1.1.1.1  1996/03/01 11:38:46  mclareni
* Paw
*
*
#include "paw/pilot.h"
*
      SUBROUTINE PNUTIL
*
*            /NTUPLE/CREATE, LIST, PRINT, CSELECT and READ action routines
*
#include "hbook/hcbook.inc"
#include "hbook/hcpiaf.inc"
#include "paw/pawcom.inc"
#include "paw/pawidn.inc"
#include "paw/pccsel.inc"
#include "paw/quest.inc"
#include "paw/pcpatl.inc"
#include "paw/pcchar.inc"
#include "paw/pcfunc.inc"
#include "paw/pawcfu.inc"
#include "paw/pchtag.inc"
*
      CHARACTER*8 CHOPT
*
      CALL KUPATL(CHPATL,NPAR)
*.______________________________________
*
*              NTUPLE/CREATE
*
      IF(CHPATL.EQ.'CREATE')THEN
         CALL PAGETI(ID)
         CALL KUGETS(CHTITL,NCH)
         CALL KUGETI(NDIM)
         CALL KUGETC(CHFILE,NCH)
         CALL KUGETI(NPRIME)
         NDIMA=ABS(NDIM)
         CALL KUGETC(CHTAG(1),NCH)
         DO 10 I=1,NDIMA
            IF(NDIM.GT.0)THEN
               CALL KUGETL(CHTAG(I),NCH)
            ELSE
               NCH=0
            ENDIF
            IF(NCH.EQ.0)THEN
               IF(I.LT.10)THEN
                  WRITE(CHTAG(I),10000) I
               ELSEIF(I.LT.100)THEN
                  WRITE(CHTAG(I),10100) I
               ELSE
                  WRITE(CHTAG(I),10200) I
               ENDIF
            ENDIF
   10    CONTINUE
         CALL HBOOKN(ID,CHTITL,NDIMA,CHFILE,NPRIME,CHTAG)
         GO TO 999
      ENDIF
*.______________________________________
*
*              NTUPLE/LIST
*
      IF(CHPATL.EQ.'LIST')THEN
         CALL HLDIR(' ','N')
         GO TO 999
      ENDIF
*.______________________________________
*
*              NTUPLE/PRINT
*
      IF(CHPATL.EQ.'PRINT')THEN
         CALL KUGETC(CHID,N)
         CALL HNTLD(CHID)
         IF(IQUEST(1).LT.0)GO TO 999
         CALL KUALFA
         CALL HPRINT(ID)
         CALL HRSTD
         GO TO 999
      ENDIF
*.______________________________________
*
*              NTUPLE/DUPLICATE
*
      IF(CHPATL.EQ.'DUPLICATE')THEN
         CALL KUGETC(CHID,N)
         CALL HNTLD(CHID)
         IF(IQUEST(1).LT.0)GO TO 999
         CALL HRSTD
         CALL PAGETI(ID2)
         CALL KUGETI(NEWBUF)
         CALL KUGETS(CHTITL,NCH)
         CALL KUGETC(CHOPT,NCH)
         CALL HNTDUP(ID,ID2,NEWBUF,CHTITL,CHOPT)
         GO TO 999
      ENDIF
*.______________________________________
*
*              NTUPLE/RECOVER
*
      IF(CHPATL.EQ.'RECOVER')THEN
         CALL KUGETI(ID)
         CALL KUALFA
         CALL HRECOV(ID,' ')
         GO TO 999
      ENDIF
*.______________________________________
*
*              NTUPLE/CSELECT
*
      IF(CHPATL.EQ.'CSELECT')THEN
         CALL KUGETC(CSOPT,NCH)
         CALL KUGETR(CSIZE)
         IF(CSIZE.LE.0.)CSIZE=0.28
#if defined(CERNLIB_CZ)
         IF (CONNPF .AND. .NOT.SERVPF) THEN
            WRITE(CHSMPF,'(A,1X,A8,F10.6)') 'nt/cselect',CSOPT,CSIZE
            CALL PFKUIP(CHSMPF, ISTAT)
            IF (ISTAT .NE. 0) THEN
               CALL HBUG('Problem executing CSELECT on Piaf server',
     +                   'PNUTIL',0)
            ENDIF
         ENDIF
#endif
         CALL UOPTC(CSOPT,'RBMCN',IOPTCS)
         IF(IOPTCN.EQ.0)CALL PACSEL
         GO TO 999
      ENDIF
*.______________________________________
*
*              NTUPLE/READ
*
      IF(CHPATL.EQ.'READ')THEN
         CALL KUGETC(CHID,N)
         CALL HNTLD(CHID)
         IF (IQUEST(1).LT.0) GO TO 999
         CALL KUGETF(CHFUNC,NCHF)
         LUNVR=12
         CALL KUOPEN(LUNVR,CHFUNC,'VERYOLD',ISTAT)
         IF (ISTAT.NE.0) THEN
            CALL HBUG('Cannot open file '//CHFUNC(1:NCHF)
     +,               'NTUPLE/READ',IDN)
            GO TO 999
         ENDIF
         CALL KUGETC(CHTEMP,NCH)
         IF (NCH.GT.1) THEN
            ITFORM              = 1
            CHTITL              = ' '
            CHTITL(1:1)         = '('
            CHTITL(2:)          = CHTEMP
            CHTITL(NCH+2:NCH+2) = ')'
            CHTEMP              = CHTITL
         ELSE
            ITFORM              = 0
         ENDIF
         CALL KUGETC(CHOPT,NCH)
         CALL KUGETI(NEVENT)
         IEV  = 0
         NDIM = IQ(LCID+2)
         IDN  = ID
         IF (IQ(LCID+3).NE.0) THEN
            CALL HBUG('Ntuple already filled','NTUPLE/READ',IDN)
            GO TO 999
         ENDIF
*
* Analyse the MATCH parameter. This analysis produce:
*
* IM1    : If 0 there is no MATCH parameter or the MATCH parameter
*          is not valid.
* NEG    : If not 0 the mact is like -/xxx/
* INUM   : Starting position to search the string. 1 is the default, 0
*          means anywhere in the line.
* CHFILE : String to search
* IMLEN  : Length of CHFILE
*
         CHTITL = ' '
         CALL KUGETS(CHTITL,NCH)
         IM1 = INDEX(CHTITL,'/')
         IF (IM1.NE.0) THEN
            IM2 = INDEX(CHTITL(IM1+1:),'/')
            CHFILE = ' '
            NEG    = 0
            INUM   = 1
            IF (IM2.GE.IM1) THEN
               CHFILE = CHTITL(IM1+1:IM2+IM1-1)
               IMLEN  = LENOCC(CHFILE)
               IM1M   = IM1-1
               IF (IM1M.GT.0) THEN
                  IF (CHTITL(IM1M:IM1M).EQ.'-') NEG = 1
               ENDIF
               IMOP = IM1+IM2+1
               IF (CHTITL(IMOP:IMOP).EQ.'(') THEN
                  IMCP = LENOCC(CHTITL)
                  IF (CHTITL(IMCP:IMCP).NE.')') THEN
                     PRINT 10500
                     IM1 = 0
                  ELSE
                     IF (CHTITL(IMOP+1:IMCP-1).EQ.'*') THEN
                        INUM = 0
                     ELSE
                        CALL IZCTOI(CHTITL(IMOP+1:IMCP-1),INUM)
                        IF (IQUEST(1).NE.0) THEN
                           PRINT 10500
                           IM1 = 0
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ELSE
               PRINT 10500
               IM1 = 0
            ENDIF
         ENDIF
*
* Fill the Ntuple taking care of the MATCH parameter if required.
* Each line of the file is read first in a large character string
* and then the event is reject or not according to the value of the
* MATCH parameter.
*
   20    CONTINUE
*
         IF (IM1.NE.0) THEN
            READ (LUNVR,'(A)',END=30,ERR=50) CHBIG
            IF (NEG.NE.0) THEN
               IF (INUM.NE.0) THEN
                  IF (CHFILE.EQ.CHBIG(INUM:INUM+IMLEN-1)) GOTO 20
               ELSE
                  IF (INDEX(CHBIG,CHFILE(1:IMLEN)).NE.0) GOTO 20
               ENDIF
            ELSE
               IF (INUM.NE.0) THEN
                  IF (CHFILE.NE.CHBIG(INUM:INUM+IMLEN-1)) GOTO 20
               ELSE
                  IF (INDEX(CHBIG,CHFILE(1:IMLEN)).EQ.0) GOTO 20
               ENDIF
            ENDIF
            IF (ITFORM.EQ.0) THEN
               READ (CHBIG,*,ERR=50)      (X(I),I=1,NDIM)
            ELSE
               READ (CHBIG,CHTEMP,ERR=60) (X(I),I=1,NDIM)
            ENDIF
         ELSE
            IF (ITFORM.EQ.0) THEN
               READ(LUNVR,*,END=30 ,ERR=50 )(X(I),I=1,NDIM)
            ELSE
               READ(LUNVR,CHTEMP,END=30 ,ERR=60 )(X(I),I=1,NDIM)
            ENDIF
         ENDIF
*
* Fill the Ntuple and jump to the next event in the file.
*
         CALL HFN(IDN,X)
         IEV = IEV+1
         IF (IEV.LT.NEVENT) GO TO 20
*
* Print the number of events read and close the ASCII file
*
   30    IF (IEV.EQ.1) THEN
            PRINT 10400
         ELSE
            PRINT 10300,IEV
         ENDIF
   40    CALL PACLOS(LUNVR)
*
* Save the Ntuple on disk if necessary
*
         NCHRZ = IQ(LCID+11)
         IF (NCHRZ.EQ.0) GO TO 999
         CHFILE = ' '
         CALL UHTOC(IQ(LCID+12),4,CHFILE,NCHRZ)
         CALL HCDIR(CHTITL,'R')
         CALL HCDIR(CHFILE,' ')
         CALL HROUT(IDN,ICYCLE,' ')
         CALL HCDIR(CHTITL,' ')
         GO TO 999
*
   50    CALL HBUG('Free format cannot be used to read '//CHFUNC(1:NCHF)
     +,            'NTUPLE/READ',IDN)
         GOTO 40
   60    CALL HBUG('The file '//CHFUNC(1:NCHF)//' cannot be read with th
     +e format '//CHTEMP,'NTUPLE/READ',IDN)
         GOTO 40
      ENDIF
*
10000 FORMAT('VAR',I1,4X)
10100 FORMAT('VAR',I2,3X)
10200 FORMAT('VAR',I3,2X)
10300 FORMAT (' ==> ',I6,' events have been read')
10400 FORMAT (' ==> Only one event has been read')
10500 FORMAT (' ==> Invalid MATCH parameter')
*
  999 END
