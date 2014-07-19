*
* $Id: sputfz.F,v 1.2 1996/04/11 14:54:15 cernlib Exp $
*
* $Log: sputfz.F,v $
* Revision 1.2  1996/04/11 14:54:15  cernlib
* Zserv/pawserv used to be build from two patches; these were put both into this
* directory.
* The #includes in all files copied from the other directory had to updated.
*
* Revision 1.1.1.1  1996/03/08 15:44:20  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE SPUTFZ(CHF)
*
*          Transfer a FZ file from the client
*
#include "cspack/czsock.inc"
#include "cspack/czunit.inc"
#include "hbook/hcmail.inc"
#include "zebra/quest.inc"
#include "cspack/pawc.inc"
      PARAMETER (JBIAS=2)
      CHARACTER*(*) CHF
      CHARACTER*64 REMOTE
      CHARACTER*4  RFORM
      CHARACTER*4  CHOPT,CHOPO
      DIMENSION    IOCR(100)
      DIMENSION    IUHEAD(400)
      INTEGER      RRECL
#if defined(CERNLIB_IBMVM)
      CHARACTER*255 CHFILE,CHTEMP
#endif
*
      JB = INDEX(CHF,' ') - 1
      JE = LENOCC(CHF)
      REMOTE = CHF(1:JB)
      READ(CHF(JB+2:JE),'(I6,A4)') RRECL,RFORM
      NCHR   = JB
 
      CHOPT = 'VON'
      IF(INDEX(RFORM,'A').NE.0) CHOPT = 'FON'
      IF(INDEX(RFORM,'D').NE.0) CHOPT = 'DON'
      IF(INDEX(RFORM,'V').NE.0) CHOPT = 'VON'
      IF(INDEX(RFORM,'V').NE.0) CHOPT(4:4) = 'C'
#if defined(CERNLIB_IBMVM)
      LBLK = 16384
      IF(INDEX(RFORM,'Z').NE.0) THEN
         CHTEMP = REMOTE(1:NCHR)
         CALL CTRANS('.',' ',CHTEMP,1,NCHR)
         LASTB  = INDEXB(CHTEMP(1:NCHR),' ')
         IF(INDEX(CHTEMP(1:NCHR),' ').EQ.LASTB) THEN
            CHTEMP(NCHR+1:NCHR+3) = ' A4'
            NCHT = NCHR + 3
         ELSE
            CHTEMP(LASTB+2:LASTB+2) = '4'
            NCHT = LASTB + 2
         ENDIF
         WRITE(CHFILE,9002) LUNXZO,CHTEMP(1:NCHT),LBLK
9002     FORMAT('FILEDEF ',I2,' DISK ',A,
     +          ' (RECFM VBS LRECL 32756 BLOCK ',I6,' PERM)')
         LCHF = LENOCC(CHFILE)
         CALL VMCMS(CHFILE(1:LCHF),IRC)
         OPEN(LUNXZO,STATUS='NEW',FORM='UNFORMATTED',IOSTAT=IRC)
      ELSE
         CALL SZOPEN(LUNXZO,REMOTE(1:NCHR),LRECL,CHOPT,IRC)
      ENDIF
#endif
#if !defined(CERNLIB_IBMVM)
      CALL SZOPEN(LUNXZO,REMOTE(1:NCHR),LRECL,CHOPT,IRC)
#endif
 
      IF(RFORM(1:1).EQ.'Z') RFORM = ' '
      CALL FZFILE(LUNXZO,RRECL/4,'FO'//RFORM)
      IF(IQUEST(1).GT.1) IRC = IQUEST(1)
*
*     Inform client if OPEN is ok
*
      CHMAIL = 'OK'
      IF(IRC.NE.0) CHMAIL = 'KO'
      CALL CZPUTA(CHMAIL,ISTAT)
      IF((ISTAT.NE.0).OR.(IRC.NE.0)) THEN
         CALL FZENDO(LUNXZO,'T')
         RETURN
      ENDIF
*
*     Perform the transfer
*
    1 CONTINUE
      NUH = 400
      CALL FZIN(LUNFZI,IHDIV,LSUP,JBIAS,' ',NUH,IUHEAD)
      IF((IQUEST(1).LT.0).OR.(IQUEST(1).GE.4)) GOTO 2
      IF(IQUEST(1).EQ.0) THEN
         IEVENT = IQUEST(11)
         IF((NUH.EQ.1).AND.(IUHEAD(1).EQ.999)
     +                .AND.(IQUEST(14).EQ.0)) GOTO 2
      ENDIF
*
*     start of run
*
      IF(IQUEST(1).EQ.1) THEN
         CALL FZRUN(LUNXZO,IQUEST(11),NUH,IUHEAD)
         GOTO 1
*
*     end of run
*
      ELSEIF(IQUEST(1).EQ.2) THEN
         CALL FZRUN(LUNXZO,-1,NUH,IUHEAD)
         GOTO 1
*
*     ZEBRA eof
*
      ELSEIF(IQUEST(1).EQ.3) THEN
         CALL FZENDO(LUNXZO,'E')
         GOTO 2
      ENDIF
 
      CALL UCOPY(IQUEST(21),IOCR,MIN(IQUEST(20),100))
      CHOPO = 'L'
      IF(IQUEST(14).EQ.0) CHOPO = 'Z'
      CALL FZOUT(LUNXZO,IHDIV,LSUP,IEVENT,CHOPO,IOCR,NUH,IUHEAD)
      IF(CHOPO.EQ.'L') CALL MZDROP(IHDIV,LSUP,' ')
      GOTO 1
    2 CONTINUE
      CALL FZENDO(LUNXZO,'T')
      CLOSE(LUNXZO)
*
   99 END
 
