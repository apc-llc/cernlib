*
* $Id: sgetfz.F,v 1.5 1998/09/25 09:24:19 mclareni Exp $
*
* $Log: sgetfz.F,v $
* Revision 1.5  1998/09/25 09:24:19  mclareni
* Modifications for the Mklinux port flagged by CERNLIB_PPC
*
* Revision 1.4  1997/09/02 08:46:15  mclareni
* WINNT mods, mostly cpp defines
*
* Revision 1.3  1997/01/17 08:56:10  gunter
* call vxinvb for linux too.
*
* Revision 1.2  1996/04/11 14:54:10  cernlib
* Zserv/pawserv used to be build from two patches; these were put both into this
* directory.
* The #includes in all files copied from the other directory had to updated.
*
* Revision 1.1.1.1  1996/03/08 15:44:20  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE SGETFZ(CHF)
*
*          Transfer a FZ file to the client
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
      DIMENSION    IUHEAD(400)
      DIMENSION    IOCR(400)
      INTEGER      RRECL
#if defined(CERNLIB_IBMVM)
      CHARACTER*255 CHTEMP,CHFILE
#endif
      DIMENSION    ITEST(5)
 
#if defined(CERNLIB_IPSC)
      DATA         IPATT/'0123CDEF'X/
#endif
#if defined(CERNLIB_DECS) || defined(CERNLIB_LINUX) || defined(CERNLIB_WINNT)
      DATA         IPATT/X'0123CDEF'/
#endif
#if defined(CERNLIB_VAXVMS)
      DATA         IPATT/Z0123CDEF/
#endif
*
      JB = INDEX(CHF,' ') - 1
      JE = LENOCC(CHF)
      REMOTE = CHF(1:JB)
      READ(CHF(JB+2:JE),'(I6,A4)') RRECL,RFORM
      CHOPT = 'VI'
      IF(INDEX(RFORM,'A').NE.0) CHOPT = 'FI'
      IF(INDEX(RFORM,'D').NE.0) CHOPT = 'DI'
      IF(INDEX(RFORM,'V').NE.0) CHOPT = 'VI'
      IF(INDEX(RFORM,'C').NE.0) CHOPT(3:3) = 'C'
#if defined(CERNLIB_IBMVM)
*
*     Get file information
*
      CHTEMP = REMOTE(1:JB)
      CALL CTRANS('.',' ',CHTEMP,1,JB)
      CALL VMQFIL(CHTEMP(1:JB),RECFM,LBLK,NRECS,
     +   NBLOCKS,CHTIME,ISTAT,IRC)
*
*     Get file mode
*
      INQUIRE(FILE='/'//CHTEMP(1:JB),NAME=CHFILE)
      LCHF = LENOCC(CHFILE)
*
*     OS simulation? Assume VBS...
*
      IF(CHFILE(LCHF:LCHF).EQ.'4') THEN
         WRITE(CHFILE,9002) LUNXZI,CHTEMP(1:JB),LBLK
9002     FORMAT('FILEDEF ',I2,' DISK ',A,
     +          ' (RECFM VBS LRECL 32756 BLOCK ',I6)
         LCHF = LENOCC(CHFILE)
         CALL VMCMS(CHFILE(1:LCHF),IRC)
         OPEN(LUNXZI,STATUS='OLD',FORM='UNFORMATTED',IOSTAT=IRC)
      ELSE
         CALL SZOPEN(LUNXZI,REMOTE(1:JB),RRECL,CHOPT,IRC)
      ENDIF
#endif
#if !defined(CERNLIB_IBMVM)
      CALL SZOPEN(LUNXZI,REMOTE(1:JB),RRECL,CHOPT,IRC)
#endif
 
      IF(RFORM(1:1).EQ.'Z') RFORM = ' '
      LENR = LENOCC(RFORM)
      IF((INDEX(RFORM(1:LENR),'X').NE.0).AND.(RRECL.EQ.0)) THEN
*
*     Get record length from file
*
         IF(INDEX(CHOPT,'D').EQ.0) THEN
         READ(2) ITEST
         REWIND(2)
         ELSE
            READ(LUNXZI,REC=1) ITEST
         ENDIF
#if defined(CERNLIB_VAX)||defined(CERNLIB_DECS) || (defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC)) || defined(CERNLIB_WINNT)
         IF(ITEST(1).NE.IPATT) CALL VXINVB(ITEST(5),1)
#endif
#if !defined(CERNLIB_CRAY)
         RRECL = JBYT(ITEST(5),1,24) * 4
#endif
#if defined(CERNLIB_CRAY)
         RRECL = JBYT(ITEST(3),1,24) * 4
#endif
      ENDIF
 
      CALL FZFILE(LUNXZI,RRECL/4,'F'//RFORM)
      IF(IQUEST(1).GT.1) IRC = IQUEST(1)
*
*     Inform client if OPEN is ok
*
20    CONTINUE
      IF(IRC.NE.0) RRECL = -1
      WRITE(CHMAIL,'(I6)') RRECL
      CALL CZPUTA(CHMAIL,ISTAT)
      IF((ISTAT.NE.0).OR.(IRC.NE.0).OR.(RRECL.LT.0)) THEN
         CALL FZENDI(LUNXZI,'T')
         RETURN
      ENDIF
*
*     Perform the transfer
*
    1 CONTINUE
      NUH = 400
      CALL FZIN(LUNXZI,IHDIV,LSUP,JBIAS,' ',NUH,IUHEAD)
      IF((IQUEST(1).LT.0).OR.(IQUEST(1).GE.4)) GOTO 2
      IF(IQUEST(1).EQ.0) THEN
         IEVENT = IQUEST(11)
      ENDIF
*
*     start of run
*
      IF(IQUEST(1).EQ.1) THEN
         CALL FZRUN(LUNFZO,IQUEST(11),NUH,IUHEAD)
         GOTO 1
*
*     end of run
*
      ELSEIF(IQUEST(1).EQ.2) THEN
         CALL FZRUN(LUNFZO,-1,NUH,IUHEAD)
         GOTO 1
*
*     ZEBRA eof
*
      ELSEIF(IQUEST(1).EQ.3) THEN
         CALL FZENDO(LUNFZO,'E')
         GOTO 2
      ENDIF
 
      CALL UCOPY(IQUEST(21),IOCR(1),MIN(IQUEST(20),100))
      CHOPO = 'L'
      IF(IQUEST(14).EQ.0) CHOPO = 'Z'
      CALL FZOUT(LUNFZO,IHDIV,LSUP,IEVENT,CHOPO,IOCR(1),NUH,IUHEAD)
      IF(CHOPO.EQ.'L') CALL MZDROP(IHDIV,LSUP,' ')
      GOTO 1
    2 CONTINUE
      NUH = 1
      IUHEAD(1) = 999
      IEVENT    = 1
      IOCR(1)   = 2
      CALL FZOUT(LUNFZO,IHDIV,LSUP,IEVENT,'Z',IOCR(1),NUH,IUHEAD)
      CALL FZENDO(LUNFZO,'F')
      CALL FZENDI(LUNXZI,'T')
      CLOSE(LUNXZI)
*
   99 END
 
