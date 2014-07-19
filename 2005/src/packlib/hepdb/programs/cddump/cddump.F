*
* $Id: cddump.F,v 1.1.1.1 1996/02/28 16:23:33 mclareni Exp $
*
* $Log: cddump.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:33  mclareni
* Hepdb, cdlib, etc
*
*
#include "sys/CERNLIB_machine.h"
#include "_hepdb/pilot.h"
      PROGRAM CDDUMP
*
*     Program to dump headers of journal file(s)
*
      COMMON/PAWC/PAW(50000)
      COMMON/QUEST/IQUEST(100)
      CHARACTER*255 CHPATH,CHDICT
      CHARACTER*8    CHALIA
      CHARACTER*80  CHOPT
      CHARACTER*40  CHACT(9)
      DIMENSION     IUHEAD(400)
      DIMENSION     IOCR(100)
      PARAMETER     (JBIAS=2)
      PARAMETER     (IEVENT=0)
 
      CALL HLIMIT(50000)
*
*     Action codes
*
      CHACT(1) = 'Enter data'
      CHACT(2) = 'MKDIR'
      CHACT(3) = 'Delete data'
      CHACT(4) = 'RMDIR'
      CHACT(5) = 'mv'
      CHACT(6) = 'Help/mnemonic names manipulation'
      CHACT(7) = 'Alias manipulation'
      CHACT(8) = 'Purge partitions'
      CHACT(9) = 'Delete object (don''t)'
 
      CALL FZFILE(1,0,'IA')
      OPEN(1,ACCESS='SEQUENTIAL',
     +    FORM='FORMATTED',
#if defined(CERNLIB_IBM)
     +    ACTION='READ',
#endif
#if defined(CERNLIB_VAXVMS)
     +    READONLY,
#endif
     +    STATUS='OLD')
      CALL FZLOGL(1,0)
#if defined(CERNLIB_IBM)
#endif
#if defined(CERNLIB_VAXVMS)
#endif
      NREC=0
   10 CONTINUE
      NHEAD = 400
      CALL FZIN(1,0,LSUP,2,'S',NHEAD,IUHEAD)
      IF(IQUEST(1).EQ.0) THEN
         NREC = NREC + 1
*
*     Print the fixed part of the header
*
         IF(IUHEAD(1).GT.0.AND.IUHEAD(1).LT.10) THEN
            PRINT 9001,CHACT(IUHEAD(1)),(IUHEAD(I),I=2,4)
 9001 FORMAT(' CDDUMP: ',A,' number of key elements: ',I3,
     +       ' number of words for CHOPT: ',I2,
     +       ' number of words for PATH: ',I2)
         ENDIF
 
         NWKEY  = IUHEAD(2)
         LCHOPT = IUHEAD(3)
         LPATH  = IUHEAD(4)
*
*     Now the variable bit
*
         IF(IUHEAD(1).EQ.1) THEN
            ICHOPT = IUHEAD(2) + 5
            IPATH  = IUHEAD(2) + 6 + LCHOPT
            CALL UHTOC(IUHEAD(ICHOPT),4,CHOPT,LCHOPT*4)
            CALL UHTOC(IUHEAD(IPATH),4,CHPATH,LPATH*4)
            PRINT 9002,CHPATH(1:LPATH*4),CHOPT(1:LCHOPT*4)
 9002 FORMAT(' CDDUMP. path ',A,' CHOPT ',A)
         ELSEIF(IUHEAD(1).EQ.2) THEN
            ICHOPT = 8
            NCFO   = (NWKEY+3)/4
            IPATH  = IUHEAD(4) + NCFO + 2*NWKEY + 8
            CALL UHTOC(IUHEAD(ICHOPT),4,CHOPT,LCHOPT*4)
            CALL UHTOC(IUHEAD(IPATH),4,CHPATH,LPATH*4)
            PRINT 9002,CHPATH(1:LPATH*4),CHOPT(1:LCHOPT*4)
            CALL CDUPTM(IDATE,ITIME,IUHEAD(6),IRC)
            PRINT 9003,IUHEAD(5),IDATE,ITIME
 9003 FORMAT(' CDDUMP. max. # objects/partition = ',I6,
     +       ' date/time = ',I6.6,'/',I4.4)
         ELSEIF(IUHEAD(1).EQ.3) THEN
*
* cdpurg/cdpurk
*
            NPAIRS = IUHEAD(5)
            CALL UHTOC(IUHEAD(ICHOPT),4,CHOPT,LCHOPT*4)
            CALL UHTOC(IUHEAD(IPATH),4,CHPATH,LPATH*4)
            PRINT 9002,CHPATH(1:LPATH*4),CHOPT(1:LCHOPT*4)
         ELSEIF(IUHEAD(1).EQ.4) THEN
            ICHOPT = 7
            IPATH  = ICHOPT + LCHOPT
            CALL UHTOC(IUHEAD(ICHOPT),4,CHOPT,LCHOPT*4)
            CALL UHTOC(IUHEAD(IPATH),4,CHPATH,LPATH*4)
            PRINT 9002,CHPATH(1:LPATH*4),CHOPT(1:LCHOPT*4)
            CALL CDUPTM(IDATE,ITIME,IUHEAD(6),IRC)
            PRINT 9004,IDATE,ITIME
 9004 FORMAT(' CDDUMP. date/time = ',I6.6,'/',I4.4)
         ELSEIF(IUHEAD(1).EQ.5) THEN
            IPATH = 2*IUHEAD(2)+6
            CALL UHTOC(IUHEAD(IPATH),4,CHPATH,LPATH*4)
            PRINT 9005,CHPATH(1:LPATH*4)
 9005 FORMAT(' CDDUMP. path ',A)
         ELSEIF(IUHEAD(1).EQ.6) THEN
            ICHOPT = IUHEAD(2) + 5
            IPATH  = IUHEAD(2) + 6 + LCHOPT
            CALL UHTOC(IUHEAD(ICHOPT),4,CHOPT,LCHOPT*4)
            CALL UHTOC(IUHEAD(IPATH),4,CHPATH,LPATH*4)
            PRINT 9002,CHPATH(1:LPATH*4),CHOPT(1:LCHOPT*4)
            IF(IUHEAD(5).EQ.1) THEN
               PRINT 9006
 9006 FORMAT(' CDDUMP. enter help information')
            ELSEIF(IUHEAD(5).EQ.2) THEN
               PRINT 9007
 9007 FORMAT(' CDDUMP. enter dictionary information')
            ENDIF
         ELSEIF(IUHEAD(1).EQ.7) THEN
            IPATH  = IUHEAD(4) + 9
            IDICT  = 7
            LDICT  = IUHEAD(4)
            LPATH  = IUHEAD(6)
            IALIA  = IUHEAD(4) + 7
            CALL UHTOC(IUHEAD(IPATH),4,CHPATH,LPATH*4)
            CALL UHTOC(IUHEAD(IDICT),4,CHDICT,LPATH*4)
            CALL UHTOC(IUHEAD(IALIA),4,CHALIA,8)
            PRINT 9008,CHALIA,CHPATH(1:LPATH*4),CHDICT(1:LDICT*4)
 9008 FORMAT(' CDDUMP. enter alias ',A,' for path ',A,
     +       ' in dictionary ',A)
         ELSEIF(IUHEAD(1).EQ.8) THEN
            ICHOPT = 7
            IPATH  = IUHEAD(3) + 7
            LPATH  = IUHEAD(4)
            CALL CDUPTM(IDATE,ITIME,IUHEAD(5),IRC)
            CALL UHTOC(IUHEAD(7),4,CHOPT,4)
            CALL UHTOC(IUHEAD(IPATH),4,CHPATH,LPATH*4)
            PRINT 9002,CHPATH(1:LPATH*4),CHOPT(1:LCHOPT*4)
            PRINT 9004,IDATE,ITIME
         ELSEIF(IUHEAD(1).EQ.9) THEN
*
*     CDDONT
*
            ICHOPT = IUHEAD(2) + 5
            IPATH  = IUHEAD(2) + 6 + LCHOPT
            CALL UHTOC(IUHEAD(ICHOPT),4,CHOPT,LCHOPT*4)
            CALL UHTOC(IUHEAD(IPATH),4,CHPATH,LPATH*4)
            PRINT 9002,CHPATH(1:LPATH*4),CHOPT(1:LCHOPT*4)
         ELSE
            PRINT 9009,IUHEAD(1)
 9009 FORMAT(' CDDUMP: unrecognised action code ',I2)
         ENDIF
 
         GOTO 10
      ENDIF
      CALL FZENDI(1,'T')
      CLOSE(1)
*
  999 END
