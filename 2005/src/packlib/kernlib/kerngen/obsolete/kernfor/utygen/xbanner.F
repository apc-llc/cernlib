*
* $Id: xbanner.F,v 1.1.1.1 1996/02/15 17:54:16 mclareni Exp $
*
* $Log: xbanner.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:16  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      PROGRAM XBANNER

C CERN PROGLIB# J403    XBANNER         .VERSION KERNFOR  4.39  940228
C ORIG. 22/09/89  JZ

*     unit number for standard output
      PARAMETER    (LUNSTP=6)
#if !defined(CERNLIB_QMAMX)
      PARAMETER    (NARADD = 0)
#endif
      CHARACTER    CHAPO*1
#if (defined(CERNLIB_QASCII))&&(!defined(CERNLIB_QMAMX))&&(!defined(CERNLIB_QMIRT))
      PARAMETER    (CHAPO=CHAR(39))
#endif
#if defined(CERNLIB_QMAMX)
      PARAMETER    (NARADD = -1)
      PARAMETER    (CHAPO='27'X)
#endif
#if defined(CERNLIB_QMIRT)
      PARAMETER    (CHAPO='27'X)
#endif
      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT, DUMMY(36)
      PARAMETER    (MXLENG=256)
      CHARACTER    TEXT*(MXLENG), CHFILE*(MXLENG)
      CHARACTER    TXLINE*9
      DIMENSION    MDATE(2), MTIME(2)
      CHARACTER    CHEJECT*1, CHJOB*16, CHDATE*16, CHTIME*5
      EQUIVALENCE (CHDATE,MDATE), (CHTIME,MTIME)
      LOGICAL      THERE

      CHARACTER*(*)   OLD, NEW, APP, OVER, UNK, FMTT
      PARAMETER      (OLD = 'OLD')
      PARAMETER      (NEW = 'NEW')
      PARAMETER      (APP = 'APPEND')
      PARAMETER      (OVER= 'OVERWRITE')
      PARAMETER      (UNK = 'UNKNOWN')
      PARAMETER      (FMTT= 'FORMATTED')

      DATA  CHJOB  / ' ' /


      LUNPR  = LUNSTP
      NARGS = IARGC () + NARADD
      IF  (NARGS.LE.1)             GO TO 17
      JGO = 1
      CALL GETARG (JGO,TEXT)
      NTX = LNBLNK (TEXT)
      JGO = 2
      GO TO 21

C--                No text parameters, do HELP

   17 PRINT 9002
      PRINT 9003
      GO TO 99
 9002 FORMAT (1X
     F/' XBANNER  param1 param2 param3 ...'
     F/' prints a banner of one or several lines of large text.'
     F/' The first parameter controls the output file :'
     F/'    the file-name maybe prefixed with :'
     F/'        + to append to an existing file,'
     F/'        = to overwrite an existing file,'
     F/'          otherwise a new file is created.'
     F/'        2 to signal page-eject and double-page repeat,'
     F/'        1 to signal page-eject,'
     F/'        0 no page eject.'
     F/'    If no file-name is given, standard output is used;'
     F/'    in this case a stand-alone prefix must be given.')
 9003 FORMAT (1X
     F/' Further parameters give one or several lines of text :'
     F/' Multiple : if the first and the last character of a'
     F/'            parameter are identical and non-alpha this'
     F/'            is split on each such character to a new line.'
     F/' Single :   otherwise the whole parameter gives one line.'
     F/' Any resulting line is always truncated to 9 characters.'
     F/' Examples :'
     F/'      xbanner 0 "LOOK OUT"'
     F/'    gives 1 line of text on standard output, no eject.'
     F/'      xbanner 1         KERN UPDATE // 1.18 APOLLO'
     F/'      xbanner 1=y.lis  /KERN/UPDATE//1.18/APOLLO/'
     F/'      xbanner 2+y.lis   KERN UPDATE "oct 89" 1.18 APOLLO'
     F/'    all give 1 or 2 pages with 5 lines of text each.')

C------            Analyse parameter 1, and open output file if nec.

   21 CHEJECT = ' '
      NPG   = 0
      JTK   = 1
      JOPEN = INDEX ('=+012',TEXT(1:1))
      IF (JOPEN.GE.3)              GO TO 22
      IF (JOPEN.EQ.0)              GO TO 24

C--       Parameter 1 starts with = or +

      JTK = 2
      NPG = INDEX ('012',TEXT(2:2))
      IF (NPG.EQ.0)                GO TO 24
      NPG = NPG - 1
      JTK = 3
      GO TO 24

C--       Parameter 1 starts with  0, 1, or 2

   22 JTK = 2
      NPG = JOPEN - 3
      JOPEN = INDEX ('=+',TEXT(2:2))
      IF (JOPEN.NE.0)  JTK = 3
      GO TO 24

C--       get the file name, if any

   24 NCHF = NTX+1 - JTK
      IF (NCHF.LE.0)               GO TO 29
      CHFILE = TEXT(JTK:NTX)

#if !defined(CERNLIB_QMVAX)
      IF (JOPEN.NE.2)  THEN
          INQUIRE (FILE=CHFILE(1:NCHF),EXIST=THERE)
          IF (THERE)  THEN
              OPEN (LUNPR,FILE=CHFILE(1:NCHF),STATUS=OLD,IOSTAT=J)
              CLOSE (LUNPR,STATUS='DELETE',IOSTAT=J)
            ENDIF
        ENDIF

#endif
#if defined(CERNLIB_QMAPO)
      IF (JOPEN.EQ.2)  THEN
          OPEN (LUNPR,FILE=CHFILE(1:NCHF),STATUS=APP)
        ELSE
          IF (THERE)  THEN
              OPEN (LUNPR,FILE=CHFILE(1:NCHF),STATUS=OLD,IOSTAT=J)
              CLOSE (LUNPR,STATUS='DELETE',IOSTAT=J)
            ENDIF
          OPEN (LUNPR,FILE=CHFILE(1:NCHF),STATUS=OVER)
        ENDIF
#endif
#if defined(CERNLIB_QMALT)||defined(CERNLIB_QMCVX)||defined(CERNLIB_QMSGI)||defined(CERNLIB_QMSUN)||defined(CERNLIB_QMVMI)||defined(CERNLIB_QMVAO)
      IF (JOPEN.EQ.2)  THEN
          OPEN (LUNPR,FILE=CHFILE(1:NCHF),STATUS=UNK,ACCESS=APP)
        ELSE
          OPEN (LUNPR,FILE=CHFILE(1:NCHF),STATUS=UNK)
        ENDIF
#endif
#if defined(CERNLIB_QMCRY)
      IF (JOPEN.NE.2)              GO TO 26
      OPEN (LUNPR,FILE=CHFILE(1:NCHF),STATUS=OLD,POSITION=APP
     +,           FORM=FMTT, ERR=26)
      GO TO 29

   26 OPEN (LUNPR,FILE=CHFILE(1:NCHF),STATUS=NEW,FORM=FMTT)
#endif
#if defined(CERNLIB_QMIBX)||defined(CERNLIB_QMIRT)
      OPEN (LUNPR,FILE=CHFILE(1:NCHF),STATUS=UNK)
      IF (JOPEN.NE.2)   REWIND LUNPR
#endif
#if defined(CERNLIB_QMVAX)
      IF (JOPEN.EQ.0)  THEN
          OPEN (LUNPR,FILE=CHFILE(1:NCHF),STATUS=NEW)
        ELSEIF (JOPEN.EQ.1)  THEN
          OPEN (LUNPR,FILE=CHFILE(1:NCHF),STATUS=UNK)
        ELSEIF (JOPEN.EQ.2)  THEN
          OPEN (LUNPR,FILE=CHFILE(1:NCHF),STATUS=UNK,ACCESS=APP)
        ENDIF
#endif
   29 IF (NPG.GE.1)  CHEJECT='1'

      CHDATE = ' '
      CALL DATIMH (MDATE,MTIME)
      CHDATE(11:15) = CHTIME

C----              Print one page

   41 WRITE (LUNPR,9041) CHEJECT,CHJOB,CHDATE,CHDATE
 9041 FORMAT (A/3X,A,6X,A,12X,A)

      DO  69  JARG=JGO,NARGS
      CALL GETARG (JARG,TEXT)
      NTX = LNBLNK (TEXT)
      IF (NTX.EQ.0)                GO TO 69
      IF (TEXT(1:1).NE.TEXT(NTX:NTX))    GO TO 67
      JV = ICHAR(TEXT(1:1))
      IF (JV.GE.65)  THEN
          IF (JV.LE.90)            GO TO 67
          IF (JV.GE.97)  THEN
              IF (JV.LE.122)       GO TO 67
            ENDIF
        ENDIF

C--       Handle multiple like  /KERN/UPDATE//1.18/APOLLO/

      JNX = 1
   54 JTX = JNX + 1
      JNX = ICFIND (TEXT(1:1),TEXT,JTX,NTX)
      NCH = JNX - JTX
      IF (NCH.EQ.0)  THEN
          NCH = 1
          TXLINE(1:1) = ' '
        ELSE
          NCH = MIN (NCH,9)
          TXLINE(1:NCH) = TEXT(JTX:JTX+NCH-1)
        ENDIF

      CALL VIZPRI (0, TXLINE(1:NCH))

      IF (JNX.LT.NTX)              GO TO 54
      GO TO 69

C--       Handle single like  MIPS.PAM

   67 NCH = MIN (NTX,9)
      CALL VIZPRI (0, TEXT(1:NCH))
   69 CONTINUE

   91 NPG = NPG - 1
      IF (NPG.GT.0)                GO TO 41
   99 CONTINUE
#if defined(CERNLIB_QMAPO)
      CALL PGM_$EXIT
#endif
      END
