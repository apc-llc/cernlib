*
* $Id: namefd.F,v 1.1.1.1 1996/02/15 17:47:50 mclareni Exp $
*
* $Log: namefd.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:50  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE NAMEFD(LUN,CHFILE,CHIN,NIN,CHOUT,NOUT,IRC)
      CHARACTER*(*) CHIN(2,NIN),CHOUT(2,NOUT),CHFILE
      CHARACTER*255 CHLINE,CHBUF(10),CHUBUF(10)
      CHARACTER*255 CHTAG ,CHVAL ,CHULIN, CHNAME
      LOGICAL       IEOF
*
#if defined(CERNLIB_IBMMVS)
      CHARACTER*8   PREFIX
#endif
*
* CERN PROGLIB# M442    NAMEFD          .VERSION KERNBIT  1.08  911120
* Author: Jamie Shiers
*
*     Emulation of VM/CMS NAMEFIND command. Process NAMES file
*     looking for the first entry containing the specified
*     input tags and return those requested.
*     CHIN(1,I) = tag name
*     CHIN(2,I) = tag value
*
*     Checks on tag name and value are case insensitive, but
*     values are returned asis.
*
*     Return codes:
*            in case of OPEN error: IOSTAT from FORTRAN OPEN
*            32 - no match found for input tags & values
*            4  - not all requested output tags found
*
*     Mods     Date   Comments
*            95/02/28 Version=1.08    Ignore lines beginning with *
*            93/01/27 Version=1.07    Add debug code, IF=$DEBUG
*            92/10/15 Version=1.06    Fix bug for single character values
*            92/10/09 Version=1.05    Handle multiple tags/line correctly
*            92/06/11 Version=1.04    Look for tags terminated by .
*                     Version=1.03    Open files on VAX READONLY
*                     Version=1.02    Open files on IBM ACTION='READ'
*                     Version=1.01    Open files on VAX shared
*                     Version=1.00    First release
* ------------------------------------------------------------------
      IRC    = 0
      IFOUND = 0

      LF     = LENOCC(CHFILE)
      CHNAME = CHFILE(1:LF)
#if defined(CERNLIB_UNIX)
      CALL CUTOL(CHNAME(1:LF))
#endif
#if defined(CERNLIB_IBMVM)
      CALL CTRANS('.',' ',CHNAME,1,LF)
#endif
#if defined(CERNLIB_IBMMVS)
      IF(CHFILE(1:1).EQ.'.') THEN
         CHNAME = CHFILE(2:LF)
         LF     = LF - 1
      ELSE
         CALL KPREFI(PREFIX,NCHPRE)
         CHNAME = PREFIX(1:NCHPRE)//CHFILE(1:LF)
         LF     = LF + NCHPRE
      ENDIF
      CALL CLTOU(CHNAME(1:LF))
#endif
#if defined(CERNLIB_IBM)
      OPEN(UNIT=LUN,FILE='/'//CHNAME(1:LF),FORM='UNFORMATTED',
     +     ACTION='READ',
#endif
#if !defined(CERNLIB_IBM)
      OPEN(UNIT=LUN,FILE=CHNAME(1:LF),FORM='FORMATTED',
#endif
#if defined(CERNLIB_VAXVMS)
     +     READONLY,
#endif
     +     STATUS='OLD',ACCESS='SEQUENTIAL',IOSTAT=IRC)

      IF(IRC.NE.0) THEN
         PRINT *,'Cannot open ',CHNAME(1:LF),' IOSTAT = ',IRC
         RETURN
      ENDIF
*
*     Loop over all records in the file, looking for the first
*     entry that matches input criteria
*
      IEOF  = .FALSE.
      NBUF  = 1
   10 CONTINUE
#if defined(CERNLIB_IBM)
      READ(LUN,NUM=LLINE,END=100) CHLINE
#endif
#if !defined(CERNLIB_IBM)
      READ(LUN,'(A)',END=100) CHLINE
      LLINE  = LENOCC(CHLINE)
#endif
      IF(LLINE.EQ.0) GOTO 10

#if defined(CERNLIB__DEBUG)
      WRITE(6,9001) CHLINE(1:LLINE)
 9001 FORMAT(' NAMEFD. read line ',A)
#endif
*
*     Ignore lines beginning with a *
*
      IF(CHLINE(1:1).EQ.'*') GOTO 10

      CHULIN = CHLINE
      CALL CLTOU(CHULIN)
*
*     Each element of the file starts with a line containing
*     :nick and ends with the next line containing :nick,
*     or the end-of-file
*
      IF(INDEX(CHULIN(1:LLINE),':NICK').NE.0) THEN
         CHBUF(NBUF) = CHLINE(1:LLINE)
*
*     Now get the remaining records for this element
*
   20    CONTINUE
#if defined(CERNLIB_IBM)
         READ(LUN,NUM=LLINE,END=30) CHLINE
#endif
#if !defined(CERNLIB_IBM)
         READ(LUN,'(A)',END=30) CHLINE
         LLINE  = LENOCC(CHLINE)
#endif
         IF(LLINE.EQ.0) GOTO 20
         IF(CHLINE(1:1).EQ.'*') GOTO 20
#if defined(CERNLIB__DEBUG)
         WRITE(6,9001) CHLINE(1:LLINE)
#endif
         CHULIN = CHLINE
         CALL CLTOU(CHULIN)
         IF(INDEX(CHULIN(1:LLINE),':NICK').NE.0) GOTO 40
         NBUF   = NBUF + 1
         CHBUF(NBUF) = CHLINE(1:LLINE)
         GOTO 20
   30    CONTINUE
         IEOF  = .TRUE.
   40    CONTINUE
*
*     Now have NBUF lines for current element.
*     Look for input tags...
*
         NTAGS = 0
         DO 60 I=1,NBUF
            LBUF = LENOCC(CHBUF(I))
            IF(LBUF.EQ.0) GOTO 60
            DO 50 J=1,NIN
*
*     Does this tag exist in current record?
*
               LTAG = LENOCC(CHIN(1,J))
               CHTAG = CHIN(1,J)(1:LTAG)
               CHUBUF(I) = CHBUF(I)
               CALL CLTOU(CHTAG)
               CALL CLTOU(CHUBUF(I))
#if defined(CERNLIB__DEBUG)
      WRITE(6,9002) CHTAG(1:LTAG),I,CHUBUF(I)(1:LBUF)
 9002 FORMAT(' NAMEFD. hunt for input tag ',A,' in buffer element ',I2,
     +       ' = ',A)
#endif
               ISTART = INDEX(CHUBUF(I)(1:LBUF),CHTAG(1:LTAG)//'.')
               ISTART = INDEX(CHUBUF(I)(1:LBUF),CHTAG(1:LTAG)//'.')
               IF(ISTART.NE.0) THEN
*
*     Does the value match?
*
                  ISTART = ISTART + LTAG + 1
                  IEND = INDEX(CHBUF(I)(ISTART+1:LBUF),' ')
                  IF(IEND.EQ.0) THEN
                     IEND = LBUF
                  ELSE
                     IEND = IEND + ISTART
                  ENDIF
                  CHVAL = CHIN(2,J)
                  LCHVAL = LENOCC(CHIN(2,J))
                  CALL CLTOU(CHVAL)
                  IF(CHVAL(1:LCHVAL).NE.CHUBUF(I)(ISTART:IEND)) THEN
                     GOTO 90
                  ELSE
*
*        Tag matched - increment counter
*
                     NTAGS = NTAGS + 1
                  ENDIF
               ENDIF
   50       CONTINUE
   60    CONTINUE
*
*     Check that we found all the tags we were looking for...
*
         IF(NTAGS.LT.NIN) GOTO 90
         IFOUND = 1
*
*     Now return want the user wanted...
*
         NTAGS = 0
         DO 80 I=1,NBUF
            LBUF = LENOCC(CHBUF(I))
            IF(LBUF.EQ.0) GOTO 80
            DO 70 J=1,NOUT
*
*     Does this tag exist in current record?
*
               LTAG = LENOCC(CHOUT(1,J))
               CHTAG = CHOUT(1,J)(1:LTAG)
               CALL CLTOU(CHTAG)
#if defined(CERNLIB__DEBUG)
      WRITE(6,9003) CHTAG(1:LTAG),I,CHUBUF(I)(1:LBUF)
 9003 FORMAT(' NAMEFD. hunt for output tag ',A,' in buffer element ',I2,
     +       ' = ',A)
#endif
               ISTART = INDEX(CHUBUF(I)(1:LBUF),CHTAG(1:LTAG)//'.')
               IF(ISTART.NE.0) THEN
*
*     Get the value of this tag...
*
                  ISTART = ISTART + LTAG + 1
                  IF(ISTART.GE.LBUF) THEN
                     IEND = LBUF
                  ELSE
                     IEND = INDEX(CHBUF(I)(ISTART+1:LBUF),' :')
                     IF(IEND.EQ.0) THEN
                        IEND = LBUF
                     ELSE
                        IEND = IEND + ISTART
                     ENDIF
                  ENDIF
                  CHOUT(2,J) = CHBUF(I)(ISTART:IEND)
#if defined(CERNLIB__DEBUG)
      WRITE(6,9004) CHTAG(1:LTAG),CHBUF(I)(ISTART:IEND)
 9004 FORMAT(' NAMEFD. value of tag ',A,' is ',A)
#endif
                  NTAGS = NTAGS + 1
               ENDIF
   70       CONTINUE
   80    CONTINUE
*
*     We have found an entry - now return
*
         IF(NTAGS.EQ.NOUT) THEN
            IFOUND = 2
            GOTO 100
         ENDIF
*
*     Entry did not have all the fields we want
*     Look for a better one...
*
   90    CONTINUE
*
*     Anything left to process?
*
         IF(.NOT.IEOF) THEN
            NBUF = 1
            CHBUF(NBUF) = CHLINE(1:LLINE)
            GOTO 20
         ENDIF
*
      ENDIF

  100 CLOSE(LUN)
*
*     No entries found matching search criteria
*
      IF(IFOUND.EQ.0) IRC = 32
      IF(IFOUND.EQ.1) IRC = 4
      END
