*
* $Id: ffgo.F,v 1.1.1.1 1996/03/08 11:50:42 mclareni Exp $
*
* $Log: ffgo.F,v $
* Revision 1.1.1.1  1996/03/08 11:50:42  mclareni
* Ffread
*
*
#include "ffread/pilot.h"
      SUBROUTINE FFGO
C
C      FUNCTIONAL DESCRIPTION:
C
C              This routine actually reads input from the specified
C              unit(s), interprets the data cards, and modifies the
C              corresponding variables.
C
C      DUMMY ARGUMENTS:
C
C              NONE
C
C      IMPLICIT INPUTS:
C
C              The common CFREAD contains the keys the user has defined.
C              Keywords LIST, NOLIST, READ, WRITE, END, STOP, KEYS, HOLL,
C              and C have predefined actions.
C
C      IMPLICIT OUTPUTS:
C
C              The variables given in the key definition are modified
C              as indicated on the data cards.
C
C      SIDE EFFECTS:
C
C              If the predefined command READ is given, an indirect input
C              file is opened and reading continues from this file.
C
#include "ffread/ffmach.inc"
#include "ffread/ffluns.inc"
#include "ffread/cfread.inc"
#include "ffread/ffcomm.inc"
#include "ffread/ffchar.inc"
      INTEGER FFFIND
 
      CHARACTER REST*(NCHWD-1), TXTSEP*1
 
      CHARACTER OUTBUF*116
 
      CHARACTER ERRHDR*48
      PARAMETER (ERRHDR =
     * ' -------------------- DATA CARD ERROR -----     ')
 
      CHARACTER BLANK*26
      INTEGER IBUF((NCHMAX+NCHWD-1)/NCHWD)
#if defined(CERNLIB_UPLOW)
 
      CHARACTER CHECK*5
#endif
 
      LOGICAL STRING, CALUSR, LISKEY, TITLE, LIST, STOP, SHIFT
 
      PARAMETER (IBFLEN = LENMAX/2)
      INTEGER LENBUF(IBFLEN), LOCBUF(IBFLEN)
      INTEGER IVALUS(IBFLEN)
      REAL    RVALUS(IBFLEN)
      EQUIVALENCE (IVALUS(1), RVALUS(1)), (IVALUE, RVALUE)
 
      PARAMETER (NSYSKY = 10)
      CHARACTER SYSKEY(NSYSKY)*6
      INTEGER LKYMIN(NSYSKY), LKYMAX(NSYSKY)
C
C        The fact that for END and STOP their index is their correct
C        length plus one is used at labels 150 and 907.
C        The index of system keys 2 to 4 is used at label 900.
C
      DATA SYSKEY / 'C     ', 'LIST  ', 'READ  ',
     *              'END   ', 'STOP  ', 'NOLIST',
     *              'WRITE ', 'HOLL  ', 'KEYS  ',
     *              '*     '/
      DATA LKYMIN / 1, 4, 4, 3, 4, 4, 4, 4, 4, 1 /,
     *     LKYMAX / 2, 4, 4, 3, 4, 6, 5, 4, 4, 2 /
 
      DATA BLANK / ' ' /
C
C----------------- Beginning of executable statements -------------------------
C
C        Check that we have at least one key
C
      IF (NKEYS .LE. 0) THEN
          WRITE (UNIT=LUNOUT, FMT=1011)
          RETURN
      END IF
C
C        Initialize some variables
C        LINPUT is copied to a local variable to aid compiler optimization.
C
      LIST   = .FALSE.
      TITLE  = .FALSE.
      LISKEY = .FALSE.
      STRING = .FALSE.
      STOP   = .FALSE.
      CALUSR = .FALSE.
      KURKEY = 0
      FFFLAG = 0.
      TXTSEP = ''''
      IBFSIZ = LINPUT
C
C        Reset variables and read in a data card
C
   1  KURSOR = 1
      IERROR = 0
      ICOUNT = 0
      SHIFT  = .FALSE.
 
      IF (LUNIN.LT.0) THEN
        CALL JUMPST(KFJUMP(-LUNIN))
        CALL JUMPX1(IBFSIZ)
      ELSE
CSELF,IF= IBMVM.
C       READ (UNIT=LUNIN, NUM=NBYT, IOSTAT=IOS) BUFFER(1:IBFSIZ)
C       IF(IOS.LT.0) GOTO 80
C       BUFFER(NBYT+1:IBFSIZ)=' '
CSELF,IF=-IBMVM.
        READ (UNIT=LUNIN, FMT=2001, END=80) BUFFER(1:IBFSIZ)
CSELF.
      END IF
      IF (STRING) GO TO 150
C
C        Parse first item.
C
      CALL FFCARD (BUFFER(1:IBFSIZ), KURSOR, ISYMB,
     *             KURSOR, LENGTH, IVALUE)
C
C        A blank card will be ignored.
C
      IF (ISYMB .EQ. 6) GO TO 900
C
C        If first non-blank character of card is '*',
C        it can't be a continuation card. In this case,
C        get the key, if this is not a comment
C
      IF (ISYMB .EQ. 2 .AND. BUFFER(KURSOR:KURSOR) .EQ. '*') THEN
          IF(BUFFER(KURSOR+1:KURSOR+1) .NE. ' ') THEN
            KURSOR = KURSOR + 1
            SHIFT  = .TRUE.
            CALL FFCARD (BUFFER(1:IBFSIZ), KURSOR, ISYMB,
     *                      KURSOR, LENGTH, IVALUE)
          END IF
      ELSE
C
C        If it seems to be a continuation card, we must have a current key
C
         IF (ISYMB .NE. 1) THEN
             IF (KURKEY .NE. 0 .AND. .NOT. SHIFT) THEN
                 GO TO 121
             ELSE
                 IF (SHIFT) THEN
                     SHIFT  = .FALSE.
                     IERROR =  5
                 ELSE
                     IERROR = 10
                 END IF
                 GO TO 900
             END IF
         END IF
      END IF
C
C        It's a symbol - convert it to uppercase if necessary.
C
C        Check for a system key.
C        The comparison in the loop below ensures that a system key
C        is compared with the token:
C         -  at least for its minimum length
C         -  at most  for its maximum length
C         -  at most  for the user-defined key length.
C
#if defined(CERNLIB_UPLOW)
      CALL FFUPCA (BUFFER, KURSOR, LENGTH)
 
#endif
      DO 2 IKEY = 1, NSYSKY
          IF (LENGTH .GE. LKYMIN(IKEY)) THEN
              ITMP = MIN(LENGTH, NCHKEY)
              IF (LKYMAX(IKEY) .GE. LENGTH) THEN
                  IF (BUFFER(KURSOR:KURSOR+ITMP-1) .EQ.
     *                SYSKEY(IKEY)(1:ITMP)) GO TO 3
              ELSE IF (ITMP .NE. LENGTH) THEN
                  IF (BUFFER(KURSOR:KURSOR+LKYMAX(IKEY)-1) .EQ.
     *                SYSKEY(IKEY)) GO TO 3
              END IF
          END IF
   2  CONTINUE
 
      GO TO 100
C
C        We have found a system key.
C        If it is 'C' or a '*', ignore this card completely.
C        Otherwise, dispatch after calling FFUSER
C        for last key if necessary.
C
   3  IF (IKEY .EQ. 1 .OR. IKEY .EQ. 10) GO TO 900
 
      KURSOR = KURSOR + LENGTH
 
   4  IF (CALUSR) THEN
        CALL JUMPST(KUJUMP)
        CALL JUMPX1(IWORDS(NOFKEY+KURKEY*NWDKEY))
      END IF
      CALUSR = .FALSE.
      KURKEY = 0
      GO TO (10, 20, 30, 40, 50, 20, 60, 70), IKEY - 1
C
C        'LIST' - turn on listing, give title if not yet done
C
  10  LIST   = .TRUE.
 
      IF (.NOT. TITLE) THEN
          WRITE (UNIT=LUNOUT, FMT=2104)
          TITLE = .TRUE.
      END IF
 
      GO TO 90
C
C        'READ' or 'WRITE' - change input/output LUNs.
C        For 'READ', a stack is used.
C
C        Get LUN, check it's an integer and in the correct range.
C
  20  CALL FFCARD (BUFFER(1:IBFSIZ), KURSOR, ISYMB,
     *             KURSOR, LENGTH, IVALUE)
 
      IF (ISYMB .NE. 3) THEN
          IERROR = 2
          GO TO 90
      END IF
 
      IF(IKEY.EQ.3) THEN
        LUNMIN = -NFJUMP
      ELSE
        LUNMIN = 0
      END IF
 
      IF (IVALUE .LT. LUNMIN .OR. IVALUE .GT. LUNMAX) THEN
          IERROR = 3
          GO TO 90
      END IF
C
C        Now store value; for 'READ', check for space on stack
C
      IF (IKEY .EQ. 3) THEN
C
C        In case of user reading routine, check if address
C        initialized
C
          IF(IVALUE.LT.0) THEN
              IF(KFJUMP(-IVALUE).EQ.IQANIL) THEN
                  IERROR = 11
                  GO TO 90
              END IF
          END IF
 
          IF (ISTACK .LT. LSTACK) THEN
              ISTACK = ISTACK + 1
              LUNS(ISTACK) = LUNIN
              LUNIN = IVALUE
          ELSE
              IERROR = 4
          END IF
      ELSE
          LUNOUT = IVALUE
      END IF
 
      GO TO 90
C
C        'END' - if indirect file, pop stack, else same as 'STOP'
C
  30  IF (ISTACK .GT. 0) THEN
  35      LUNIN = LUNS(ISTACK)
          ISTACK = ISTACK - 1
          IF(LUNIN.LT.0) THEN
              IF(KFJUMP(-LUNIN).EQ.IQANIL) THEN
                  IERROR = 11
                  GO TO 35
              END IF
          END IF
          GO TO 90
      END IF
C
C        'STOP', or 'END' on primary input
C
  40  STOP   = .TRUE.
      GO TO 90
C
C        'NOLIST' - turn off listing
C
  50  LIST   = .FALSE.
      GO TO 90
C
C        'HOLL' - change text seperator, must be a special character
C
  60  CALL FFCARD (BUFFER(1:IBFSIZ), KURSOR, ISYMB,
     *             KURSOR, LENGTH, IVALUE)
 
      IF (ISYMB .EQ. 2) THEN
          IF (BUFFER(KURSOR:KURSOR) .EQ. '*'  .OR.
     *        BUFFER(KURSOR:KURSOR) .EQ. '=') THEN
              IERROR = 1
          ELSE
              TXTSEP = BUFFER(KURSOR:KURSOR)
          END IF
      ELSE
          IERROR = 1
      END IF
 
      GO TO 90
C
C        'KEYS' - give listing of all keys at end of reading
C
  70  LISKEY = .TRUE.
      GO TO 90
C
C        End-of-file on input: give error if reading string.
C        Then treat same as 'END'.
C
  80  IF (STRING) THEN
          STRING = .FALSE.
          IERROR = 7
      END IF
 
      IKEY   = 4
      BUFFER = 'END'
      GO TO 4
C
C        Check whether user wants to be called for this key.
C        That's it for a system key.
C
  90  IF (SHIFT) THEN
          CALL UCTOH  (SYSKEY(IKEY)//BLANK, IBUF, NCHWD, NCHKEY)
          CALL JUMPST(KUJUMP)
CF90     Pass explicitely the dimensionality to bypass argument checking
          CALL JUMPX1(IBUF(1))
      END IF
 
      GO TO 900
C
C        We might have a user key here.
C
 100  ITMP0 = FFFIND (BUFFER(KURSOR:KURSOR+LENGTH-1))
      IF (ITMP0 .GT. 0) GO TO 101
C
C        This is an unknown command. Ignore it with error message.
C
      IERROR = 8
      GO TO 900
C
C        New key, wind up old one and reset variables
C
 101  IF (CALUSR) THEN
        CALL JUMPST(KUJUMP)
        CALL JUMPX1(IWORDS(NOFKEY+KURKEY*NWDKEY))
      END IF
      CALUSR = SHIFT
      KURKEY = ITMP0
      LOC    = 1
C
C        Update cursor. Only action if item was text or special character
C
 110  KURSOR = KURSOR + LENGTH
C
C        Loop here over all items on the card
C
 120  CALL FFCARD (BUFFER(1:IBFSIZ), KURSOR, ISYMB,
     *             KURSOR, LENGTH, IVALUE)
 
      IF (ISYMB .EQ. 1) GO TO 110
 121  IF (ISYMB .EQ. 2) GO TO 130
      IF (ISYMB .EQ. 6) GO TO 600
 
      KURSOR         = KURSOR + LENGTH
      ICOUNT         = ICOUNT + 1
      LENBUF(ICOUNT) = 1
      LOCBUF(ICOUNT) = LOC
 
      GO TO (200, 300, 400), ISYMB - 2
C
C        Special character. If it's the text seperator,
C        start storing a text. Otherwise, consider it
C        part of a comment and ignore it.
C
 130  IF (BUFFER(KURSOR:KURSOR) .NE. TXTSEP) GO TO 110
C
C        Check whether we're reading a logical key -
C        strings are not allowed in this case.
C
 131  IF (IWORDS(NOFTYP+KURKEY) .EQ. 3) THEN
          IERROR = 6
          GO TO 900
      END IF
C
C        Take everything up to next seperator
C
      KURSOR = KURSOR + 1
      IF (KURSOR .GT. IBFSIZ) GO TO 132
#if !defined(CERNLIB_VAX)
      LSTRNG = INDEX (BUFFER(KURSOR:IBFSIZ), TXTSEP)
#endif
#if defined(CERNLIB_VAX)
      LSTRNG = LIB$LOCC (TXTSEP, BUFFER(KURSOR:IBFSIZ))
#endif
      IF (LSTRNG .NE. 0) GO TO 140
C
C        Text overflows to next card
C        Do necessary housekeeping.
C
 132  STRING  = .TRUE.
      LSTRNG  = IBFSIZ - KURSOR + 1
 
      IF (LSTRNG .EQ. 0) THEN
          LREST = 0
          GO TO 600
      END IF
 
      LREST = LSTRNG - (LSTRNG / NCHWD) * NCHWD
 
      IF (LREST .NE. 0) THEN
          REST   = BUFFER(IBFSIZ-LREST+1:IBFSIZ)
          LSTRNG = LSTRNG - LREST
      END IF
 
      GO TO 141
C
C        We have a text item. Store it - empty string is a no-op.
C
 140  LSTRNG = LSTRNG - 1
 
      IF (LSTRNG .EQ. 0) THEN
          KURSOR = KURSOR + 1
          GO TO 120
      END IF
 
 141  ICOUNT = ICOUNT + 1
      CALL UCTOH (BUFFER(KURSOR:KURSOR+LSTRNG-1),
     *            IVALUS(ICOUNT), NCHWD, LSTRNG)
      ITMP1 = (LSTRNG + NCHWD - 1) / NCHWD - 1
 
      DO 142 ITMP2 = ICOUNT, ICOUNT + ITMP1
          LENBUF(ITMP2) = 1
          LOCBUF(ITMP2) = LOC
          LOC           = LOC + 1
 142  CONTINUE
 
      ICOUNT = ICOUNT + ITMP1
C
C        If the string wasn't terminated, we're already done with this card.
C        Else, update cursor and continue parsing.
C
      IF (STRING) GO TO 600
      KURSOR = KURSOR + LSTRNG + 1
      GO TO 500
C
C        A string has been continued.
C        Check whether first item is END or STOP.
C
 150  IKEY = 0
#if defined(CERNLIB_UPLOW)
      CHECK = BUFFER(1:5)
      CALL FFUPCA (CHECK, 1, 5)
      IF (CHECK(1:4) .EQ. 'END ' ) IKEY = 4
      IF (CHECK(1:5) .EQ. 'STOP ') IKEY = 5
#endif
#if !defined(CERNLIB_UPLOW)
      IF (BUFFER(1:4) .EQ. 'END ' ) IKEY = 4
      IF (BUFFER(1:5) .EQ. 'STOP ') IKEY = 5
#endif
      IF (IKEY .NE. 0) THEN
          IERROR = 7
#if defined(CERNLIB_UPLOW)
          BUFFER(1:IKEY) = CHECK(1:IKEY)
#endif
          STRING = .FALSE.
          GO TO 4
      END IF
C
C        Try to find the end of it and deal with leftovers.
C
#if !defined(CERNLIB_VAX)
      LSTRNG = INDEX (BUFFER(1:IBFSIZ), TXTSEP)
#endif
#if defined(CERNLIB_VAX)
      LSTRNG = LIB$LOCC (TXTSEP, BUFFER(1:IBFSIZ))
#endif
 
      IF (LREST .EQ. 0 .AND. LSTRNG .EQ. 1) THEN
          KURSOR = 2
          STRING = .FALSE.
          GO TO 120
      END IF
 
      ICOUNT         = 1
      LENBUF(ICOUNT) = 1
      LOCBUF(ICOUNT) = LOC
      LOC            = LOC + 1
 
      IF (LREST+LSTRNG .LE. NCHWD) THEN
          CALL UCTOH (REST(1:LREST)//BUFFER(1:LSTRNG),
     *                IVALUS(ICOUNT), NCHWD, LREST+LSTRNG)
          KURSOR = LSTRNG + 1
          GO TO 500
      END IF
 
      CALL UCTOH (REST(1:LREST)//BUFFER(1:NCHWD-LREST),
     *            IVALUS(ICOUNT), NCHWD, NCHWD)
C
C        Now set up to copy the rest of the card.
C
      KURSOR = NCHWD - LREST + 1
 
      IF (LSTRNG .NE. 0) THEN
          LSTRNG = LSTRNG - KURSOR
          STRING = .FALSE.
      ELSE
          LSTRNG = IBFSIZ - KURSOR
      END IF
 
      GO TO 141
C
C        Found an integer item.
C        Check whether it's an address or repeat count.
C
 200  CALL FFSKIP (BUFFER(1:IBFSIZ), KURSOR)
      IF (KURSOR .GT. IBFSIZ)                   GO TO 230
      IF (      BUFFER(KURSOR:KURSOR) .EQ. '*') GO TO 220
      IF (.NOT. BUFFER(KURSOR:KURSOR) .EQ. '=') GO TO 230
C
C        This is an address. Validate it and get next field.
C        Next field may be integer, real, logical, or string.
C
      IF (IVALUE .LE. 0 .OR. IVALUE .GT. IWORDS(NOFLEN+KURKEY)) THEN
          LOC    = IVALUE + 1
          IERROR = 9
          GO TO 900
      END IF
 
      LOCBUF(ICOUNT) = IVALUE
      LOC            = IVALUE
      KURSOR         = KURSOR + 1
      CALL FFCARD (BUFFER(1:IBFSIZ), KURSOR, ISYMB,
     *             KURSOR, LENGTH, IVALUE)
 
      IF (ISYMB .EQ. 1 .OR. ISYMB .EQ. 6) THEN
          IERROR = 5
          GO TO 900
      END IF
 
      IF (ISYMB  .EQ. 2) THEN
          IF (BUFFER(KURSOR:KURSOR) .EQ. TXTSEP) THEN
              GO TO 131
          ELSE
              IERROR = 5
              GO TO 900
          END IF
      END IF
 
      KURSOR = KURSOR + LENGTH
      GO TO (300, 400), ISYMB - 3
C
C         Another integer. Check for a repeat count.
C
      CALL FFSKIP (BUFFER(1:IBFSIZ), KURSOR)
      IF (KURSOR .GT. IBFSIZ)             GO TO 230
      IF (BUFFER(KURSOR:KURSOR) .NE. '*') GO TO 230
C
C        This is a repeat count. Check it is positive and get next field.
C        Strings are not allowed to have repeat counts.
C
 220  IF (IVALUE .LE. 0) THEN
          IERROR = 5
          GO TO 900
      END IF
 
      LENBUF(ICOUNT) = IVALUE
      KURSOR         = KURSOR + 1
      CALL FFCARD (BUFFER(1:IBFSIZ), KURSOR, ISYMB,
     *             KURSOR, LENGTH, IVALUE)
 
      IF (ISYMB .LT. 3 .OR. ISYMB .EQ. 6) THEN
          IERROR = 5
          GO TO 900
      END IF
 
      KURSOR = KURSOR + LENGTH
      GO TO (300, 400), ISYMB - 3
C
C        Integer value. Check type and convert if necessary.
C
 230  GO TO (240, 240, 250, 260), IWORDS(NOFTYP+KURKEY)+1
 
 240  IVALUS(ICOUNT) = IVALUE
      GO TO 500
 
 250  RVALUS(ICOUNT) = REAL(IVALUE)
      GO TO 500
 
 260  IERROR = 6
      GO TO 900
C
C        Floating point value. Check type and convert if necessary.
C
 300  GO TO (320, 310, 320, 330), IWORDS(NOFTYP+KURKEY)+1
 
 310  IVALUS(ICOUNT) = INT(RVALUE)
      GO TO 500
 
 320  RVALUS(ICOUNT) = RVALUE
      GO TO 500
 
 330  IERROR = 6
      GO TO 900
C
C        Logical value. Error if for floating point key.
C
 400  IF (IWORDS(NOFTYP+KURKEY) .NE. 2) THEN
          IVALUS(ICOUNT) = IVALUE
      ELSE
          IERROR = 6
          GO TO 900
      END IF
C
C        Correct location counter before we parse next item.
C        At the same time, check that we haven't exceeded
C        the allowed address range.
C
 500  LOC    = LOCBUF(ICOUNT) + LENBUF(ICOUNT)
 
      IF (LOC-1 .GT. IWORDS(NOFLEN+KURKEY)) THEN
          IERROR = 9
          GO TO 900
      END IF
 
      IF (KURSOR .LE. IBFSIZ) GO TO 120
C
C        End of card - now actually store the values.
C
C        If no value given, store 1, 1.0, and .TRUE., respectively,
C        depending on type of key, into current location.
C
C        This only differs from element 1 if the only item
C        on this card is the end of a continued text.
C
 600  IF (ICOUNT .NE. 0) THEN
          NOFF1 = IWORDS(NOFLOC+KURKEY)
          DO 601 ITMP5 = 1, ICOUNT
              NOFF2 = NOFF1 + LOCBUF(ITMP5) - 1
              DO 602 ITMP6 = 0, LENBUF(ITMP5) - 1
                  IWORDS(NOFF2+ITMP6) = IVALUS(ITMP5)
 602          CONTINUE
 601      CONTINUE
      ELSE
          ITMP7 = IWORDS(NOFLOC+KURKEY) + LOC - 1
          GO TO (611, 612, 613), IWORDS(NOFTYP+KURKEY)
 
 611          IWORDS(ITMP7) = 1
              GO TO 900
 
 612          RVALUS(1)   = 1.
              CALL UCOPY2 (RVALUS(1), IWORDS(ITMP7), 1)
              GO TO 900
 
CF90  The call to UCOPY2 was confusing the compiler
 613          CALL UCOPY  (.TRUE., IWORDS(ITMP7), 1)
              GO TO 900
      END IF
C
C        Here we print the data card and any error messages,
C        if so required. Indent data cards according to 'READ'
C        nesting level.
C
 900  IF (LIST) THEN
          IF (IKEY .EQ. 3) THEN
              ITMP8 = ISTACK + 1
          ELSE IF ((IKEY .EQ. 4 .OR. IKEY .EQ. 5) .AND. .NOT. STOP) THEN
              ITMP8 = ISTACK + 3
          ELSE
              ITMP8 = ISTACK + 2
          END IF
          IF (SHIFT) ITMP8 = ITMP8 - 1
 
          WRITE (UNIT=LUNOUT, FMT=2105) BLANK(1:ITMP8)//BUFFER(1:IBFSIZ)
 
          IF (IERROR .NE. 0) THEN
              FFFLAG = FFFLAG + 1.
              GO TO (901, 902, 903, 904, 905,
     *               906, 907, 908, 909, 910, 915), IERROR
          END IF
 
      END IF
C
C     Check for end of input
C
      IF (.NOT. STOP) GO TO 1
      GO TO  920
 
 901  WRITE (UNIT=LUNOUT, FMT=1001) ERRHDR
      IF (.NOT. STOP) GO TO 1
      GO TO  920
 
 902  WRITE (UNIT=LUNOUT, FMT=1002) ERRHDR
      IF (.NOT. STOP) GO TO 1
      GO TO  920
 
 903  WRITE (UNIT=LUNOUT, FMT=1003)  ERRHDR, IVALUE
      IF (.NOT. STOP) GO TO 1
      GO TO  920
 
 904  WRITE (UNIT=LUNOUT, FMT=1004)  ERRHDR, IVALUE
      IF (.NOT. STOP) GO TO 1
      GO TO  920
 
 905  WRITE (UNIT=LUNOUT, FMT=1005) ERRHDR
      IF (.NOT. STOP) GO TO 1
      GO TO  920
 
 906  WRITE (UNIT=LUNOUT, FMT=1006) ERRHDR
      IF (.NOT. STOP) GO TO 1
      GO TO  920
 
 907  WRITE (UNIT=LUNOUT, FMT=1007) ERRHDR, SYSKEY(IKEY)(1:IKEY-1)
      IF (.NOT. STOP) GO TO 1
      GO TO  920
 
 908  WRITE (UNIT=LUNOUT, FMT=1008) ERRHDR
      IF (.NOT. STOP) GO TO 1
      GO TO  920
 
 909  WRITE (UNIT=LUNOUT, FMT=1009) ERRHDR, LOC-1
      IF (.NOT. STOP) GO TO 1
      GO TO 920
 
 910  WRITE (UNIT=LUNOUT, FMT=1010) ERRHDR
      IF (.NOT. STOP) GO TO 1
C
 915  WRITE (UNIT=LUNOUT, FMT=1012) ERRHDR, LUNIN
      IF (.NOT. STOP) GO TO 1
C
C        End of reading.
C        List keys if required
C
 920  IF (LISKEY) THEN
          WRITE (UNIT=LUNOUT, FMT=2101)
          MAXPOS = LEN (OUTBUF) - NCHKEY + 1
          OUTBUF = ' FFREAD KEYWORDS'
          IPOS   = 29
 
          DO 921 ITMP9 = 1, NSYSKY
              OUTBUF(IPOS:IPOS+NCHKEY-1) = SYSKEY(ITMP9)
              IPOS = IPOS + NCHKEY + 1
              IF (IPOS .GT. MAXPOS) THEN
                  WRITE (UNIT=LUNOUT, FMT=2103) OUTBUF
                  OUTBUF = ' '
                  IPOS = 29
              END IF
 921      CONTINUE
          IF (IPOS .NE. 29) WRITE (UNIT=LUNOUT, FMT=2103) OUTBUF
 
          WRITE (UNIT=LUNOUT, FMT=2102)
          OUTBUF = ' USER   KEYWORDS'
          IPOS   = 29
 
          DO 922 ITMP9 = 1, NKEYS
              CALL UHTOC (IWORDS(NOFKEY+ITMP9*NWDKEY), NCHWD,
     *                    OUTBUF(IPOS:IPOS+NCHKEY-1), NCHKEY)
              IPOS = IPOS + NCHKEY + 1
              IF (IPOS .GT. MAXPOS) THEN
                  WRITE (UNIT=LUNOUT, FMT=2103) OUTBUF
                  OUTBUF = ' '
                  IPOS = 29
              END IF
 922      CONTINUE
          IF (IPOS .NE. 29) WRITE (UNIT=LUNOUT, FMT=2103) OUTBUF
 
      END IF
C
C        All done !
C
      RETURN
C
C        FORMATs for all error messages.
C
1001  FORMAT (A,
     * 'KEY ''HOLL'' MUST BE FOLLOWED BY SPECIAL CHARACTER ',
     * 'NEITHER * NOR =')
1002  FORMAT (A,
     * 'KEY ''READ''/''WRITE'' MUST BE FOLLOWED BY AN INTEGER')
1003  FORMAT (A,
     * 'LUN', I12, ' FOR ''READ''/''WRITE'' OUT OF RANGE')
1004  FORMAT (A,
     * 'CAN''T READ FROM UNIT ', I3, ' BECAUSE OF STACK OVERFLOW')
1005  FORMAT (A,
     * 'SYNTAX ERROR')
1006  FORMAT (A,
     * 'TYPE MISMATCH OF KEY DECLARATION AND VALUE')
1007  FORMAT (A,
     * 'SECOND DELIMITER NOT FOUND. SCAN TERMINATED BY ''', A, '''')
1008  FORMAT (A,
     * 'COMMAND NOT FOUND IN DICTIONARY')
1009  FORMAT (A,
     * 'ADDRESS', I12, ' OUT OF RANGE')
1010  FORMAT (A,
     * 'DECK CANNOT START BY A CONTINUATION CARD')
1011  FORMAT
     * (' FFGO  --- NO KEYS ARE CURRENTLY DEFINED.'/
     *  '           YOU MUST BE JOKING!')
1012  FORMAT(A,
     *' FFGO  --- UNSET USER READ ROUTINE - UNIT ',I3,' SKIPPED.')
C
C        FORMAT for input.
C
2001  FORMAT (A)
C
C        FORMATs for output.
C
2101  FORMAT (/)
2102  FORMAT ( )
2103  FORMAT (' ', A)
2104  FORMAT ('1           USER''S DIRECTIVES TO RUN THIS JOB'/
     *        '            ----------------------------------'//)
2105  FORMAT (' ***** DATA CARD CONTENT   ', A)
 
      END
