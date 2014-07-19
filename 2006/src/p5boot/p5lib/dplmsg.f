CDECK  ID>, DPLMSG.
      SUBROUTINE DPLMSG (MSG,NCHL,LINE)

C-    Print one line in trouble with a message
C-    LINE  of length NCHL is the text, void if NCHL <= 0
C.    started 25-nov-93

      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
C--------------    End CDE              --------------------------------
      CHARACTER    MSG*(*), LINE*512


      CALL DPBLAN (1)
      NQUSED = NQUSED + 1

C--           LINE is void

      IF (NCHL.LE.0)  THEN
          WRITE (IQPRNT,9024) MSG
          IF (NCHL.LT.0)           GO TO 27
          RETURN
        ENDIF

C--           LINE is present

      NCH = MIN (NCHL,80)
      WRITE (IQPRNT,9024) MSG,LINE(1:NCH)

      IF (NCHL.LE.80)              RETURN

C--           LINE is longer than 80 characters

   27 WRITE (IQPRNT,9027) NCHL
      NQUSED = NQUSED + 1
      RETURN

 9024 FORMAT (1X,A,1X,A)
 9027 FORMAT (7X,'**!! line length is ',I7,' chars')
      END
