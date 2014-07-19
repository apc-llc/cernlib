CDECK  ID>, NOSPACE.
      SUBROUTINE NOSPACE (ERRM)

      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
C--------------    End CDE              --------------------------------
      CHARACTER    ERRM*(*)

      IF (ERRM(1:1).NE.'-')  THEN
          WRITE (IQPRNT,9001) ERRM
          IF (IQPRNT.NE.IQTYPE) WRITE (IQTYPE,9001) ERRM
        ENDIF

      CALL MQSHOW

      CALL P_KILL ('Space full')

 9001 FORMAT (/'***!!! Memory problem because ',A)
      END
