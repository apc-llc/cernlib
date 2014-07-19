CDECK  ID>, P_KILL.
      SUBROUTINE P_KILL (MSG)

      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
C--------------    End CDE              --------------------------------
      CHARACTER    MSG*(*)

      IF (NQINIT.NE.0)  IQPRNT= IQTYPE
      WRITE (IQTYPE,9011) MSG
      IF (IQPRNT.NE.IQTYPE)  WRITE (IQPRNT,9011) MSG
      CALL EXITRC (2)

 9011 FORMAT (/' ***!!! Kill the run for: ',A,' !!!***')
      END
