CDECK  ID>, P_FATAL.
      SUBROUTINE P_FATAL (MSG)

      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
C--------------    End CDE              --------------------------------
      CHARACTER    MSG*(*)

      IF (NQINIT.NE.0)  IQPRNT= IQTYPE
      WRITE (IQTYPE,9011) MSG
      IF (IQPRNT.NE.IQTYPE)  WRITE (IQPRNT,9011) MSG
      CALL PABEND

 9011 FORMAT (/' ***!!! Fatal error for ',A,' !!!***')
      END
