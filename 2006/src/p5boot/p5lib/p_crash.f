CDECK  ID>, P_CRASH.
      SUBROUTINE P_CRASH (MSG)

      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
C--------------    End CDE              --------------------------------
      CHARACTER    MSG*(*)

      IF (NQINIT.NE.0)  IQPRNT= IQTYPE
      WRITE (IQTYPE,9000)
      IF (IQPRNT.NE.IQTYPE)  WRITE (IQPRNT,9000)

      CALL P_KILL (MSG)

 9000 FORMAT (/' ****!!!!!   Patchy crashing on a bug   !!!!!****'
     F/' please call for help: zoll@cern.ch')
      END
