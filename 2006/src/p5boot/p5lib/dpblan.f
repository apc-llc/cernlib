CDECK  ID>, DPBLAN.
      SUBROUTINE DPBLAN (MODE)

C-    MODE = -1: after text just printed, print a blank line
C-            0: if NQLLBL=0, print a blank line
C-            1: if NQLLBL=0, print a blank line before text about to print
C-      also  0: as 1, but assume the text to be printed ends
C-                     with a blank line

      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
C--------------    End CDE              --------------------------------

      IF (MODE.LT.0)               GO TO 27
      IF (MODE.EQ.0)  THEN
          IF (NQLLBL.EQ.0)         GO TO 27
        ELSE
          IF (NQLLBL.EQ.0)         GO TO 28
          NQLLBL = 0
        ENDIF
      RETURN

   27 NQLLBL = 1
   28 WRITE (IQPRNT,'(A)')
      NQUSED = NQUSED + 1
      RETURN
      END
