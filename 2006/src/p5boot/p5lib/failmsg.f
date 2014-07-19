CDECK  ID>, FAILMSG.
      SUBROUTINE FAILMSG (JSEVER,NSPACE,NBLANK,MSG)

C-    Print warning/error message, where
C-        JSEVER  severity:  -1 info,  0 warning,  1 error,  2 fatal
C-        NSPACE  = -1  no page-eject to glue to last line printed
C-                   0  no blank line before
C-                  >0  number of lines needed together
C-
C-    return NDSLAT >0  normal printing
C-                   0  suppress printing, maximum number passed

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER      CQBLAN*128, CQEQS*128, CQCETA*32
      CHARACTER      CQAPO*1, CQBSL*1, CQTAB*1
      COMMON /QCHAR/ CQBLAN, CQEQS, CQCETA, CQAPO, CQBSL, CQTAB
      COMMON /QPAGE/ NQLMAX,NQLTOL,NQLTOK,NQCMAX,NQCPGH,NQPAGE
     +,              NQWYLDO,NQWYL,NQNEWH,NQJOIN,NQDKNO,NQDKPG
      CHARACTER      CQDATEM*10, CQDATE*8, CQTIME*5
      COMMON /QSTATE/NQERR,NQWARN,NQINFO,NQLOCK
     +,              IQDATE,IQTIME, CQDATEM,CQDATE,CQTIME
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      COMMON /LUNSLN/IFLAUX, IXLUN(12)
C--------------    End CDE              --------------------------------
      CHARACTER    MSG*(*)


      NN = NSPACE
      NB = MAX (NBLANK, 1)
      NDSLAT = 0
      IF (JSEVER.LT.0)  THEN
          NQINFO = MIN (NQINFO+1, 999)
          IF (NQINFO.GE.199)       RETURN
        ELSEIF (JSEVER.EQ.0)  THEN
          NQWARN = MIN (NQWARN+1, 999)
          IF (NQWARN.GE.199)       RETURN
        ELSE
          NQERR = MIN (NQERR+1, 999)
          IF (NQERR.GE.199)  THEN
              IF (JSEVER.EQ.1)     RETURN
            ENDIF
        ENDIF

      IF (NN.LT.0)                 GO TO 41
      IF (NQNEWH.NE.0)             GO TO 31
      IF (NN+1.LT.NQLTOK)          GO TO 40
      NQUSED = NQLTOK

   31 CALL DPHEAD
      GO TO 41

   40 IF (NN.NE.0)  CALL DPBLAN (1)

   41 IF (JSEVER.LT.0)  THEN
          WRITE (IQPRNT,9041) CQBLAN(1:NB), NQINFO,MSG
        ELSEIF (JSEVER.EQ.0)  THEN
          WRITE (IQPRNT,9042) CQBLAN(1:NB), NQWARN,MSG
        ELSE
          WRITE (IQPRNT,9043) CQBLAN(1:NB), NQERR,MSG
        ENDIF
      NQUSED = NQUSED + 1
      NQLLBL = 0
      NDSLAT = 7
      IF (JSEVER.NE.1)             RETURN
      IF (NQERR.NE.1)              RETURN
      IF (IFLAUX.NE.0)             RETURN
      WRITE (IQPRNT,9049) CQBLAN(1:NB)
      NQUSED = NQUSED + 1
      RETURN

 9041 FORMAT (A,'!   Inform',I4,'  *! ',A,' !*')
 9042 FORMAT (A,'!! Warning',I4,'  **!! ',A,' !!**')
 9043 FORMAT (A,'!!!  Error',I4,'  ***!!! ',A,' !!!***    !!!!!!!!')
 9049 FORMAT (A,'!!!  EXE processing is inhibited from now on  !!!')
      END
