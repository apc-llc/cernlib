CDECK  ID>, SMSID.
      SUBROUTINE SMSID (JTYP,JSTR,LINE,JCOL)

C-    Set the stream ID like fort:3
C.    started 5-may-94

      CHARACTER    LINE*(*)


      J = JCOL
      IF (JTYP.EQ.1)  THEN
          LINE(J:J+3) = 'fort'
          J = J + 4
        ELSEIF (JTYP.EQ.2)  THEN
          LINE(J:J+1) = 'cc'
          J = J + 2
        ELSE
          LINE(J:J+1) = 'as'
          J = J + 2
        ENDIF

      LINE(J:J) = ':'
      CALL CSETDI (JSTR, LINE,J+1,J+1)
      RETURN
      END
