CDECK  ID>, FLSPLIT.
      SUBROUTINE FLSPLIT (IXFILE,IXDIR,IXFN,IXEXT)

C-    Split the input file name dir/fn.ext into its parts,
C-    return dir/ fn .ext as names with index IXDIR, IXFN, IXEXT
C.    started 17-apr-95

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      PARAMETER     (NSLIFI=128)
      CHARACTER      CHLIFI*(NSLIFI), CHLIEX*8
      COMMON /FLINKC/LUNOP,LUNFD,LUNOLD,LUNSIZ
     +,              IXFLUN, NLIFI,CHLIFI,CHLIEX
C--------------    End CDE              --------------------------------


      CALL FLNAME (IXFILE,JFN)

      JSLA = ICFILA ('/', CHLIFI,1,NLIFI)
      JFN  = NGSLAT + 1

      JDOT = ICFILA ('.', CHLIFI,JFN,NLIFI)
      IF (JDOT.LT.JFN)  JDOT = NLIFI + 1

      IXD = 0
      IXF = 0
      IXX = 0
      IF (JFN.GE.3)       IXD = NA_LONG (CHLIFI(1:JSLA))
      IF (JFN.LT.JDOT)    IXF = NA_LONG (CHLIFI(JFN:JDOT-1))
      IF (JDOT.LT.NLIFI)  IXX = NA_LONG (CHLIFI(JDOT:NLIFI))
      IXDIR = IXD
      IXFN  = IXF
      IXEXT = IXX
      RETURN
      END
