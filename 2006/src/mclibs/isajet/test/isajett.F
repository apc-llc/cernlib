      PROGRAM ISAJETT
      EXTERNAL ALDATA
C
      CHARACTER*(*) DFILE
#if defined(CERNLIB_IBMVM)
      PARAMETER (DFILE='/ISAJET DATA *')
#else
#if defined(CERNLIB_VAXVMS)
      PARAMETER (DFILE='CERN_ROOT:[lib]isajet.dat')
#else
      PARAMETER (DFILE='isajet.dat')
#endif
#endif
C
C          MAIN PROGRAM FOR ISAJET
C
#if defined(CERNLIB_GFORTRAN)
      call dummysub(aldata)
#endif
      OPEN(1,FILE=DFILE,STATUS='OLD')
#if defined(CERNLIB_IBMVM)
      OPEN(2,STATUS='SCRATCH',FORM='UNFORMATTED')
#endif
#if 0 
    Note that there is nothing for CERNLIB_VAXVMS!
#endif
#if !defined(CERNLIB_IBMVM) && !defined(CERNLIB_VAXVMS) && !defined(CERNLIB_WINNT)
      OPEN(2,FILE='/dev/null',STATUS='UNKNOWN',FORM='UNFORMATTED')
#endif
#ifdef CERNLIB_WINNT
      OPEN(2,FILE='NUL',STATUS='UNKNOWN',FORM='UNFORMATTED')
#endif
C
      CALL ISAJET(-1,-2,5,6)
C
      END
      SUBROUTINE EXIT
      END
#if defined(CERNLIB_GFORTRAN)
      subroutine dummysub(a)
      return
      end
#endif
