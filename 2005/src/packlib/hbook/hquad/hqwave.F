*
* $Id: hqwave.F,v 1.1.1.1 1996/01/16 17:08:07 mclareni Exp $
*
* $Log: hqwave.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:07  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.18/00 02/02/93  09.32.29  by  John Allison
*-- Author :
      SUBROUTINE HQWAVE (CHID, TAGS, CHISQ, ALOGLI, NNX, NNY, NNZ,
     +LUWAVE, LUAVSH, LUAVSF)
      CHARACTER*(*) CHID, TAGS (*)
      INTEGER NNX, NNY, NNZ, LUWAVE, LUAVSH, LUAVSF
      REAL CHISQ, ALOGLI
* Output 3-D histogram and fitted function in Wavefront's Data Visualiser
*   format.  Also write an AVS .fld files (one for the histogram, one for the
*   function) to read it.
* If you have just completed multiquadric smoothing and it will write out the
*   histogram contents.  If you pick up a previously smoothed ntuple from a
*   .hbook file, the histogram will not be present.  If you smooth one ntuple,
*   then pick another up from a .hbook file, you may get the wrong histogram
*   contents.
* CHID is a character identification, which (it is suggested) should be the
*   filename "basename".
* LUWAVE, LUAVSH and LUAVSF are the logical nos.  It is suggested that files are
*   named (with the OPEN statement in the calling routine) to:
*   LUWAVE: filename = basename.wave
*   LUAVSH: filename = basename_hist.fld
*   LUAVSF: filename = basename_func.fld
* If NNX, etc. is different to NX, etc., the function is written on a
*   grid defined by NNX, etc., and the histogram is not written.
 
#include "hbook/hcqcom.inc"
#include "hbook/hcbook.inc"
 
      CHARACTER*80 CHQMES, CHID1
      LOGICAL HTHERE, FTHERE
      CHARACTER*40 CHTITL
      INTEGER L, IX, IY, IZ, NCHX, NCHY, NCHZ
      INTEGER LENOCC, LCHID, LCHTIT, NLINES
      REAL V (3), X, Y, Z, DDX, DDY, DDZ
      EQUIVALENCE (X, V (1)), (Y, V(2)), (Z, V(3))
      REAL HQF
 
      IF (NDIM .NE. 3) GO TO 70
 
      IF (NSIG .GT. 0) THEN
         FTHERE = .TRUE.
      ELSE
         FTHERE = .FALSE.
      END IF
 
      IF (NNX .EQ. NX .AND. NNY .EQ. NY .AND. NNZ .EQ. NZ) THEN
         HTHERE = .TRUE.
      ELSE
         HTHERE = .FALSE.
      END IF
 
      IF (.NOT. HTHERE .AND. .NOT. FTHERE) GO TO 80
 
      DDX = DXT / NNX
      DDY = DYT / NNY
      DDZ = DZT / NNZ
 
      LCHID = LENOCC (CHID)
      CHID1 = CHID
 
      NCHZ = LENOCC (TAGS (1))
      NCHY = LENOCC (TAGS (2))
      NCHX = LENOCC (TAGS (3))
      CHTITL = TAGS (3) (1: NCHZ) // '%' // TAGS (2) (1: NCHY)
     +// '%' // TAGS (1) (1: NCHX)
      LCHTIT = LENOCC (CHTITL)
 
#if defined(CERNLIB_UNIX)
      CALL CUTOL (CHID1)
      CALL CUTOL (CHTITL)
#endif
 
* Write .wave file for Wavefront's Data Visualiser.
      WRITE (LUWAVE,
     +'(''# Multiquadric data for Wavefront''''s Data Visualiser.'')')
      WRITE (LUWAVE, '(''# Identifier '', A)') CHID1 (1: LCHID)
      WRITE (LUWAVE, '(''# Tags '', A)') CHTITL (1: LCHTIT)
      WRITE (LUWAVE, '(''# No. of multiquadric parameters'', I6)') NSIG
      WRITE (LUWAVE, '(''# Chi-squared'', G12.5, '' for'', I6,
     +'' degrees of freedom.'')') CHISQ, NBTOT - NSIG
      WRITE (LUWAVE, '(''# Log likelihood'', G12.5)') ALOGLI
 
      WRITE (LUWAVE, '(/)')
      WRITE (LUWAVE, '(''define mesh mquad_'', A)') CHTITL (1: LCHTIT)
      WRITE (LUWAVE, '('' mesh_topology mquad_topology'')')
      WRITE (LUWAVE, '('' mesh_grid mquad_grid'')')
 
      WRITE (LUWAVE, '(/)')
      WRITE (LUWAVE, '(''define reg_grid mquad_grid'')')
      WRITE (LUWAVE, '('' grid_samp'', 3I5)') NNX, NNY, NNZ
      WRITE (LUWAVE, '('' origin'', 3G12.5)') XMI + DX / 2.,
     + YMI + DY / 2., ZMI + DZ / 2.
      WRITE (LUWAVE, '('' step'', 3G12.5)') DDX, DDY, DDZ
 
      WRITE (LUWAVE, '(/)')
      WRITE (LUWAVE, '(''define reg_topology mquad_topology'')')
      WRITE (LUWAVE, '('' elem_samp'', 3I5)') NNX - 1, NNY - 1, NNZ - 1
 
      NLINES = 21
 
      IF (HTHERE) THEN
         WRITE (LUWAVE, '(/)')
         WRITE (LUWAVE, '(''define volume mquad_histogram'')')
         WRITE (LUWAVE, '('' volume_mesh mquad_'', A)')
     +   CHTITL (1: LCHTIT)
         WRITE (LUWAVE, '('' volume_vdata mquad_histogram_contents'')')
         NLINES = NLINES + 5
      END IF
 
      IF (FTHERE) THEN
         WRITE (LUWAVE, '(/)')
         WRITE (LUWAVE, '(''define volume mquad_function'')')
         WRITE (LUWAVE, '('' volume_mesh mquad_'', A)')
     +   CHTITL (1: LCHTIT)
         WRITE (LUWAVE, '('' volume_vdata mquad_function_value'')')
         NLINES = NLINES + 5
      END IF
 
      IF (HTHERE .AND. .NOT. FTHERE) THEN
         WRITE (LUWAVE, '(/)')
         WRITE (LUWAVE, '(''define vdata 1 mquad_histogram_contents'')')
         WRITE (LUWAVE, '('' data list'')')
         WRITE (LUWAVE, '(G15.8)') (Q (L3H + L), L = 1, NBTOT)
      ELSE IF (HTHERE .AND. FTHERE) THEN
         WRITE (LUWAVE, '(/)')
         WRITE (LUWAVE, '(''define vdata 2 mquad_histogram_contents'',
     +   '' mquad_function_value'')')
         WRITE (LUWAVE, '('' data list'')')
         DO 30    IZ = 1, NNZ
            Z = ZMI + (IZ - 0.5) * DDZ
            DO 20    IY = 1, NNY
               Y = YMI + (IY - 0.5) * DDY
               DO 10    IX = 1, NNX
                  X = XMI + (IX - 0.5) * DDX
                  L = (IZ - 1) * NNX * NNY + (IY - 1) * NNX + IX
                  WRITE (LUWAVE, '(2G15.8)') Q (L3H + L), HQF (V)
   10          CONTINUE
   20       CONTINUE
   30    CONTINUE
      ELSE IF (.NOT. HTHERE .AND. FTHERE) THEN
         WRITE (LUWAVE, '(/)')
         WRITE (LUWAVE, '(''define vdata 1 mquad_function_value'')')
         WRITE (LUWAVE, '('' data list'')')
         DO 60    IZ = 1, NNZ
            Z = ZMI + (IZ - 0.5) * DDZ
            DO 50    IY = 1, NNY
               Y = YMI + (IY - 0.5) * DDY
               DO 40    IX = 1, NNX
                  X = XMI + (IX - 0.5) * DDX
                  L = (IZ - 1) * NNX * NNY + (IY - 1) * NNX + IX
                  WRITE (LUWAVE, '(G15.8)') HQF (V)
   40          CONTINUE
   50       CONTINUE
   60    CONTINUE
      END IF
      NLINES = NLINES + 4
 
* Write AVS _hist.fld file.
      IF (HTHERE) THEN
         WRITE (LUAVSH, '(''# AVS field file.'')')
         WRITE (LUAVSH, '(''# Identifier '', A)') CHID1 (1: LCHID) //
     +   ' (histogram)'
         WRITE (LUAVSH, '(/''######################################'')')
         WRITE (LUAVSH, '(''#   (Note: you may have to edit the .wave '
     +   //'filename below.)'')')
         WRITE (LUAVSH, '(''######################################''/)')
         WRITE (LUAVSH, '(''# Tags '', A)') CHTITL (1: LCHTIT)
         WRITE (LUAVSH, '(''# No. of multiquadric parameters'', I6)')
     +   NSIG
         WRITE (LUAVSH, '(''# Chi-squared'', G12.5, '' for'', I6,'
     +   //''' degrees of freedom.'')') CHISQ, NBTOT - NSIG
         WRITE (LUAVSH, '(''# Log likelihood'', G12.5)') ALOGLI
 
         WRITE (LUAVSH, '(/)')
         WRITE (LUAVSH, '(''ndim = 3'')')
         WRITE (LUAVSH, '(''dim1 = '', I3)') NNX
         WRITE (LUAVSH, '(''dim2 = '', I3)') NNX
         WRITE (LUAVSH, '(''dim3 = '', I3)') NNX
         WRITE (LUAVSH, '(''nspace = 3'')')
         WRITE (LUAVSH, '(''veclen = 1'')')
         WRITE (LUAVSH, '(''data = float'')')
         WRITE (LUAVSH, '(''field = uniform'')')
         WRITE (LUAVSH, '(''min_ext = '', 3F15.7)') XMI + DX / 2.,
     +   YMI + DY / 2., ZMI + DZ / 2.
         WRITE (LUAVSH, '(''max_ext = '', 3F15.7)') XMA - DX / 2.,
     +   YMA - DY / 2., ZMA - DZ / 2.
         WRITE (LUAVSH, '(''label = histogram'')')
         IF (FTHERE) THEN
            WRITE (LUAVSH, '(''variable 1 file='', A, ''.wave '
     +      //'filetype=ascii skip='', I5, '' stride=2'')')
     +      CHID1 (1: LCHID), NLINES
         ELSE
            WRITE (LUAVSH, '(''variable 1 file='', A, ''.wave '
     +      //'filetype=ascii skip='', I5)') CHID1 (1: LCHID), NLINES
         END IF
      END IF
 
* Write AVS _func.fld file.
      IF (FTHERE) THEN
         WRITE (LUAVSF, '(''# AVS field file.'')')
         WRITE (LUAVSF, '(''# Identifier '', A)') CHID1 (1: LCHID) //
     +   ' (function)'
         WRITE (LUAVSF, '(/''######################################'')')
         WRITE (LUAVSF, '(''#   (Note: you may have to edit the .wave '
     +   //'filename below.)'')')
         WRITE (LUAVSF, '(''######################################''/)')
         WRITE (LUAVSF, '(''# Tags '', A)') CHTITL (1: LCHTIT)
         WRITE (LUAVSF, '(''# No. of multiquadric parameters'', I6)')
     +   NSIG
         WRITE (LUAVSF, '(''# Chi-squared'', G12.5, '' for'', I6,'
     +   //''' degrees of freedom.'')') CHISQ, NBTOT - NSIG
         WRITE (LUAVSF, '(''# Log likelihood'', G12.5)') ALOGLI
 
         WRITE (LUAVSF, '(/)')
         WRITE (LUAVSF, '(''ndim = 3'')')
         WRITE (LUAVSF, '(''dim1 = '', I3)') NNX
         WRITE (LUAVSF, '(''dim2 = '', I3)') NNX
         WRITE (LUAVSF, '(''dim3 = '', I3)') NNX
         WRITE (LUAVSF, '(''nspace = 3'')')
         WRITE (LUAVSF, '(''veclen = 1'')')
         WRITE (LUAVSF, '(''data = float'')')
         WRITE (LUAVSF, '(''field = uniform'')')
         WRITE (LUAVSF, '(''min_ext = '', 3F15.7)') XMI + DX / 2.,
     +   YMI + DY / 2., ZMI + DZ / 2.
         WRITE (LUAVSF, '(''max_ext = '', 3F15.7)') XMA - DX / 2.,
     +   YMA - DY / 2., ZMA - DZ / 2.
         IF (HTHERE) THEN
            WRITE (LUAVSF, '(''variable 1 file='', A, ''.wave '
     +      //'filetype=ascii skip='', I5, '' offset=1 stride=2'')')
     +      CHID1 (1: LCHID), NLINES
         ELSE
            WRITE (LUAVSF, '(''variable 1 file='', A, ''.wave '
     +      //'filetype=ascii skip='', I5)') CHID1 (1: LCHID), NLINES
         END IF
      END IF
 
 
      GO TO 100
 
   70 CONTINUE
      WRITE (CHQMES, '(''Wrong dimensions ('', I2,
     +'') - only 3-D programmed.'')') NDIM
      GO TO 90
 
   80 CONTINUE
      CHQMES = 'Neither histogram nor function exist.'
      GO TO 90
 
   90 CONTINUE
      CALL HBUG (CHQMES, 'HQWAVE', IDMQ)
 
  100 CONTINUE
 
      END
