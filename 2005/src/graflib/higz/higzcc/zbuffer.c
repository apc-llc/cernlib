/*
 * $Id: zbuffer.c,v 1.4 1998/09/25 09:23:59 mclareni Exp $
 *
 * $Log: zbuffer.c,v $
 * Revision 1.4  1998/09/25 09:23:59  mclareni
 * Modifications for the Mklinux port flagged by CERNLIB_PPC
 *
 * Revision 1.3  1997/03/14 14:22:29  mclareni
 * WNT mods
 *
 * Revision 1.2.2.1  1997/01/21 11:25:23  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.2  1996/03/08 14:56:19  cernlib
 * Correct ippfst and put_string macro for vms. There was infinite recursion.
 *
 * Revision 1.1.1.1  1996/02/14 13:10:27  mclareni
 * Higz
 *
 */
#include "higz/pilot.h"
#if defined(CERNLIB_ZBUF)
/*CMZ :  2.07/20 12/12/95  10.19.50  by  O.Couet*/
/*-- Author : O.Couet*/
/***********************************************************************
 *                                                                     *
 * Name: Z-buffer                                    Date:    15.08.95 *
 *                                                   Revised:          *
 * Author: Evgeni Chernyaev (chernaev@mx.ihep.su)                      *
 *                                                                     *
 * Contents (C interface):                                             *
 *     Zbuf_Init       - initialize Z-buffer                           *
 *     Zbuf_Clear      - clear screen                                  *
 *     Zbuf_Clip       - set cliping window                            *
 *     Zbuf_World      - set world min-max scope                       *
 *     Zbuf_Pixel      - draw point                                    *
 *     Zbuf_Line       - draw line                                     *
 *     Zbuf_Fill       - fill area                                     *
 *     Zbuf_Spectr     - set spectrum for Gouraud shading              *
 *     Zbuf_Shade      - fill area using Gouraud shading               *
 *     Zbuf_XFlush     - flush Z-image to X Windows window (HIGZ)      *
 *     Zbuf_Print      - print Z-image on terminal screen (for debug)  *
 *     Zbuf_PostScript - output Z-image to PostScript                  *
 *                                                                     *
 * (FORTRAN interface):                                                *
 *     IREP=IHZINI(NX,NY,NZ)                                           *
 *     CALL IHZCLE(ICOL)                                               *
 *     CALL IHZCLI(XMIN,YMIN,XMAX,YMAX)                                *
 *     CALL IHZWRD(XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX)                      *
 *     CALL IHZPNT(ICOL,X,Y,Z)                                         *
 *     CALL IHZLIN(ICOL,X1,Y1,Z1,X2,Y2,Z2)                             *
 *     CALL IHZFIL(ICOL,N,X,Y,Z)                                       *
 *     CALL IHZSPE(NL,FMIN,FMAX,ICOL)                                  *
 *     CALL IHZSHA(N,X,Y,Z,F)                                          *
 *     CALL IHZTOX(ITRAN,X0,Y0)                                        *
 *     CALL IHZPRN()                                                   *
 *     CALL IHZPS(IFGRAY,IFROTATE,XMIN,YMIN,XMAX,YMAX,NCOL,R,G,B)      *
 *                                                                     *
 ***********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __STDC__
#define ARGS(alist) alist
#else
#define ARGS(alist) ()
#endif

typedef unsigned char byte;

/*   F O R T R A N   I N T E R F A C E  -  M A I N   R O U T I N E S*/

#if defined(CERNLIB_QX_SC)
#define ihzini ihzini_
#define ihzcle ihzcle_
#define ihzcli ihzcli_
#define ihzwrd ihzwrd_
#define ihzpix ihzpix_
#define ihzlin ihzlin_
#define ihzfil ihzfil_
#define ihzspe ihzspe_
#define ihzsha ihzsha_
#define ihztox ihztox_
#define ihzprn ihzprn_
#define ihzps  ihzps_
#define ippfst ippfst_
#endif
#if defined(CERNLIB_QXCAPT)
#define ihzini IHZINI
#define ihzcle IHZCLE
#define ihzcli IHZCLI
#define ihzwrd IHZWRD
#define ihzpix IHZPIX
#define ihzlin IHZLIN
#define ihzfil IHZFIL
#define ihzspe IHZSPE
#define ihzsha IHZSHA
#define ihztox IHZTOX
#define ihzprn IHZPRN
#define ihzps  IHZPS
#define ippfst IPPFST
#endif
#if defined(CERNLIB_IBM)
#pragma linkage (ihzini ,FORTRAN)
#pragma linkage (ihzcle ,FORTRAN)
#pragma linkage (ihzcli ,FORTRAN)
#pragma linkage (ihzwrd ,FORTRAN)
#pragma linkage (ihzpix ,FORTRAN)
#pragma linkage (ihzlin ,FORTRAN)
#pragma linkage (ihzfil ,FORTRAN)
#pragma linkage (ihzspe ,FORTRAN)
#pragma linkage (ihzsha ,FORTRAN)
#pragma linkage (ihztox ,FORTRAN)
#pragma linkage (ihzprn ,FORTRAN)
#define linkage (ihzps  ,FORTRAN)
#define linkage (ippfst ,FORTRAN)
#endif
 

/*   U S E F U L   M A C R O S*/

#define SWAP(A,B,C) C = A; A = B; B = C
#define MIN(A,B)    (A < B ? A : B)
#define MAX(A,B)    (A > B ? A : B)

/*  G L O B A L   V A R I A B L E S   A N D   D E F I N I T I O N S*/

#if defined(CERNLIB_VAX)||defined(CERNLIB_DECS)||defined(CERNLIB_MSDOS)||defined(CERNLIB_WINNT)||(defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC))
#define Z_OFFSET 1       /* offset for short* on int (machine depended) */
#define C_OFFSET 2       /* offset for byte* on int (machine depended) */
#endif
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_DECS))&&(!defined(CERNLIB_MSDOS))&&(!defined(CERNLIB_WINNT))&&(!(defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC)))
#define Z_OFFSET 0       /* offset for short* on int (machine depended) */
#define C_OFFSET 1       /* offset for byte* on int (machine depended) */
#endif
#define MAXSIZE  4096    /* max size of z-screen */
#define LMAX     50      /* max number of edges in polygon */

struct EDGE {
  short x;               /* X of 1st node      & current X        */
  short y;               /* Y of 1st node                         */
  int   z;               /* Z of 1st node      & current Z        */
  int   c;               /* Colour of 1st node & current Colour   */
  short xx;              /* X of 2nd node      & X increment      */
  short yy;              /* Y of 2nd node                         */
  int   zz;              /* Z of 2nd node      & Z increment      */
  int   cc;              /* Colour of 2nd node & Colour increment */
  short test;            /* current parameter                     */
  short dxsign;          /* sign of X increment                   */
};

static int       Zstat = 0;               /* status of z-buffer */

static short     *Zbuf;                   /* z-buffer */
static byte      *Zimg;                   /* image */

static float     XWmin, YWmin, ZWmin,
                 XWmax, YWmax, ZWmax;     /* world min-max scope */
static long      Nalloc=0;                /* size of allocated memory */
static short     Nx, Ny, Nz;              /* z-image size */
static short     Xmin, Ymin, Xmax, Ymax;  /* cliping rectangle */
static int       IXmin, IYmin,
                 IXmax, IYmax;            /* area occupated by image */
static int       IBack=-1;                /* background colour */
static float     Fmin, Fmax;              /* min, max lightness */
static int       Nlevel, FstCol;          /* number of levels, 1st colour */

static struct EDGE edge[LMAX], *jedge[LMAX];
static int       Nedge, xscan[LMAX], zscan[LMAX], cscan[LMAX];

/***********************************************************************
 *                                                                     *
 *                E X T E R N A L   R O U T I N E S                    *
 *                                                                     *
 ***********************************************************************/

void ixput_image ARGS((int,int,int,int,int,int,int,int,int,int,byte*));

#if defined(CERNLIB_VAX)
#include <descrip.h>
#define ippfst_vms(s1) \
        do { extern void ippfst(struct dsc$descriptor_s*); \
             struct dsc$descriptor_s s1_dsc; \
             s1_dsc.dsc$b_dtype = DSC$K_DTYPE_T; \
             s1_dsc.dsc$b_class = DSC$K_CLASS_S; \
             s1_dsc.dsc$a_pointer = s1; \
             s1_dsc.dsc$w_length = strlen(s1); \
             ippfst(&s1_dsc); } while(0)
#endif
#if !defined(CERNLIB_VAX)
#define zippfst(s1) \
        ippfst(s1,strlen(s1))
#endif

#ifdef CERNLIB_WINNT
/***********************************************************************
 *             Dummy routine for Windows NT			       *
 ***********************************************************************/
void ixput_image(offset,itran,x0,y0,nx,ny,xmin,ymin,xmax,ymax,image)
             int offset,itran,x0,y0,nx,ny,xmin,ymin,xmax,ymax;
   unsigned char *image;
{
}
#endif

/***********************************************************************
 *                                                                     *
 *                I N T E R N A L   R O U T I N E S                    *
 *                                                                     *
 ***********************************************************************
 *                                                                     *
 * Name: Zbuf_WtoS                                   Date:    15.08.95 *
 * Author: E.Chernyaev (IHEP/Protvino)               Revised:          *
 *                                                                     *
 * Function: Transform coordinates of point from world to screen       *
 *                                                                     *
 * Input:  XW, YW, ZW - world coodinates of point                      *
 * Output: XS, YS, ZS - screen coordinates of point                    *
 *                                                                     *
 ***********************************************************************/
static void Zbuf_WtoS(XW, YW, ZW, XS, YS, ZS)
               float  XW, YW, ZW;
                 int *XS,*YS,*ZS;
{
  int x, y;

  *XS   = ((XW - XWmin)/(XWmax-XWmin)) * Nx;
  *YS   = ((YW - YWmin)/(YWmax-YWmin)) * Ny;
  *ZS   = ((ZW - ZWmin)/(ZWmax-ZWmin)) * Nz * 65536;
  IXmin = MIN(IXmin,*XS); IXmax = MAX(IXmax,*XS);
  IYmin = MIN(IYmin,*YS); IYmax = MAX(IYmax,*YS);
}

/***********************************************************************
 *                                                                     *
 * Name: Zbuf_FtoC                                   Date:    15.08.95 *
 * Author: E.Chernyaev (IHEP/Protvino)               Revised:          *
 *                                                                     *
 * Function: Translate function value to colour index                  *
 *                                                                     *
 * Input: F - function value                                           *
 *                                                                     *
 * Output: ICOL - extended colour index                                *
 *                                                                     *
 ***********************************************************************/
static void Zbuf_FtoC(F, ICOL)
                float F;
                 int *ICOL;
{
  *ICOL = (((F - Fmin)/(Fmax - Fmin))*Nlevel + FstCol) * 65536;
}

/***********************************************************************
 *                                                                     *
 * Name: Zbuf_GetEdges                               Date:    15.08.95 *
 * Author: E.Chernyaev (IHEP/Protvino)               Revised:          *
 *                                                                     *
 * Function: Get edges of the polygon                                  *
 *                                                                     *
 * Input: N              - number of edges                             *
 *        X(*),Y(*),Z(*) - coordinates of nodes                        *
 *        F1(*)          - function values at nodes                    *
 *        ifval          - flag: 1 - copy function values              *
 *                               0 - do not copy                       *
 *                                                                     *
 ***********************************************************************/
static void Zbuf_GetEdges(N, X, Y, Z, F, ifval)
                      int N, ifval;
                   float *X,*Y,*Z,*F;
{
  int i, n;
  int i1, x1, y1, z1, icol1;
  int i2, x2, y2, z2, icol2;
  byte *jcol,*jcol1,*jcol2;

  Nedge = 0;

  /*   C H E C K   N U M B E R   O F   E D G E S   */

  n = N;
  if (n < 0 || n > LMAX) {
    fprintf(stderr,"Zbuf_GetEdges: wrong n. of edges=%d (max=%d)\n",n,LMAX);
    return;
  }

  /*   S K I P   H O R I Z O N T A L   E D G E S   */
  /*   S E T   R I G H T   O R I E N T A T I O N   O F   E D G E S  */

  n = icol1 = icol2 = 0;
  for (i=0; i < N; i++) {
    i1 = i; i2 = i+1; if (i2 == N) i2 = 0;
    Zbuf_WtoS(X[i1], Y[i1], Z[i1], &x1, &y1, &z1);
    if (ifval) Zbuf_FtoC(F[i1], &icol1);
    Zbuf_WtoS(X[i2], Y[i2], Z[i2], &x2, &y2, &z2);
    if (ifval) Zbuf_FtoC(F[i2], &icol2);
    if (y1 == y2)               continue;
    if (y1 < Ymin && y2 < Ymin) continue;
    if (y1 > Ymax && y2 > Ymax) continue;
    if (y2 > y1) {
      edge[n].x  = x1; edge[n].y  = y1; edge[n].z  = z1; edge[n].c  = icol1;
      edge[n].xx = x2; edge[n].yy = y2; edge[n].zz = z2; edge[n].cc = icol2;
    }else{
      edge[n].x  = x2; edge[n].y  = y2; edge[n].z  = z2; edge[n].c  = icol2;
      edge[n].xx = x1; edge[n].yy = y1; edge[n].zz = z1; edge[n].cc = icol1;
    }
    n++;
  }
  Nedge = n;
}

/***********************************************************************
 *                                                                     *
 * Name: Zbuf_MinMax                                 Date:    15.08.95 *
 * Author: E.Chernyaev (IHEP/Protvino)               Revised:          *
 *                                                                     *
 * Function: Find surrounding box (min-max scope)                      *
 *                                                                     *
 * Output: XMIN,XMAX,YMIN,YMAX - min-max scope                         *
 *                                                                     *
 ***********************************************************************/
static void Zbuf_MinMax(XMIN, XMAX, YMIN, YMAX)
                   int *XMIN,*XMAX,*YMIN,*YMAX;
{
  int       i, xmin, xmax, ymin, ymax;

  xmin = xmax = edge[1].x; ymin = ymax = edge[1].y;
  for (i=0; i<Nedge; i++) {
    if (xmin > edge[i].x)  xmin = edge[i].x;
    if (xmax < edge[i].x)  xmax = edge[i].x;
    if (xmin > edge[i].xx) xmin = edge[i].xx;
    if (xmax < edge[i].xx) xmax = edge[i].xx;
    if (ymin > edge[i].y)  ymin = edge[i].y;
    if (ymax < edge[i].yy) ymax = edge[i].yy;
  }
  *XMIN = xmin; *XMAX = xmax; *YMIN = ymin; *YMAX = ymax;
}

/***********************************************************************
 *                                                                     *
 * Name: Zbuf_SortEdges                              Date:    15.08.95 *
 * Author: E.Chernyaev (IHEP/Protvino)               Revised:          *
 *                                                                     *
 * Function: Sort edges along Y                                        *
 *                                                                     *
 ***********************************************************************/
static void Zbuf_SortEdges()
{
  int         i, k;
  struct EDGE *jcur;

  for (i=0; i<Nedge;   i++) { jedge[i] = &edge[i]; }
  for (i=0; i<Nedge-1; i++) {
    if (jedge[i+1]->y >= jedge[i]->y) continue;
    for (k=i+1; k>0; k--) {
      if (jedge[k]->y >= jedge[k-1]->y) break;
      SWAP(jedge[k],jedge[k-1],jcur);
    }
  }
}

/***********************************************************************
 *                                                                     *
 * Name: Zbuf_InitEdges                              Date:    15.08.95 *
 * Author: E.Chernyaev (IHEP/Protvino)               Revised:          *
 *                                                                     *
 * Function: Initialize edges for polygon filling                      *
 *                                                                     *
 ***********************************************************************/
static void Zbuf_InitEdges()
{
  int i, dx, dy;

  for (i=0; i<Nedge; i++) {
    dx           = edge[i].xx - edge[i].x;
    dy           = edge[i].yy - edge[i].y;
    if (dx < 0) { edge[i].dxsign = -1; dx = -dx;} else { edge[i].dxsign = 1;}
    edge[i].xx = dx;
    if (dx <= dy) {
      edge[i].test  = -(dy+1)/2;
      edge[i].zz    = (edge[i].zz - edge[i].z) / dy;
      edge[i].cc    = (edge[i].cc - edge[i].c) / dy;
    }else{
      edge[i].test  = -dx;
      edge[i].zz    = (edge[i].zz - edge[i].z) / dx;
      edge[i].cc    = (edge[i].cc - edge[i].c) / dx;
    }
  }
}

/***********************************************************************
 *                                                                     *
 *     M A I N   R O U T I N E S   -   C   I N T E R F A C E           *
 *                                                                     *
 ***********************************************************************
 *                                                                     *
 * Name: Zbuf_Init                                   Date:    15.08.95 *
 * Author: E.Chernyaev (IHEP/Protvino)               Revised:          *
 *                                                                     *
 * Function: Initialize Z-buffer                                       *
 *                                                                     *
 * Input: NX,NY,NZ - dimensions of Z-buffer                            *
 *                                                                     *
 * Return: 0 - O.K.                                                    *
 *         1 - error during initialization                             *
 *                                                                     *
 ***********************************************************************/
int Zbuf_Init(NX, NY, NZ)
         int  NX, NY, NZ;
{
  Zstat = 0;

  /*   C H E C K   P A R A M E T E R S  */

  if (NX <= 0 || NX > MAXSIZE ||
      NY <= 0 || NY > MAXSIZE ||
      NZ <= 0 || NZ > MAXSIZE ) {
    fprintf(stderr,"Zbuf_Init: wrong z-buffer dimensions: %d %d %d\n",
          NX,NY,NZ);
    return 1;
  }

  Nx = NX; Ny = NY; Nz = NZ;
  Xmin = 0;  Ymin = 0; Xmax = Nx - 1; Ymax = Ny - 1;
  IXmin = Xmax+1; IYmin = Ymax+1; IXmax = Xmin-1; IYmax = Ymin-1;

  /*   A L L O C A T E   Z - B U F F E R   */

  if ((long)Nx * (long)Ny > Nalloc) {

    if (Nalloc != 0) {
      free((char *) Zbuf);
      free((char *) Zimg);
    }

    Zbuf = (short *) malloc (Nx*Ny*sizeof(short));
    if (!Zbuf) {
      fprintf(stderr,"Zbuf_Init: can not allocate z-buffer\n");
      return 1;
    }

    Zimg = (byte *) malloc (Nx*Ny*sizeof(byte));
    if (!Zimg) {
      fprintf(stderr,"Zbuf_Init: can not allocate image\n");
      return 1;
    }

    Nalloc = (long)Nx * (long)Ny;
  }

  Zstat = 1;
  return 0;
}

/***********************************************************************
 *                                                                     *
 * Name: Zbuf_Clear                                  Date:    15.08.95 *
 * Author: E.Chernyaev (IHEP/Protvino)               Revised:          *
 *                                                                     *
 * Function: Clear screen                                              *
 *                                                                     *
 * Input: ICOL - background colour                                     *
 *                                                                     *
 ***********************************************************************/
void Zbuf_Clear(ICOL)
            int ICOL;
{
  int y,nx;

  if (!Zstat) return;
  if (IBack != ICOL) {
    memset ((char *)Zbuf, (char)0, Nx*Ny*sizeof(short));
    memset ((char *)Zimg, (char)(ICOL & 0xff), Nx*Ny);
    IBack = ICOL;
  }else{
    IXmin = MAX(IXmin,Xmin); IYmin = MAX(IYmin,Ymin);
    IXmax = MIN(IXmax,Xmax); IYmax = MIN(IYmax,Ymax);
    nx    = IXmax - IXmin + 1;
    for (y=IYmin; y<=IYmax; y++) {
      memset ( (char *)(Zbuf+y*Nx+IXmin), (char)0, nx*sizeof(short));
      memset ( (char *)(Zimg+y*Nx+IXmin), (char)(ICOL & 0xff), nx);
    }
  }
  IXmin = Xmax+1; IYmin = Ymax+1; IXmax = Xmin-1; IYmax = Ymin-1;
}

/***********************************************************************
 *                                                                     *
 * Name: Zbuf_Clip                                   Date:    15.08.95 *
 * Author: E.Chernyaev (IHEP/Protvino)               Revised:          *
 *                                                                     *
 * Function: Set cliping window                                        *
 *                                                                     *
 * Input: XMIN, YMIN, XMAX, YMAX - clipping rectangle                  *
 *                                                                     *
 ***********************************************************************/
void Zbuf_Clip(XMIN, YMIN, XMAX, YMAX)
           int XMIN, YMIN, XMAX, YMAX;
{
  if (!Zstat) return;
  Xmin = MAX(XMIN,0); Xmax = MIN(XMAX,Nx-1);
  Ymin = MAX(YMIN,0); Ymax = MIN(YMAX,Ny-1);
}

/***********************************************************************
 *                                                                     *
 * Name: Zbuf_World                                  Date:    15.08.95 *
 * Author: E.Chernyaev (IHEP/Protvino)               Revised:          *
 *                                                                     *
 * Function: Set world min-max scope                                   *
 *                                                                     *
 * Input: XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX - world min-max scope     *
 *                                                                     *
 ***********************************************************************/
void Zbuf_World(XMIN, YMIN, ZMIN, XMAX, YMAX, ZMAX)
          float XMIN, YMIN, ZMIN, XMAX, YMAX, ZMAX;
{
  float d;

  XWmin = XMIN; YWmin = YMIN; ZWmin = ZMIN;
  XWmax = XMAX; YWmax = YMAX; ZWmax = ZMAX;
  if (XWmin >= XWmax || YWmin >= YWmax || ZWmin >= ZWmax) {
    fprintf(stderr,"Zbuf_World: error in min-max scope\n");
  }
  d = 0.5 * (XWmax-XWmin) / (Nx-1); XWmin -= d; XWmax += d;
  d = 0.5 * (YWmax-YWmin) / (Ny-1); YWmin -= d; YWmax += d;
  d = 0.5 * (ZWmax-ZWmin) / (Nz-1); ZWmin -= d; ZWmax += d;
}

/***********************************************************************
 *                                                                     *
 * Name: Zbuf_Point                                  Date:    15.08.95 *
 * Author: E.Chernyaev (IHEP/Protvino)               Revised:          *
 *                                                                     *
 * Function: Draw point (Z-buffer)                                     *
 *                                                                     *
 * Input: ICOL  - colour                                               *
 *        X,Y,Z - coordinates of the point                             *
 *                                                                     *
 ***********************************************************************/
void Zbuf_Point(ICOL, X, Y, Z)
            int ICOL;
          float X, Y, Z;
{
  int     x, y, z, k;
  byte    *jimg;
  short   *jbuf, zz;

  /*   C H E C K   P A R A M E T E R S   */

  if (!Zstat) return;
  Zbuf_WtoS(X, Y, Z, &x, &y, &z);
  if (x < Xmin || x > Xmax || y < Ymin || y > Ymax) return;

  /*   D R A W   P I X E L   */

  k    = y * Nx + x;
  jimg = Zimg + k;
  jbuf = Zbuf + k;
  zz   = z / 65536;
  if (*jbuf <= zz) { *jbuf = zz; *jimg = ICOL & 0xff; }
}

/***********************************************************************
 *                                                                     *
 * Name: Zbuf_Line                                   Date:    15.08.95 *
 * Author: E.Chernyaev (IHEP/Protvino)               Revised:          *
 *                                                                     *
 * Function: Draw line (Z-buffer)                                      *
 *                                                                     *
 * Input: ICOL     - colour                                            *
 *        X1,Y1,Z1 - 1st point of the line                             *
 *        X2,Y2,Z2 - 2nd point of the line                             *
 *                                                                     *
 ***********************************************************************/
void Zbuf_Line(ICOL, X1, Y1, Z1, X2, Y2, Z2)
           int ICOL;
         float X1, Y1, Z1, X2, Y2, Z2;
{
  int     x1, y1, z1, x2, y2, z2;
  int     dx, dy, dz, dmax, dxdx, dydy, dxsign;
  int     t, xcur, ycur, zcur, xlim1, xlim2;
  byte    *jimg, icol;
  short   *jbuf, *jend, *jzcur ;

  if (!Zstat) return;

  /*   T R A N S F O R M   C O O R D I N A T E S  */

  icol = ICOL & 0xff;
  if (Y2 > Y1) {
    Zbuf_WtoS(X1, Y1, Z1, &x1, &y1, &z1);
    Zbuf_WtoS(X2, Y2, Z2, &x2, &y2, &z2);
  }else{
    Zbuf_WtoS(X1, Y1, Z1, &x2, &y2, &z2);
    Zbuf_WtoS(X2, Y2, Z2, &x1, &y1, &z1);
  }

  /*   S E T   S O M E   P R E D E F I N I T I O N S   */

  dxsign = x1 > x2 ? -1 : 1;
  dx     = x1 > x2 ? x1 - x2 : x2 - x1;
  dxdx   = dx + dx;
  dy     = y2 - y1;
  dydy   = dy + dy;
  dmax   = MAX(dx,dy);
  t      = -dmax;
  if (dmax > 0) dz = (z2 - z1) / dmax;

  zcur   = z1+65536;
  jzcur  = (short *) &zcur + Z_OFFSET;

  jimg   = Zimg + y1*Nx + x1;
  jbuf   = Zbuf + y1*Nx + x1;
  jend   = Zbuf + y2*Nx + x2;

  /*   I S   L I N E   F U L L Y   I N S I D E   W I N D O W  ?  */

  if (y1 >= Ymin && y2 <= Ymax &&
      x1 >= Xmin && x1 <= Xmax &&
      x2 >= Xmin && x2 <= Xmax) {

    /*   D R A W   P O I N T   */
    if ( dmax == 0) {
      if (z1 < z2) zcur = z2+65536;
      if (*jbuf <= *jzcur) { *jbuf = *jzcur; *jimg = icol; }
      return;
    }

    /*   D R A W   L I N E   */

    if (dx >= dy) {
      while (jbuf != jend) {
      if (*jbuf <= *jzcur) { *jbuf = *jzcur; *jimg  = icol; }
      if ((t += dydy) >= 0) { t -= dxdx; jbuf += Nx; jimg += Nx; }
        jbuf += dxsign; jimg += dxsign; zcur += dz;
      }
    }else{
      while (jbuf != jend) {
      if (*jbuf <= *jzcur) { *jbuf = *jzcur; *jimg = icol; }
      if ((t += dxdx) >= 0) { t -= dydy; jbuf += dxsign; jimg += dxsign; }
        jbuf += Nx; jimg += Nx; zcur += dz;
      }
    }
    if (*jbuf <= *jzcur) { *jbuf = *jzcur; *jimg = icol; }

  }else{

    if (y1 > Ymax || y2 < Ymin) return;
    if (x1 < Xmin && x2 < Xmin) return;
    if (x1 > Xmax && x2 > Xmax) return;
    xcur = dxsign*x1;
    ycur = y1;
    if (x2 >= x1) {
      xlim1 = Xmin; xlim2 = Xmax;
    }else{
      xlim1 = -Xmax; xlim2 = -Xmin;
    }

    /*   D R A W   C L I P P E D   L I N E   */

    if (dx >= dy) {
      while (jbuf != jend) {
      if (ycur > Ymax || xcur > xlim2) return;
        if (ycur >= Ymin && xcur >= xlim1)
        if (*jbuf <= *jzcur) { *jbuf = *jzcur; *jimg = icol; }
      if ((t += dydy) >= 0)
        { t -= dxdx; jbuf += Nx; jimg += Nx; ycur++; }
      jbuf += dxsign; jimg += dxsign; xcur++; zcur += dz;
      }
    }else{
      while (jbuf != jend) {
        if (ycur > Ymax || xcur > xlim2) return;
        if (ycur >= Ymin && xcur >= xlim1)
        if (*jbuf <= *jzcur) { *jbuf = *jzcur; *jimg = icol; }
      if ((t += dxdx) >= 0)
        { t -= dydy; jbuf += dxsign; jimg += dxsign; xcur++; }
        jbuf += Nx; jimg += Nx; ycur++; zcur += dz;
      }
    }
    if (ycur > Ymax || xcur > xlim2) return;
    if (ycur < Ymin || xcur < xlim1) return;
    if (*jbuf <= *jzcur) { *jbuf = *jzcur; *jimg = icol; }
  }
}

/***********************************************************************
 *                                                                     *
 * Name: Zbuf_Fill                                   Date:    15.08.95 *
 * Author: E.Chernyaev (IHEP/Protvino)               Revised:          *
 *                                                                     *
 * Function: Fill area (Z-buffer)                                      *
 *                                                                     *
 * Input: ICOL           - colour                                      *
 *        N              - number of edges                             *
 *        X(*),Y(*),Z(*) - coordinates of nodes                        *
 *                                                                     *
 ***********************************************************************/
void Zbuf_Fill(ICOL, N, X, Y, Z)
          int  ICOL, N;
        float *X,*Y,*Z;
{
  int     i, k, step, istart, nx, xcur, ycur, zcur, dz;
  int     xmin, xmax, ymin, ymax;
  byte    icol,*jimg,*jimg_base;
  short   dx, dy, *jzcur,*jbuf,*jend,*jbuf_base;
  struct EDGE *jcur;

  if (!Zstat) return;

  /*   P R E P A R A T O R Y   S T U F F  */

  Zbuf_GetEdges (N, X, Y, Z, (float *) NULL, 0);
  if (Nedge == 0 ) return;

  Zbuf_MinMax (&xmin, &xmax, &ymin, &ymax);
  if (xmin > Xmax || xmax < Xmin) return;
  if (ymax > Ymax) ymax = Ymax;

  Zbuf_SortEdges();
  Zbuf_InitEdges();

  /*   F I L L  P O L Y G O N :  L O O P   A L O N G   S C A N L I N E S   */

  icol   = ICOL & 0xff;
  jzcur  = (short *) &zcur + Z_OFFSET;
  istart = 0;
  for (ycur=ymin; ycur<=ymax; ycur++) {
    nx = 0;
    /*   P R E P A R E   X   I N T E R S E C T I O N S   */
    for (i=istart; i<Nedge; i++) {
      jcur = jedge[i];
      if (jcur->y  >  ycur) break;
      if (jcur->yy <= ycur) {
        if (i == istart) istart++;
      }else{
        xscan[nx] = jcur->x; zscan[nx] = jcur->z; nx++;
        dx = jcur->xx;
      dy = jcur->yy - jcur->y;
      if (dx <= dy ) {
        jcur->z += jcur->zz;
          if ((jcur->test += dx) >= 0) {
          jcur->test -= dy;
          jcur->x    += jcur->dxsign;
        }
      }else{
          step = dx/dy;
        if ((jcur->test += step*dy) >= 0) {
          jcur->test -= dx;
        }else{
          jcur->test += dy - dx; step++;
        }
        jcur->x += jcur->dxsign * step;
        jcur->z += jcur->zz * step;
      }
      }
    }
    if (ycur < Ymin) continue;

    /*   S O R T   I N T E R S E C T I O N   P O I N T S   A L O N G   X   */
    for (i=0; i<nx-1; i++) {
      if (xscan[i+1] >= xscan[i]) continue;
      for (k=i+1; k>0; k--) {
      if (xscan[k] >= xscan[k-1]) break;
        SWAP(xscan[k],xscan[k-1],xcur);
        SWAP(zscan[k],zscan[k-1],zcur);
      }
    }

    /*   F I L L   S P A N S   */
    jimg_base = Zimg + ycur*Nx;
    jbuf_base = Zbuf + ycur*Nx;
    for (i=0; i<nx; i+=2) {
      if ((xmin = xscan[i])   > Xmax) break;
      if ((xmax = xscan[i+1]) < Xmin) continue;
      if (xmin == xmax) {
        jimg = jimg_base + xmin;
        jbuf = jbuf_base + xmin;
        zcur = MAX(zscan[i],zscan[i+1]);
      if (*jbuf <= *jzcur) { *jbuf = *jzcur; *jimg = icol; }
      }else{
        zcur = zscan[i];
      dz   = (zscan[i+1] - zcur) / (xmax - xmin);
        if (xmin < Xmin) { zcur += dz * (Xmin-xmin); xmin = Xmin; }
      if (xmax > Xmax) xmax = Xmax;
        jimg = jimg_base + xmin;
        jbuf = jbuf_base + xmin;
      jend = jbuf_base + xmax;
      while (jbuf != jend) {
        if (*jbuf <= *jzcur) { *jbuf = *jzcur; *jimg = icol; }
        jbuf++; jimg++; zcur += dz;
      }
      if (*jbuf <= *jzcur) { *jbuf = *jzcur; *jimg = icol; }
      }
    }
  }
}

/***********************************************************************
 *                                                                     *
 * Name: Zbuf_Spectr                                 Date:    15.08.95 *
 * Author: E.Chernyaev (IHEP/Protvino)               Revised:          *
 *                                                                     *
 * Function: Set colour spectrum for Gouraud shading                   *
 *                                                                     *
 * Input: NL   - number of levels                                      *
 *        FMIN - min lightness                                         *
 *        FMAX - max lightness                                         *
 *        ICOL - colour index of 1st level                             *
 *                                                                     *
 ***********************************************************************/
void Zbuf_Spectr(NL, FMIN, FMAX, ICOL)
             int NL, ICOL;
           float FMIN, FMAX;
{
  Nlevel = NL;
  Fmin   = FMIN;
  Fmax   = FMAX + (FMAX-FMIN)*0.001;
  FstCol = ICOL;
}

/***********************************************************************
 *                                                                     *
 * Name: Zbuf_Shade                                  Date:    15.08.95 *
 * Author: E.Chernyaev (IHEP/Protvino)               Revised:          *
 *                                                                     *
 * Function: Gouraud shading (Z-buffer)                                *
 *                                                                     *
 * Input: N              - number of edges                             *
 *        X(*),Y(*),Z(*) - coordinates of nodes                        *
 *        F(*)           - function values (lightness) at nodes        *
 *                                                                     *
 ***********************************************************************/
void Zbuf_Shade(N, X, Y, Z, F)
            int N;
         float *X,*Y,*Z,*F;
{
  int     i, k, step, istart, nx, ccur, xcur, ycur, zcur, dc, dz;
  int     xmin, xmax, ymin, ymax;
  byte    *jccur,*jimg,*jimg_base;
  short   dx, dy, *jzcur,*jbuf, *jend,*jbuf_base;
  struct EDGE *jcur;

  if (!Zstat) return;

  /*   P R E P A R A T O R Y   S T U F F  */

  Zbuf_GetEdges (N, X, Y, Z, F, 1);
  if (Nedge == 0 ) return;

  Zbuf_MinMax (&xmin, &xmax, &ymin, &ymax);
  if (xmin > Xmax || xmax < Xmin) return;
  if (ymax > Ymax) ymax = Ymax;

  Zbuf_SortEdges();
  Zbuf_InitEdges();

  /*   F I L L  P O L Y G O N :  L O O P   A L O N G   S C A N L I N E S   */

  jccur  = (byte *)  &ccur + C_OFFSET;
  jzcur  = (short *) &zcur + Z_OFFSET;
  istart = 0;
  for (ycur=ymin; ycur<=ymax; ycur++) {
    nx = 0;
    /*   P R E P A R E   X   I N T E R S E C T I O N S   */
    for (i=istart; i<Nedge; i++) {
      jcur = jedge[i];
      if (jcur->y  >  ycur) break;
      if (jcur->yy <= ycur) {
        if (i == istart) istart++;
      }else{
        xscan[nx] = jcur->x; zscan[nx] = jcur->z; cscan[nx] = jcur->c; nx++;
        dx = jcur->xx;
      dy = jcur->yy - jcur->y;
      if (dx <= dy ) {
        jcur->z += jcur->zz;
        jcur->c += jcur->cc;
          if ((jcur->test += dx) >= 0) {
          jcur->test -= dy;
          jcur->x    += jcur->dxsign;
        }
      }else{
          step = dx/dy;
        if ((jcur->test += step*dy) >= 0) {
          jcur->test -= dx;
        }else{
          jcur->test += dy - dx; step++;
        }
        jcur->x += jcur->dxsign * step;
        jcur->z += jcur->zz * step;
        jcur->c += jcur->cc * step;
      }
      }
    }
    if (ycur < Ymin) continue;

    /*   S O R T   I N T E R S E C T I O N   P O I N T S   A L O N G   X   */
    for (i=0; i<nx-1; i++) {
      if (xscan[i+1] >= xscan[i]) continue;
      for (k=i+1; k>0; k--) {
      if (xscan[k] >= xscan[k-1]) break;
        SWAP(xscan[k],xscan[k-1],xcur);
        SWAP(zscan[k],zscan[k-1],zcur);
        SWAP(cscan[k],cscan[k-1],ccur);
      }
    }

    /*   F I L L   S P A N S   */
    jimg_base = Zimg + ycur*Nx;
    jbuf_base = Zbuf + ycur*Nx;
    for (i=0; i<nx; i+=2) {
      if ((xmin = xscan[i])   > Xmax) break;
      if ((xmax = xscan[i+1]) < Xmin) continue;
      if (xmin == xmax) {
        jimg = jimg_base + xmin;
        jbuf = jbuf_base + xmin;
        if (zscan[i] > zscan[i+1]) {
        zcur = zscan[i]; ccur = cscan[i];
      }else{
        zcur = zscan[i+1]; ccur = cscan[i+1];
      }
      if (*jbuf <= *jzcur) { *jbuf = *jzcur; *jimg = *jccur; }
      }else{
        zcur = zscan[i];
      dz   = (zscan[i+1] - zcur) / (xmax - xmin);
        ccur = cscan[i];
      dc   = (cscan[i+1] - ccur) / (xmax - xmin);
        if (xmin < Xmin) {
        zcur += dz * (Xmin-xmin); ccur += dc * (Xmin-xmin); xmin = Xmin;
      }
      if (xmax > Xmax) xmax = Xmax;
        jimg = jimg_base + xmin;
        jbuf = jbuf_base + xmin;
      jend = jbuf_base + xmax;
      while (jbuf != jend) {
        if (*jbuf <= *jzcur) { *jbuf = *jzcur; *jimg = *jccur; }
        jbuf++; jimg++; zcur += dz; ccur += dc;
      }
      if (*jbuf <= *jzcur) { *jbuf = *jzcur; *jimg = *jccur; }
      }
    }
  }
}

/***********************************************************************
 *                                                                     *
 * Name: Zbuf_XFlush                                 Date:    15.08.95 *
 * Author: E.Chernyaev (IHEP/Protvino)               Revised:          *
 *                                                                     *
 * Function: Flush Z-image to X Windows screen                         *
 *                                                                     *
 * Input: ITRAN  - index of transparent colour, if necessary, or -1    *
 *        X0, Y0 - coordinates of upper-left corner of the image       *
 *                                                                     *
 ***********************************************************************/
void Zbuf_XFlush(ITRAN, X0, Y0)
             int ITRAN, X0, Y0;
{
  IXmin = MAX(IXmin,Xmin); IYmin = MAX(IYmin,Ymin);
  IXmax = MIN(IXmax,Xmax); IYmax = MIN(IYmax,Ymax);
  ixput_image(0,ITRAN,X0,Y0,Nx,Ny,IXmin,IYmin,IXmax,IYmax,Zimg);
}

/***********************************************************************
 *                                                                     *
 * Name: Zbuf_Print                                  Date:    15.08.95 *
 * Author: E.Chernyaev (IHEP/Protvino)               Revised:          *
 *                                                                     *
 * Function: Print contents of z-image to the screen                   *
 *                                                                     *
 ***********************************************************************/
void Zbuf_Print()
{
  int   ix, iy, z;
  short *jbuf;
  byte  *jimg, icol;

  printf("Z-Screen: %d x %d\n",Nx,Ny);

  if (!Zstat) {
    fprintf(stderr,"Zbuf_Print: z-buffer is not initialized\n");
    return;
  }

  for (iy=0; iy<Ny; iy++) {
    jbuf = Zbuf + Nx*(Ny-iy-1);
    jimg = Zimg + Nx*(Ny-iy-1);
    for (ix=0; ix<Nx; ix++) {
      icol = *jimg++;
      z    = *jbuf++;
/*
  if (icol == 0) { printf(" ."); } else { printf("%2d",icol); }
*/
      if (icol == 0) { printf(" ."); } else { printf("%2d",z); }
    }
    printf("\n");
  }
}

/***********************************************************************
 *                                                                     *
 *             O U T P U T   T O   P O S T S C R I P T                 *
 *                                                                     *
 * Output LZW compressed Z-buffer image to PostScript.                 *
 * The code is based on PS_LZWencode.c routine from xwpick-2.21        *
 *                                                                     *
 ***********************************************************************/

static char      s[80];

#if !defined(CERNLIB_VAX)
#define put_string   zippfst(&s[0])
#else
#define put_string   ippfst_vms(&s[0])
#endif

/***********************************************************************
 *                                                                     *
 * Name: ASCII85encode                               Date:    05.11.93 *
 * Author: E.Chernyaev (IHEP/Protvino)               Revised: 20.04.95 *
 *                                                                     *
 * Function: ASCII85 encode and output byte buffer                     *
 *                                                                     *
 * Input: k   - number of bytes                                        *
 *        Buf - byte buffer                                            *
 *                                                                     *
 ***********************************************************************/
static void ASCII85encode(k, Buf)
                     int  k;
                     byte Buf[];
{
  unsigned long Value;
  int    i, j, n;
  char   *js;

  if (k == 0) return;
  js = s;
  for (j=0; j<k; j+=4) {
    Value = Buf[j]*256*256*256;
    if (j+1 < k) Value += Buf[j+1]*256*256;
    if (j+2 < k) Value += Buf[j+2]*256;
    if (j+3 < k) Value += Buf[j+3];
    if (j == 0) {
      if ( (char) (Value / (85*85*85*85) + 33) == '%') *js++ = ' ';
    }
    for (n=4; n>=0; n--) {
      *(js+n) = Value % 85 + 33;
      Value /= 85;
    }
    if (k-j < 4) {
     js += k-j+1;
    }else{
     js += 5;
    }
  }
  *js = '\0'; put_string;
}

#define BITS           12                       /* largest code size */
#define HSIZE          5003                     /* hash table size */
#define SHIFT          4                        /* shift for hashing */
#define CLEARCODE      256                      /* Clear Code */
#define EOD            257                      /* End Of Data code */
#define PIXS           170000                   /* largest # of pixels */

/***********************************************************************
 *                                                                     *
 * Name: PutCode                                     Date:    05.11.93 *
 * Author: E.Chernyaev (IHEP/Protvino)               Revised:          *
 *                                                                     *
 * Function: Put out code (LZW encoding)                               *
 *                                                                     *
 * Input: Code - code                                                  *
 *        CodeSize - codesize                                          *
 *                                                                     *
 ***********************************************************************/
static void PutCode(Code, CodeSize)
                int Code, CodeSize;
{
  static int k, PartA, PartB, SizeA, SizeB;
  static int mask[] = { 0x0000, 0x0001, 0x0003, 0x0007, 0x000F,
                                0x001F, 0x003F, 0x007F, 0x00FF,
                                0x01FF, 0x03FF, 0x07FF, 0x0FFF };
  static byte Accum[56];
  if (Code == -1) {
    k      = 0;
    PartA  = 0;
    SizeA  = 0;
    return;
  }

  PartB = Code;
  SizeB = CodeSize;

  while (SizeB >= 8) {
    SizeB  = SizeA + SizeB - 8;
    Accum[k++] = PartA | (PartB >> SizeB);
    if (k == 56) {
      ASCII85encode(k,Accum);
      k      = 0;
    }
    PartB &= mask[SizeB];
    SizeA  = 0;
    PartA  = 0;
  }

  SizeA = SizeB;
  PartA = PartB << (8-SizeB);
  if (Code == EOD) {
    if (SizeA != 0) Accum[k++] = PartA;
    ASCII85encode(k, Accum);
  }
}

/***********************************************************************
 *                                                                     *
 * Name: EncodeData                                  Date:    27.11.95 *
 * Author: E.Chernyaev (IHEP/Protvino)               Revised:          *
 *                                                                     *
 * Function: Lelpel-Ziv Welch encoding of an image                     *
 *                                                                     *
 * Input: Ncol - number of colours                                     *
 *        R[]  - red components                                        *
 *        G[]  - green components                                      *
 *        B[]  - blue components                                      *
 *                                                                     *
 ***********************************************************************/
static void EncodeData(Ncol, R, G, B)
                  int  Ncol;
             float *R,*G,*B;
{
  int    i, k, x, y, disp, Code, K, r, g, b;
  long   CodeK, Npix;
  char   **q,*js;
  int    FreeCode, CurCodeSize, CurMaxCode;
  byte   *ScLine;

  long   HashTab [HSIZE];                       /* hash table */
  int    CodeTab [HSIZE];                       /* code table */

  /*   O U T P U T   P A L E T T E   */

  js = s;
  for (k=0; k<Ncol; k++) {
    r =  R[k] * 256.; r = MAX(r,0); r = MIN(r,255);
    g =  G[k] * 256.; g = MAX(g,0); g = MIN(g,255);
    b =  B[k] * 256.; b = MAX(b,0); b = MIN(b,255);
    sprintf(js,"%02x%02x%02x",r,g,b);
    js += 6;
    if (k % 10 == 9) {
      put_string; js = s;
    }else{
      sprintf(js++," ");
    }
  }
  for (k=Ncol; k<256; k++) {
    sprintf(js,"000000");
    js += 6;
    if (k % 10 == 9) {
      put_string; js = s;
    }else{
      sprintf(js++," ");
    }
  }
  put_string;

  /*   L W Z   C O M P R E S S I O N   */

  PutCode(-1, 0);
  FreeCode    = CLEARCODE + 2;
  CurCodeSize = 9;
  CurMaxCode  = 511;
  memset((char *) HashTab, -1, sizeof(HashTab));
  PutCode(CLEARCODE, CurCodeSize);              /* 1st - clear code */
  Npix = 0;
  for (y=0; y<Ny; y++) {
    ScLine = Zimg + (Ny-1-y)*Nx;
    x     = 0;
    if (y == 0) Code  = ScLine[x++];
    while(x < Nx) {
      K     = ScLine[x++];                      /* next symbol */
      Npix++;
      CodeK = ((long) K << BITS) + Code;        /* set full code */
      k     = (K << SHIFT) ^ Code;              /* xor hashing */

      if (HashTab[k] == CodeK) {                /* full code found */
        Code = CodeTab[k];
        continue;
      }
      else if (HashTab[k] < 0 )                 /* empty slot */
        goto NOMATCH;

      disp  = HSIZE - k;                        /* secondary hash */
      if (k == 0) disp = 1;

PROBE:
      if ((k -= disp) < 0)
        k  += HSIZE;

      if (HashTab[k] == CodeK) {                /* full code found */
        Code = CodeTab[k];
        continue;
      }

      if (HashTab[k] > 0)                       /* try again */
        goto PROBE;

NOMATCH:                                        /* full code not found */
      PutCode(Code, CurCodeSize);
      Code = K;
      if (FreeCode == CurMaxCode) {
        CurCodeSize++;
        CurMaxCode = CurMaxCode*2 + 1;
      }

      if (CurCodeSize <= BITS && Npix <= PIXS) {
        CodeTab[k] = FreeCode++;                /* code -> hashtable */
        HashTab[k] = CodeK;
      }else{
        if (CurCodeSize > BITS) CurCodeSize = BITS;
        PutCode(CLEARCODE, CurCodeSize);
        memset((char *) HashTab, -1, sizeof(HashTab));
        FreeCode    = CLEARCODE + 2;
        CurCodeSize = 9;
        CurMaxCode  = 511;
        Npix = 0;
      }
    }
  }
   /*   O U T P U T   T H E   R E S T  */

  PutCode(Code, CurCodeSize);
  if (FreeCode == CurMaxCode && CurCodeSize != BITS)
    CurCodeSize++;
  PutCode(EOD, CurCodeSize);
  sprintf(s,"~>");                              put_string;
}

/***********************************************************************
 *                                                                     *
 * Name: Zbuf_PostScript                             Date:    26.11.95 *
 * Author: E.Chernyaev (IHEP/Protvino)               Revised:          *
 *                                                                     *
 * Function: Output LZW compressed Z-buffer image to PostScript        *
 *                                                                     *
 * Input: IFGRAY   - gray flag: set it when grayscale image is needed  *
 *        IFROTATE - rotation flag: set it for landscape mode          *
 *        XMIN,YMIN,XMAX,YMAX - image dimensions on page (in PS points)*
 *        NCOL     - number of colours                                 *
 *        R[]      - red components                                    *
 *        G[]      - green components                                  *
 *        B[]      - blue components                                   *
 *                                                                     *
 ***********************************************************************/
void Zbuf_PostScript(IFGRAY,IFROTATE,XMIN,YMIN,XMAX,YMAX,NCOL,R,G,B)
                 int IFGRAY,IFROTATE,XMIN,YMIN,XMAX,YMAX,NCOL;
               float *R,*G,*B;
{
  char       **q;

  static char *HeaderEPS[] = {
    "%***********************************************************************",
    "%*                                                                     *",
    "%* Function: Display a Lempel-Ziv Welch (LZW) encoded color image.     *",
    "%*           This is an Encapsulated PostScript file suitable for any  *",
    "%            kind of PostScript printer: Level I/II, color/grayscale...*",
    "%*                                                                     *",
    "%* Author: Evgeni CHERNYAEV (chernaev@vxcern.cern.ch)                  *",
    "%*                                                                     *",
    "%***********************************************************************",
    "gsave",
    "%***********************************************************************",
    "%*                              Image Description                      *",
    "%***********************************************************************",
    NULL,
  };

  static char *CommonPart[] = {
    "/IfColor systemdict /colorimage known {true} {false} ifelse def",
    "GRAYneeded {/IfColor false def} if",
    "/IfLevel2 systemdict /filter known systemdict /setcolorspace known and",
    "  {true} {false} ifelse def",
    "%***********************************************************************",
    "%                               Procedures                             *",
    "%***********************************************************************",
    "userdict /Table known not userdict /StrArray known not or",
    "{/Table 4096 array def /StrArray 10 array def} if",
    "/InLine 71 string def",
    "/BinCodes 112 string def",
    "/CurBit 0 def /FreeCode 258 def /CurCodeSize 9 def /CurMask 511 def",
    "/incr IfColor {3} {1} ifelse def",
    "IfLevel2 not {/setcolorspace null def /filter null def} if",
    "%***********************************************************************",
    "/ReadLine {                     % Read Next Data Line (ASCII85 decode) *",
    "%***********************************************************************",
    "  BinCodes 0 BinCodes 56 56 getinterval putinterval",
    "  currentfile InLine readline pop pop",
    "  InLine 0 get 255 and 32 eq {/kk 1 def} {/kk 0 def} ifelse",
    "  1 1 14 {",
    "    /i exch 1 sub def",
    "    /k i 5 mul kk add def",
    "    /c1 InLine k get 33 sub 255 and def",
    "    /rest c1 1868977 mul",
    "      InLine k 1 add get 33 sub 255 and 614125 mul add",
    "      InLine k 2 add get 33 sub 255 and 7225 mul add",
    "      InLine k 3 add get 33 sub 255 and 85 mul add",
    "      InLine k 4 add get 33 sub 255 and add def",
    "    /k i 4 mul 56 add def",
    "    BinCodes k c1 3 mul rest 16777216 idiv add 255 and put",
    "    /rest rest 16777216 mod def",
    "    BinCodes k 1 add rest 65536 idiv put",
    "    BinCodes k 2 add rest 256 idiv 255 and put",
    "    BinCodes k 3 add rest 255 and put",
    "  } for",
    "} bind def",
    "%***********************************************************************",
    "/ReadCode {                     % Read next code                       *",
    "%***********************************************************************",
    "  /CurByte CurBit 8 idiv def",
    "  /CurCode",
    "    BinCodes CurByte get 8 bitshift",
    "    BinCodes CurByte 1 add get add 8 bitshift",
    "    BinCodes CurByte 2 add get add",
    "    CurCodeSize CurBit 7 and add 24 sub bitshift CurMask and def",
    "  /CurBit CurBit CurCodeSize add dup 448 ge {ReadLine 448 sub} if def",
    "} bind def",
    "%***********************************************************************",
    "/DecodeCode {                   % Decode CurCode                       *",
    "%***********************************************************************",
    "  ReadCode CurCode 256 eq {",
    "    /FreeCode 258 def /CurCodeSize 9 def /CurMask 511 def",
    "    /StrInd -1 def /Lrest 0 def ReadCode",
    "  }{",
    "    L Lrest gt {",
    "      /StrInd StrInd 1 add def /Lrest 65535 def",
    "      StrArray StrInd get null eq {StrArray StrInd 65535 string put} if",
    "      /CurStr StrArray StrInd get def",
    "    } if",
    "    Table FreeCode CurStr 65535 Lrest sub L getinterval",
    "    dup 0 Table OldCode get putinterval",
    "    dup L incr sub Table",
    "      CurCode FreeCode lt {CurCode} {OldCode} ifelse get",
    "    0 incr getinterval putinterval put",
    "    /Lrest Lrest L sub def /FreeCode FreeCode 1 add def",
    "    FreeCode CurMask ge CurCodeSize 12 lt and {",
    "      /CurCodeSize CurCodeSize 1 add def",
    "      /CurMask CurMask 1 bitshift 1 add def",
    "    } if",
    "  } ifelse",
    "  /OldCode CurCode def",
    "  Table CurCode get dup length incr add /L exch def",
    "} bind def",
    "%***********************************************************************",
    "/DisplayImage {          % Display a LZW-encoded color image           *",
    "%***********************************************************************",
    "  /DelX Xmax Xmin sub def",
    "  /DelY Ymax Ymin sub def",
    "  Xmin Ymin translate",
    "  DelX DelY scale",
    "  /Palette currentfile 768 string readhexstring pop def",
    "  currentfile InLine readline pop pop",
    "  IfColor not {",
    "    0 1 255 {",
    "      Palette exch dup /i exch 3 mul def",
    "      Palette i 0 add get 0.299 mul",
    "      Palette i 1 add get 0.587 mul add",
    "      Palette i 2 add get 0.114 mul add cvi put",
    "    } for",
    "    /Palette Palette 0 256 getinterval def",
    "  } if",
    "  /Matr IfRotate",
    "    {[0 colomns rows 0 0 0]} {[colomns 0 0 rows neg 0 rows]} ifelse def",
    "  IfLevel2 {",
    "    [/Indexed IfColor {/DeviceRGB} {/DeviceGray} ifelse 255 Palette]",
    "    setcolorspace",
    "    /infile currentfile /ASCII85Decode filter /LZWDecode filter def",
    "    8 dict",
    "    dup /ImageType 1 put",
    "    dup /Width colomns put",
    "    dup /Height rows put",
    "    dup /BitsPerComponent 8 put",
    "    dup /ImageMatrix Matr put",
    "    dup /Interpolate false put",
    "    dup /Decode [0 255] put",
    "    dup /DataSource infile put image",
    "  }{",
    "    0 1 255 {",
    "      Table exch dup incr mul Palette exch incr getinterval put",
    "    } for",
    "    ReadLine ReadLine",
    "    colomns rows 8 Matr {DecodeCode}",
    "    IfColor {false 3 colorimage} {image} ifelse",
    "  } ifelse",
    "} bind def",
    "%***********************************************************************",
    "%*                              Image decoding                         *",
    "%***********************************************************************",
    "DisplayImage",
    NULL,
  };

  /*   C H E C K   P A R A M E T E R S   */

  if (!Zstat) return;
  if (NCOL <= 0 || NCOL > 256) {
    fprintf(stderr,"Zbuf_PostScript: wrong number of colors: %d\n",NCOL);
    return;
  }

  /*   O U T P U T   H E A D E R   */

  for (q=HeaderEPS;*q; q++) {
    sprintf(s,"%s",*q);                                 put_string;
  }

  /*  I M A G E   D E S C R I P T I O N   */

  sprintf(s,"/colomns %d def /rows %d def",Nx,Ny);      put_string;

  if (IFGRAY) {
    sprintf(s,"/GRAYneeded true def");                  put_string;
  }else{
    sprintf(s,"/GRAYneeded false def");                 put_string;
  }
  if (IFROTATE) {
    sprintf(s,"/IfRotate true def");                    put_string;
  }else{
    sprintf(s,"/IfRotate false def");                   put_string;
  }
  sprintf(s,"/Xmin %d def /Ymin %d def /Xmax %d def /Ymax %d def",
             XMIN,YMIN,XMAX,YMAX);                      put_string;

  /*   O U T P U T   P O S T S C R I P T   C O D E  */

  for (q=CommonPart; *q; q++) {
    sprintf(s,"%s",*q);                                 put_string;
  }

  /*   O U T P U T   E N C O D E D   D A T A   */

  EncodeData(NCOL, R, G, B);

  sprintf(s,"grestore");                                put_string;
}

/***********************************************************************
 *                                                                     *
 *  M A I N   R O U T I N E S   -   F O R T R A N   I N T E R F A C E  *
 *                                                                     *
 ***********************************************************************
 *                                                                     *
 * Name: FORTRAN interface routines                  Date:    15.11.95 *
 * Author: E.Chernyaev (IHEP/Protvino)               Revised:          *
 *                                                                     *
 * Contents: IREP=IHZINI(NX,NY,NZ)                                     *
 *           CALL IHZCLE(ICOL)                                         *
 *           CALL IHZCLI(XMIN,YMIN,XMAX,YMAX)                          *
 *           CALL IHZWRD(XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX)                *
 *           CALL IHZPNT(ICOL,X,Y,Z)                                   *
 *           CALL IHZLIN(ICOL,X1,Y1,Z1,X2,Y2,Z2)                       *
 *           CALL IHZFIL(ICOL,N,X,Y,Z)                                 *
 *           CALL IHZSPE(NL,FMIN,FMAX,ICOL)                            *
 *           CALL IHZSHA(N,X,Y,Z,F)                                    *
 *           CALL IHZTOX(X0,Y0)                                        *
 *           CALL IHZPRN()                                             *
 *           CALL IHZPS(IFGRAY,IFROTATE,XMIN,YMIN,XMAX,YMAX,NCOL,R,G,B)*
 *                                                                     *
 ***********************************************************************/
int type_of_call ihzini(NX, NY, NZ)
      int *NX,*NY,*NZ;
{ return Zbuf_Init(*NX,*NY,*NZ); }

void  type_of_call ihzcle(ICOL)
       int *ICOL;
{ Zbuf_Clear(*ICOL); }

void  type_of_call ihzcli(XMIN, YMIN, XMAX, YMAX)
       int *XMIN,*YMIN,*XMAX,*YMAX;
{ Zbuf_Clip(*XMIN,*YMIN,*XMAX,*YMAX);}

void  type_of_call ihzwrd(XMIN, YMIN, ZMIN, XMAX, YMAX, ZMAX)
     float *XMIN,*YMIN,*ZMIN,*XMAX,*YMAX,*ZMAX;
{ Zbuf_World(*XMIN,*YMIN,*ZMIN,*XMAX,*YMAX,*ZMAX); }

void  type_of_call ihzpnt(ICOL, X, Y, Z)
       int *ICOL;
     float *X,*Y,*Z;
{ Zbuf_Point(*ICOL,*X,*Y,*Z); }

void  type_of_call ihzlin(ICOL, X1, Y1, Z1, X2, Y2, Z2)
       int *ICOL;
     float *X1,*Y1,*Z1,*X2,*Y2,*Z2;
{ Zbuf_Line(*ICOL,*X1,*Y1,*Z1,*X2,*Y2,*Z2); }

void  type_of_call ihzfil(ICOL, N, X, Y, Z)
       int *ICOL,*N;
     float *X,*Y,*Z;
{ Zbuf_Fill(*ICOL,*N, X, Y, Z); }

void  type_of_call ihzspe(NL, FMIN, FMAX, ICOL)
       int *NL,*ICOL;
     float *FMIN,*FMAX;
{ Zbuf_Spectr(*NL,*FMIN,*FMAX,*ICOL); }

void  type_of_call ihzsha(N, X, Y, Z, F)
       int *N;
     float *X,*Y,*Z,*F;
{ Zbuf_Shade(*N, X, Y, Z, F); }

void  type_of_call ihztox(IBACK, X0, Y0)
       int *IBACK,*X0,*Y0;
{ Zbuf_XFlush(*IBACK,*X0,*Y0); }

void ihzprn()
{ Zbuf_Print(); }

void  type_of_call ihzps(IFGRAY, IFROTATE, XMIN, YMIN, XMAX, YMAX, NCOL, R, G, B)
      int *IFGRAY,*IFROTATE,*XMIN,*YMIN,*XMAX,*YMAX,*NCOL;
    float *R,*G,*B;
{ Zbuf_PostScript(*IFGRAY,*IFROTATE,*XMIN,*YMIN,*XMAX,*YMAX,*NCOL, R, G, B); }

/*    E N D   O F   Z - B U F F E R*/
#endif
