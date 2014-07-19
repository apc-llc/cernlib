/*
 * $Id: style.h,v 1.1.1.1 1996/03/01 11:39:14 mclareni Exp $
 *
 * $Log: style.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:14  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.45.07  by  Fons Rademakers*/
/*-- Author :*/
/***********************************************************************
 *                                                                     *
 * style.h: declarations and definitions for the style structure.      *
 *                                                                     *
 ***********************************************************************/
#ifndef _style_h
#define _style_h

#include "axis.h"
#include "attstate.h"

/* type definitions */
typedef struct Style Style;
typedef struct Color Color;

struct Style {
   char     *Header;
   int       OFile;
   int       ODate;
   int       OFit;
   int       OStat;
   int       File;
   int       Date;
   char      Fit[4];
   char      Stat[8];
   int       Zone[2];
   float     Angle[2];
   char     *Label[9];
   int       Linx;
   int       Liny;
   int       Linz;
   int       Grid;
   char      Ndvx[MAX_NUMBER];
   char      Ndvy[MAX_NUMBER];
   char      Ndvz[MAX_NUMBER];
   float     Ymgu;
   float     Gsiz;
   float     Ygti;
   float     Xmgr;
   float     Xwin;
   float     Xmgl;
   float     Smgu;
   float     Smgr;
   float     Xlab;
   float     Ywin;
   float     Ysiz;
   float     Xsiz;
   float     Ytic;
   float     Xval;
   float     Vsiz;
   float     Ymgl;
   float     Xtic;
   float     Tsiz;
   float     Yhti;
   float     Ylab;
   float     Asiz;
   float     Baro;
   float     Barw;
   float     Csiz;
   float     Hmax;
   float     Errx;
   float     Yval;
   float     Hcol;
   float     Bcol;
   float     Pcol;
   float     Fcol;
   float     Xcol;
   float     Ycol;
   float     Mscf;
   int       Mtyp;
   int       Ltyp;
   int       Htyp;
   int       Btyp;
   int       Ptyp;
   int       Ftyp;
   int       Dmod;
   int       Ncol;
   int       Bwid;
   int       Hwid;
   int       Fwid;
   int       Pwid;
   int       Xwid;
   int       Ywid;
   char      Cfon[4];
   char      Tfon[4];
   char      Lfon[4];
   char      Vfon[4];
   char      Gfon[4];
   Color     *Col;
   char      Ptit[MAX_TITLE_SIZE];
   char      Htit[MAX_TITLE_SIZE];
   char      Atit[2*MAX_TITLE_SIZE];
   Style    *link;
};

struct Color {
   float red;
   float green;
   float bleu;
};



/* global data */
extern Style *style_root;
extern Style *current_style;

#endif  /* _style_h */
