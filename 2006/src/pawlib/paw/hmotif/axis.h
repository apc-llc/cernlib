/*
 * $Id: axis.h,v 1.1.1.1 1996/03/01 11:39:11 mclareni Exp $
 *
 * $Log: axis.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:11  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.34.17  by  Fons Rademakers*/
/*-- Author :*/
/***********************************************************************
 *                                                                     *
 * graphics.h: declarations and definitions for axis                   *
 *                                                                     *
 ***********************************************************************/
#ifndef _axis_h
#define _axis_h


#define MAX_NUMBER  10
#define MAX_P       20
#define MAX_S       10
#define MAX_T        5


/* type definitions */
typedef struct _axis {
   Boolean    x_grid;      /* vertical grid (not)active */
   Boolean    y_grid;      /* horizontal grid (not)active */
   Boolean    x_log;       /* logarithmic x axis */
   Boolean    y_log;       /* logarithmic y axis*/
   Boolean    z_log;       /* logarithmic z axis*/
   Boolean    xop;         /* x axis division optimization */
   Boolean    yop;         /* y axis division optimization */
   Boolean    zop;         /* z axis division optimization */
   Boolean    op;          /* default value of optimization */
   int        dnp;         /* default value for the primary axis divisions */
   int        dns;         /* default value for the secondary axis divisions */
   int        dnt;         /* default value for the tertiary axis divisions */
   int        xnp;         /* number of primary x axis divisions */
   int        xns;         /* number of secondary x axis divisions */
   int        xnt;         /* number of tertiary x axis divisions */
   int        ynp;         /* number of primary y axis divisions */
   int        yns;         /* number of secondary y axis divisions */
   int        ynt;         /* number of tertiary y axis divisions */
   int        znp;         /* number of primary z axis divisions */
   int        zns;         /* number of secondary z axis divisions */
   int        znt;         /* number of tertiary z axis divisions */
   int        xnl;         /* number of the label list for the x axis */
   int        ynl;         /* number of the label list for the y axis */
   int        xo;          /* type of x axis label orientation */
   int        yo;          /* type of y axis label orientation */
} AxisStruct;

/* global data */
extern AxisStruct axis;


#endif  /* _axis_h */
