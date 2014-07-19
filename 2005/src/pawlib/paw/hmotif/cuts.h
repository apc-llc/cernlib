/*
 * $Id: cuts.h,v 1.1.1.1 1996/03/01 11:39:12 mclareni Exp $
 *
 * $Log: cuts.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:12  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 14/03/94  01.08.10  by  Fons Rademakers*/
/*-- Author :*/
/***********************************************************************
 *                                                                     *
 * cuts.h: declarations and definitions for the cut editor.            *
 *                                                                     *
 ***********************************************************************/
#ifndef _cuts_h
#define _cuts_h

#define CONTEXT_MACRO_ACCESS 1
#include "cuteditor.h"
#include "cutexpr.h"
#include "dynamiccedialog.h"
#undef CONTEXT_MACRO_ACCESS


#define MAX_CUT_EDITORS      1

#define INDENTATION          15

enum cut_t { WEIGHT, CUT, OPEN_BRACKET, CLOSE_BRACKET };


/* type definitions */
typedef struct _CutEditorStruct CutEditorStruct;
typedef struct _Cut Cut;
struct _Cut {
   Widget            widget;               /* cut widget itself */
   Widget            dynamicDialog;        /* dynamic cut dialog */
   char              cut[2*MAX_STRING];    /* cut string */
   char              not[6];               /* NOT operator */
   char              lvalue[MAX_STRING];   /* lvalue in cut string */
   char              operator[3];          /* cut operator */
   char              rvalue[MAX_STRING];   /* rvalue in cut string */
   char              loperator[6];         /* logical operator with next cut */
   int               id;                   /* cut identifier */
   enum cut_t        type;                 /* type of cut widget */
   int               dyn_cut_percent;      /* dynamic cut percentage */
   Boolean           selected;             /* true if cut is selected */
   Boolean           active;               /* true if cut is active */
   CutEditorStruct  *editor;               /* editor to which cut belongs */
   Cut              *link;                 /* link to next cut */
};

struct _CutEditorStruct {
   Widget  widget;                         /* cut editor widget itself */
   Widget  openDialog;                     /* open cuts file dialog */
   Widget  saveAsDialog;                   /* save cuts as dialog */
   Widget  indentDialog;                   /* indentation dialog */
   char    title[MAX_STRING];              /* name of cut editor title */
   char    file[MAX_STRING];               /* cuts file currently in editor */
   int     indentation;                    /* cut indentation */
   int     ncuts;                          /* number of cuts */
   int     last_id;                        /* id of last cut */
   Cut    *cuts;                           /* first cut in the list of cuts */
   Cut    *last_cut;                       /* last cut in list of cuts */
};


#endif  /* _cuts_h */
