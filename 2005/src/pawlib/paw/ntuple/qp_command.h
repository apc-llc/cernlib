/*
 *  qp_command.h  --
 *	Definitions for commands on resulting ntuple expressions
 *
 *  Original: 20-Apr-1995 15:42
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_command.h,v 1.19 1999/07/02 12:31:46 couet Exp $
 *
 *  $Log: qp_command.h,v $
 *  Revision 1.19  1999/07/02 12:31:46  couet
 *  - hplot_interface.h is now replaced by hplot.h in CVSCOSRC
 *  - bug fixed in qp_execute.c: the zones were not properly reset after a
 *    NT/SCAN with option SPIDER
 *
 *  Revision 1.18  1996/08/30 10:04:28  lecointe
 *  Restored Gouraud Shading in NT/PLOT
 *
 *  Revision 1.17  1996/08/21 12:55:34  lecointe
 *  Restore the spider plot in ntuple/scan
 *
 *  Revision 1.16  1996/04/23 18:38:10  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_QP_COMMAND
#define CERN_QP_COMMAND

#include	<stdio.h>


#include	"qp_query.h"
#include	"qp_value.h"
#include	"smap.h"


#define	HFILL_BSIZE	2048	/* buffer size for filling and plotting */
#define	PRINTFUN_SIZE	35	/* minimum buffer size for print functions */
#define	MAX_MAP_STRINGS	512	/* the max number of strings in a histo */


typedef		float (*CvtFun)( void *, void * );
typedef struct _cvt_call_back_ {
	CvtFun		f;
	void *		d;
} CvtCallBack;

typedef		void (*PrintFun)( String, void * );

/* does not seem needed at the moment */
#if 0
typedef struct _print_call_back_ {
	PrintFun		f;
	void *		d;
} PrintCallBack;
#endif



typedef		void (*TableCallBack)( int * );


typedef enum _cmd_type_ {
	CMD_NULL,
	CMD_MINMAX,
	CMD_MINMAXAVG,
	CMD_VFILL,
	CMD_HFILL1,
	CMD_HFILL2,
	CMD_SCAT2,
	CMD_SCAT3,
	CMD_SCAT4,
	CMD_GOURAUD,
	CMD_LOOP,
	CMD_DUMP,
	CMD_SCAN,
	CMD_SPIDERSCAN
} CmdType;


typedef struct _extreme_ {
	DataType	dtyp;
	Uval		min;
	Uval		max;
	SMap		sm;
} Extreme;

typedef struct _extremeavg_ {
	DataType	dtyp;
	Uval		min;
	Uval		max;
	double		avg;
	SMap		sm;
} ExtremeAvg;


#if 0
typedef struct _qp_cmd_null_ {
	/* input part */
	/* runtime part */
} QPCmdNull;
#endif


typedef struct _qp_cmd_minmax_ {
	/* input part */
	/* runtime part */
	bool		has_events;	/* atleast one event passed the cuts */
	int		n;	/* number of expressions to minmax */
	Extreme		*el;	/* list of Extreme struct for each expression */
} QPCmdMinMax;


typedef struct _qp_cmd_minmaxavg_ {
	/* input part */
	/* runtime part */
	bool		has_events;	/* atleast one event passed the cuts */
	int		n;	        /* number of expressions to minmax */
	int		evt_count;	/* Number of selected events */
	ExtremeAvg	*el;	/* list of Extreme struct for each expression */
} QPCmdMinMaxAvg;


#if 0
typedef struct _qp_cmd_vfill_ {
	/* input part */
	/* runtime part */
} QPCmdVFill;
#endif


typedef struct _qp_cmd_hfill1_ {
	/* input part */
	int		idh;	/* histogram id */
	/* runtime part */
	int		n;
	float		*x;
	float		*w;
	CvtCallBack	cvt_x;	/* function to convert x to float */
	int		step_x;
} QPCmdHFill1;


typedef struct _qp_cmd_hfill2_ {
	/* input part */
	int		idh;	/* histogram id */
	/* runtime part */
	CvtCallBack	cvt_x;	/* function to convert x to float */
	int		step_x;
	CvtCallBack	cvt_y;	/* function to convert y to float */
	int		step_y;
} QPCmdHFill2;


typedef struct _qp_cmd_scat2_ {
	/* input part */
	int		idh;	/* histogram id */
	bool		line;	/* connect points with lines */
	bool		logx;
	bool		logy;
	/* runtime part */
	int		n;
	int		*ievt;
	float		*x;
	float		*y;
	CvtCallBack	cvt_x;	/* function to convert x to float */
	int		step_x;
	CvtCallBack	cvt_y;	/* function to convert y to float */
	int		step_y;
} QPCmdScat2;


typedef struct _qp_cmd_scat3_ {
	/* input part */
	bool		line;	/* connect points with lines */
	bool		logx;	/* x-axis is logarithmic */
	bool		logy;	/* y-axis is logarithmic */
	bool		logz;	/* z-axis is logarithmic */
	/* runtime part */
	int		n;
	int		*ievt;
	float		*x;
	float		*y;
	float		*z;
	CvtCallBack	cvt_x;	/* function to convert x to float */
	int		step_x;
	CvtCallBack	cvt_y;	/* function to convert y to float */
	int		step_y;
	CvtCallBack	cvt_z;	/* function to convert z to float */
	int		step_z;
} QPCmdScat3;


typedef struct _qp_cmd_scat4_ {
	/* input part */
	bool		line;	/* connect points with lines */
	float		col_min;/* minimum value of the color attribute */
	float		col_max;/* maximum value of the color attribute */
	bool		logx;	/* x-axis is logarithmic */
	bool		logy;	/* y-axis is logarithmic */
	bool		logz;	/* z-axis is logarithmic */
	/* runtime part */
	int		n;	/* nummber of values in the buffers */
	int		*ievt;	/* buffers for the values to be plotted */
	float		*x;
	float		*y;
	float		*z;
	float		*c;
	CvtCallBack	cvt_x;	/* function to convert x to float */
	int		step_x;
	CvtCallBack	cvt_y;	/* function to convert y to float */
	int		step_y;
	CvtCallBack	cvt_z;	/* function to convert z to float */
	int		step_z;
	CvtCallBack	cvt_c;	/* function to convert c to float */
	int		step_c;
} QPCmdScat4;


typedef struct _qp_cmd_gouraud_ {
	/* input part */
	bool		logx;	/* x-axis is logarithmic */
	bool		logy;	/* y-axis is logarithmic */
	bool		logz;	/* z-axis is logarithmic */
	/* runtime part */
        float           min[3]; /* Minimum value for each selected elements */
        float           max[3]; /* Maximum value for each selected elements */
        float           s[3];   /* Isosurface values        */
        int		nx;     /* Nb of bin in X dimension */
	int		ny;     /* Nb of bin in Y dimension */
	int		nz;     /* Nb of bin in Z dimension */
	float		dx;     /* X interval               */
	float		dy;     /* Y interval               */
	float		dz;     /* Z interval               */
	float		*x;     /* X coord values           */
	float		*y;     /* Y coord values           */
	float		*z;     /* Z coord values           */
	float		*value; /* 3D Histogram values      */
	CvtCallBack	cvt_x;	/* function to convert x to float */
	int		step_x;
	CvtCallBack	cvt_y;	/* function to convert y to float */
	int		step_y;
	CvtCallBack	cvt_z;	/* function to convert z to float */
	int		step_z;
} QPCmdGouraud;

#if 0
typedef struct _qp_cmd_loop_ {
	/* input part */
	/* runtime part */
} QPCmdLoop;
#endif


typedef struct _qp_cmd_dump_ {
	/* input part */
	FILE *		fp;		/* Stream to dump on (stdout or file) */
	String		sep1;		/* value separator */
	String		sep2;		/* expression separator */
	/* runtime part */
	PrintFun	f[MAX_EXPRS];	/* format function for each expression*/
} QPCmdDump;


typedef struct _qp_cmd_scan_ {
	/* input part */
	bool		pawpp;		/* use Paw++ table widget for display */
	/* runtime part */
	TableCallBack	matlab;		/* Callback to set labels in paw++ */
	TableCallBack	matrow;		/* Callback to set values in paw++ */
	PrintFun	f[MAX_EXPRS];	/* format function for each expression*/
	int		fw[MAX_EXPRS];	/* field width, space for value */
	String		sep;		/* horizontal separator line */
	String		title;		/* title line */
	String		buf;		/* buffer for line */
	int		max_line;	/* maximum number of lines to print */
	int		cur_line;	/* number of lines already printed */
	bool		no_prompt;	/* no more questions your honor ! */
	bool		evt_count;	/* Number of selected events */
} QPCmdScan;

typedef struct _qp_cmd_spider_scan_ {
	/* input part */
	/* runtime part */
        CvtCallBack	cvt[MAX_EXPRS]; /* Conversion function for each expression*/
        float           min[MAX_EXPRS]; /* Minimum value for each selected elements */
        float           max[MAX_EXPRS]; /* Maximum value for each selected elements */
        float           avg[MAX_EXPRS]; /* Average value for each selected elements */
        int             t_code[MAX_EXPRS];/* Type code for each selected elements */
        float           saved[8];       /* Some graphic parameters to be changed and 
					   restored at the end */
	float		rzone;	        /* number of zones -> max(horizontally,vertically) */
        char            *expr_str;      /* concatenated names of the scanned elements */
	int		max_line;	/* maximum number of events to scan between prompts */
	int		cur_line;	/* number of events already scaned */
	bool		no_prompt;	/* no more questions your honor ! */
	int		evt_count;	/* Number of selected events */
	int		spider_type;	/* 1=contour 2=wedges*/
	bool		use_average;	/* if TRUE, draw the average spider*/
} QPCmdSpiderScan;


typedef struct _qp_cmd_ {
	CmdType		command;

	/* CmdType dependent part */
	union {
#if 0
		QPCmdNull		null;
#endif
		QPCmdMinMax		minmax;
		QPCmdMinMaxAvg		minmaxavg;
#if 0
		QPCmdVFill		vfill;
#endif
		QPCmdHFill1		hfill1;
		QPCmdHFill2		hfill2;
		QPCmdScat2		scat2;
		QPCmdScat3		scat3;
		QPCmdScat4		scat4;
		QPCmdGouraud		gouraud;
#if 0
		QPCmdLoop		loop;
#endif
		QPCmdDump		dump;
		QPCmdScan		scan;
		QPCmdSpiderScan		sp_scan;
	} u;

} QPCmd;


extern int const	datatype_width[];


QPCmd *
qpcmd_new( CmdType cmd );

void
qpcmd_free( QPCmd * cmd );

void
qpcmd_getminmax( QPCmd * cmd, int i, float * min, float * max );

void
qpcmd_getminmaxavg( QPCmd * cmd, int i, float * min, float * max , float * avg );

SMap
qpcmd_labels(  QPCmd * cmd, int i );


CvtCallBack
datatype_to_cvtcallback( DataType dtype, void * cbd );

PrintFun
datatype_to_printfun( DataType dtype );

PrintFun
datatype_to_dumpfun( DataType dtype );

int
datatype_to_typecode( DataType dtype);

#endif	/*	CERN_QP_COMMAND	*/

